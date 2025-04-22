# Workspace ----
setwd(dirname(.rs.api.getSourceEditorContext()$path))

library(tidyverse)

use_files <-
  list.files(
    path = "../../create-data/output/n-1000/",
    pattern = "(claim|cond|enc|med|obs|org|patients|payers|proc|hosp).*.csv$",
    full.names = TRUE,
    recursive = TRUE
  )

walk(
  .x = use_files,
  ~ assign(
    x = paste0(
      "base_",
      basename(.x) |> tools::file_path_sans_ext()
    ),
    value = read_csv(.x) |> rename_all(tolower),
    envir = globalenv()
  )
)


#   select
# en.id,
# min(dx.description) as diagnosis_name
# from
# main.encounters        en
# inner join main.claims cl on cl.patientid = en.patient and cl.servicedate::date = en.start::date and cl.providerid = en.provider
# inner join conditions  dx on dx.code = cl.diagnosis1
# group by 1

# Data ----
# * stg_encounter ----
stg_encounter <-
  base_encounters |>
  filter(
    code == "185347001",
    reasoncode == "91302008",
    start >= "2000-01-01"
  ) |>
  left_join(
    base_payers |>
      transmute(
        payer = id,
        payor_name = name
      )
  ) |>
  left_join(
    base_closest_hospital |>
      rename(
        organization = id
      )
  ) |>
  transmute(
    encounter_id = str_sub(id, 1, 8),
    patient_id = str_sub(patient, 1, 8),
    encounter_date = as.Date(start),
    hospital_admit_date = start,
    hospital_discharge_date = stop,
    los_days = round(interval(start, stop) / days(1), 1),
    event_type_id = code,
    event_type = description,
    reason_id = reasoncode,
    reason_for_visit =
      case_when(
        reason_id == 185347001 ~ "Sepsis (disorder)",
        .default = description
      ),
    hospital_name,
    payor_name,
    hospital_x,
    hospital_y
  ) |>
  arrange(hospital_admit_date)

# * stg_diagnosis_encounter ----
# stg_diagnosis_encounter <-
#   stg_encounter |>
#   filter(
#     !str_detect(encounter_id, "^(7bd17689)")
#   ) |>
#   #slice(86) |>
#   inner_join(
#     by = join_by(patient_id, encounter_date, provider_id),
#     y = {
#       base_claims |>
#         filter(!str_detect(id, "^(97bf469a)")) |>
#         transmute(
#           patient_id = patientid,
#           encounter_date = as.Date(servicedate),
#           provider_id = providerid,
#           primary_diagnosis = diagnosis1
#         ) |>
#         distinct()
#     }
#   )

# * stg_sepsis_patient ----
stg_sepsis_patient <-
  base_patients |>
  transmute(
    patient_id = str_sub(id, 1, 8),
    dob = as.Date(birthdate),
    race,
    ethnicity,
    zip,
    gender,
    death_date = as.Date(deathdate),
    patient_x = lon,
    patient_y = lat,
  ) |>
  inner_join(
    stg_encounter |>
      select(patient_id, hospital_discharge_date)
  ) |>
  mutate(
    .keep = "unused",
    death_date = if_else(death_date < hospital_discharge_date + days(1), hospital_discharge_date, death_date)
  ) |>
  arrange(patient_id)


# * stg_encounter_event ----
stg_encounter_event <-
  base_procedures |>
  transmute(
    encounter_id = str_sub(encounter, 1, 8),
    patient_id = str_sub(patient, 1, 8),
    event_start = start,
    event_end = stop,
    event_id = code,
    event_description = description
  ) |>
  filter(
    encounter_id %in% stg_encounter$encounter_id
  ) |>
  mutate(
    event_type = str_remove_all(event_description, ".*\\(|\\)$"),
    event_description = str_remove(event_description, " \\(.*")
  )

# * stg_sepsis_flowsheet ----
stg_sepsis_flowsheet <-
  base_observations |>
  filter(
    str_sub(encounter, 1, 8) %in% stg_encounter$encounter_id
  ) |>
  transmute(
    encounter_id = str_sub(encounter, 1, 8),
    patient_id = str_sub(patient, 1, 8),
    record_date = date,
    flowsheet_category = category,
    flowsheet_id = code,
    flowsheet_name = description,
    value,
    units,
    value_type = type,
    value_numeric = case_when(type == "numeric" ~ round(as.numeric(value), 2), .default = NA_real_),
    abnormal_ind =
      case_when(
        code %in% c("44963-7", "88262-1") ~ 1,
        code == "8478-0" & value_numeric < 60 ~ 1,
        code == "32693-4" & value_numeric >= 2 ~ 1,
        code == "44963-7" ~ 1,
        .default = 0
      )
  ) |>
  arrange(encounter_id, record_date)


# * stg_sepsis_medication ----
stg_sepsis_medication <-
  base_medications |>
  filter(
    str_sub(encounter, 1, 8) %in% stg_encounter$encounter_id
  ) |>
  transmute(
    encounter_id = str_sub(encounter, 1, 8),
    patient_id = str_sub(patient, 1, 8),
    medication_start_date = start,
    medication_end_date = stop,
    medication_id = code,
    medication_name = description
  ) |>
  arrange(encounter_id, medication_start_date)


# * fact_sepsis_encounter ----
fact_sepsis_encounter <-
  stg_encounter |>
  select(
    -c(event_type_id:reason_for_visit, hospital_x, hospital_y)
  ) |>
  left_join({
    stg_sepsis_patient |>
      select(patient_id, race, ethnicity, gender, dob, death_date)
  }) |>
  left_join({
    stg_sepsis_flowsheet |>
      filter(abnormal_ind == 1) |>
      summarise(
        .by = encounter_id,
        high_lactate_ind = max(flowsheet_id == "32693-4") |> as.integer(),
        low_map_ind = max(flowsheet_id == "8478-0") |> as.integer()
      )
  }) |>
  left_join({
    stg_encounter_event |>
      summarise(
        .by = encounter_id,
        ventilator_ind = max(event_id == "40617009") |> as.integer(),
        icu_ind = max(event_id == "305351004") |> as.integer()
      )
  }) |>
  left_join({
    stg_sepsis_medication |>
      filter(!str_detect(medication_name, "norepinephrine")) |>
      summarise(
        .by = encounter_id,
        abx_ind = 1
      )
  }) |>
  mutate(
    age_years = interval(dob, hospital_admit_date) / years(1) |> round(1),
    death_ind = as.integer(between(death_date, encounter_date, hospital_discharge_date))
  ) |>
  relocate(
    age_years,
    .after = gender
  ) |>
  select(-c(dob, death_date)) |>
  mutate(
    across(ends_with("_ind"), ~ replace_na(.x, 0))
  )

# Export ----
write_csv(stg_encounter, "export/stg_encounter.csv")
write_csv(stg_encounter_event, "export/stg_encounter_event.csv")
write_csv(stg_sepsis_flowsheet, "export/stg_sepsis_flowsheet.csv")
write_csv(stg_sepsis_medication, "export/stg_sepsis_medication.csv")
write_csv(stg_sepsis_medication, "export/stg_sepsis_medication.csv")
write_csv(fact_sepsis_encounter, "export/fact_sepsis_encounter.csv")


# Plots
fact_sepsis_encounter |>
  mutate(los_days = as.integer(los_days)) |>
  ggplot(aes(los_days, fill = as.factor(death_ind))) +
  geom_bar()
