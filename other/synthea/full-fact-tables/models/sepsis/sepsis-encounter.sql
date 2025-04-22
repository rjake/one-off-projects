-- https://synthetichealth.github.io/module-builder/#sepsis

set file_search_path = 'C:/Users/rileyj3/github/chop-pkgs/chopdata/data-raw/scratchwork/synthea/output/n-1000/csv/';

select *
from main.code_ids
where code = 91302008
;
create or replace table fact_encounter as
select
    substr(en.id, 1, 8) as encounter_id,
    substr(en.patient, 1, 8) as patient_id,
    en.start::date as encounter_date,
    en.start as hospital_admit_date,
    en.stop as hospital_discharge_date,
    round(extract('epoch' from age(en.stop, en.start)) / 60.0 / 60.0 / 24.0, 1) as los_days, -- add a little variation
    -- visit
    hs.hospital_name,
    py.name as payor_name, --en.payer,
    -- location
    hs.hospital_x,
    hs.hospital_y
from
    main.encounters                  en
    left join main.payers            py on py.id        = en.payer
    left join main.closest_hospital  hs on hs.id        = en.organization
where
    en.code = '185347001'
    and en.reasoncode = '91302008'
    and en.start::date > '2000-01-01'
;
select * from fact_encounter;
;
create or replace table fact_patient as
select
    substr(pt.id, 1, 8) as patient_id,
    pt.birthdate::date as dob,
    pt.race as race,
    pt.ethnicity as ethnicity,
    pt.zip,
    pt.gender as gender,
    pt.lon as patient_x,
    pt.lat as patient_y,
    pt.deathdate::date as death_date
from
    main.fact_encounter     en
    inner join main.patients pt on substr(pt.id, 1, 8) = en.patient_id
;
select * from fact_patient;
;
create or replace table fact_encounter_event as
select
    en.encounter_id,
    en.patient_id,
    pc.start as event_start,
    pc.stop as event_end,
    pc.code as event_id,
    pc.description as event_description
from
    main.fact_encounter     en
    inner join main.procedures pc on substr(pc.encounter, 1, 8) = en.encounter_id
order by en.encounter_id, pc.start
;
select * from fact_encounter_event where encounter_id = '7f5c9743' order by event_start
;
create or replace table fact_flowsheet as
select
    en.encounter_id,
    en.patient_id,
    ob.date as record_date,
    ob.category,
    ob.code,
    ob.description,
    ob.value,
    ob.units,
    ob.type as value_type,
    case
        when ob.type = 'numeric' then round(value::numeric, 2)
    end as value_numeric,
    case
        when code = '8478-0' and value_numeric < 60 then 1
        when code = '32693-4' and value_numeric >= 2 then 1
        when code = '44963-7' and value like 'Increased%' then 1
        else 0
    end as abnormal_ind
    --ob.*
from
    main.fact_encounter          en
    inner join main.observations ob on substr(ob.encounter, 1, 8) = en.encounter_id
--where en.encounter_id = '7f5c9743'
order by en.encounter_id, ob.date
;
select * from fact_flowsheet where encounter_id = '7f5c9743' order by record_date;
select * from fact_flowsheet where code = '8480-6';
select code, description, count(*), min(value), max(value), mean(abnormal_ind) from fact_flowsheet group by 1,2
;
create or replace table fact_medication as
select
    en.encounter_id,
    en.patient_id,
    md.start as medication_start_date,
    md.stop as medication_end_date,
    md.code as medication_id,
    md.description as medication_name
from
    main.fact_encounter     en
    inner join main.medications md on substr(md.encounter, 1, 8) = en.encounter_id
--where en.encounter_id = '7f5c9743'
order by en.encounter_id, md.start
;
select * from fact_medication where encounter_id = '7f5c9743' order by medication_start_date
;

;
create or replace table sepsis_encounter as
with
indicators as (
    select
        encounter_id,
        max(case when fs.code = '32693-4' then fs.abnormal_ind else 0 end) as high_lactate_ind,
        max(case when fs.code = '8478-0' then fs.abnormal_ind else 0 end) as low_map_ind,
        max(case when ee.event_id = '40617009' then 1 else 0 end) as ventilator_ind,
        max(case when ee.event_id = '305351004' then 1 else 0 end) as icu_ind
    from
        fact_encounter                  en
        left join fact_encounter_event  ee using (encounter_id)
        left join fact_flowsheet        fs using (encounter_id)
    group by 1
) --select * from indicators;

select
    en.encounter_id,
    en.encounter_date,
    en.hospital_admit_date,
    en.hospital_discharge_date,
    en.los_days,
    en.hospital_name,
    -- patient
    pt.race,
    pt.ethnicity,
    pt.gender,
    round(
        extract(epoch from en.hospital_admit_date - pt.dob) / 60.0 / 60.0 / 24.0 / 365.25,
        1
    ) as age_years,
    en.payor_name,
    -- indicators
    id.high_lactate_ind,
    id.low_map_ind,
    id.ventilator_ind,
    id.icu_ind,
    case
        when pt.death_date between en.encounter_date and en.hospital_discharge_date + '1 day'::interval then 1
        else 0
    end as death_ind,
    -- ids
    en.patient_id
from
    fact_encounter          en
    left join indicators    id using (encounter_id)
    left join fact_patient  pt using (patient_id)
;

select *
from sepsis_encounter
;

COPY tbl TO 'output.csv' (HEADER, DELIMITER ',');
COPY tbl TO 'output.csv' (HEADER, DELIMITER ',');
COPY tbl TO 'output.csv' (HEADER, DELIMITER ',');
