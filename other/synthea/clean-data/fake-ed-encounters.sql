/*
Describes ED visits
*/
copy ed_encounters to '../output/ed_encounters.csv' (header, delimiter ',');
drop table ed_encounters if exists;
create table ed_encounters as 

with
cohort as (
    select
        round(extract('epoch' from age(en.stop, en.start)) / 60.0 / 60.0, 1) as los_hour,
        en."start" + to_years(
            case
                when month(birthdate) < 7 or substring(en.id, 1, 1) in ('1', '2') then 2023
                else 2022
                end -  year(en."start")::int
            ) as encounter_date, -- a little more in 2023
        (en."start"::date - pt.birthdate)::int as age_days,
        regexp_replace(en.reasondescription, '(.*) \(.*', '\1') as visit_group,
        --en."start" + to_years( 2023 - year(en.start)::int ) as encounter_date, -- make 2023
        en.*
    from
        main.encounters           en
        inner join main.patients  pt on pt.id = en.patient
    where
        en.encounterclass = 'emergency' --or regexp_matches(en.description, '(?i)admission|emergency|hospital|inpatient|unit')
        and en.reasondescription is not null
        and regexp_matches(en.reasondescription, '(?i)burn|concussion|fracture|laceration|sprain')
        --and age_days / 365.25 < 105
) --select * from cohort;visit_group, count(*) from cohort group by 1;

, total_meds as (
    -- select encounter, count(*) as n from main.medications group by 1
    select
        encounter,
        dispenses,
        description as medication,
        totalcost as total_cost,
        case when regexp_matches(lower(description), 'codone|meperidine') then 1 else 0 end as opioid_ind,
        row_number() over(partition by encounter order by totalcost desc) as med_rank
    from main.medications
    --where encounter = '3143c9be-20b7-7f4b-d6ec-d82b0fae015d'
) --select * from total_meds;

, total_careplan as (
    select
        cp.encounter,
        cp.description,
        case when cp.reasondescription like '%disorder%' then 'Other' else cp.reasondescription end as reasondescription
    from
        main.careplans    cp
        --inner join cohort co on co.id = cp.encounter
) --select * from total_careplan;

, total_procs as (--select encounter, count(*) as n from main.procedures group by 1)
    select
        encounter,
        --description,
        max(case when description like 'Suture%' then 1 else 0 end) as req_sutures_ind,
        sum(case when description like 'Surg%' then 1 else 0 end) as req_surgery_ind,
        max(case when description = 'Bone immobilization' then 1 else 0 end) as req_immobilization_ind,
        max(case when description like  'Admission%' then 1 else 0 end) as req_admission_ind,
        sum(case when regexp_matches(description, 'Radiog|scan|ray$') then 1 else 0 end) as n_imaging,
        group_concat(
            case when description not in ('Admission to burn unit', 'Injection of tetanus antitoxin', 'Bone density scan (procedure)') then reasondescription end,
            '^'
        ) as proc_reasons
    from main.procedures
    where encounter in (select id from cohort)
    group by 1
) --select * from total_procs;

, total_conditions as (--select encounter, count(*) as n from main.procedures group by 1)
    select
        cn.encounter,
        count(*) as n_cond
    from
        main.conditions   cn
        inner join cohort co on cn.patient = co.patient
    group by 1
) --select * from total_conditions;

select -- en.description, count(*), count(distinct en.id), sum(si.n)
    en.id as encounter_id,
    co.encounter_date,
    year(co.encounter_date) as year_date,

    -- pt.id as patient_id, pt."last" || ', ' || pt."first" as patient_name,
    -- pt.birthdate, en."start", en.stop,

    (co.los_hour + (2 - random())) * ( --add randomness to LOS
        case
            when regexp_matches(co.visit_group, 'Laceration|Sprain') then 1
            when co.visit_group like 'Fracture%' and coalesce(tc.n_cond, 1) > 8 then 2
            else coalesce(tc.n_cond, 1)
            end
        ) as los_hour,
    -- patient
    co.encounter_date - to_days(co.age_days) as dob,
    case
        when extract('epoch' from age(en.start::date, pt.birthdate)) / 60.0 / 60.0 / 24 / 365.25 < 18 then '-'
        else case pt.marital
            when 'M' then 'Married'
            when 'D' then 'Divorced'
            when 'W' then 'Widowed'
            else 'Single'
            end
        end as marital_status,
    pt.race,
    pt.ethnicity,
    pt.gender,
    -- visit
    case when age_days / 365.25 < 18 then 'CHILDREN''S HOSPITAL OF PHILADELPHIA' else og.name end as hospital_cachement_area,-- en.organization,
    --og.name end as organization_name,
    pr.name as provider_name,-- en.provider,
    py.name as payor_name, --en.payer,
    --case lower(en.description) when  'emergency room admission (procedure)' then 'emergency room admission' else lower(en.description) end as visit_type,
    visit_group as visit_group,
    coalesce(
        cp.reasondescription,
        tp.proc_reasons,
        case
            when en.reasondescription like '%Concussion%' then 'Concussion'
            when en.reasondescription like 'Gunshot%' then 'Bullet wound'
--            when en.reasondescription like 'Burn%' then 'Other burn'
            when en.reasondescription like 'Sprain%' then 'Other'
        end,
        ''--regexp_replace(en.reasondescription, '(.*) \(.*', '\1')
        'Other'
    ) as visit_reason,
    -- location
    pt.lon as patient_x,
    pt.lat as patient_y,
    round(
        acos(-- distance in miles given curvature of earth
            cos(radians(90 - og.lat)) * cos(radians(90 - pt.lat)) +
            sin(radians(90 - og.lat)) * sin(radians(90 - pt.lat)) * cos(radians(og.lon - pt.lon))
        ) * 3959,
        1
    ) as distance_miles,
    -- events
    case when tp.n_imaging > 0 then 1 else 0 end as imaging_ind,
    case when tc.n_cond > 4 then 1 else 0 end as medically_complex_ind,
    case
        when pt.deathdate <= en.stop then 'death'
        when coalesce(tp.req_sutures_ind, 0) = 1 then 'sutures'
        when coalesce(tp.req_immobilization_ind, 0) = 1 then 'immobilization'
        when coalesce(tp.req_admission_ind, 0) = 1 then 'admission'
        when en.reasondescription like 'Gunshot%' then 'other'
        when en.reasondescription like 'Traumatic injury of spinal cord%' then 'admission'
        when coalesce(tp.req_surgery_ind, 0) = 1 then 'surgery'
        when tm.medication is not null then 'medication'
        else 'other'
    end as injury_outcome,
    --tm.medication,
    coalesce(tm.opioid_ind, 0) as opioid_given_ind,
    en.patient as patient_id
from
    main.encounters                  en
    inner join cohort                co on co.id        = en.id
    left join total_meds             tm on tm.encounter = en.id and med_rank = 1  --  1,433
    left join total_procs            tp on tp.encounter = en.id  --  1,433
    left join total_conditions       tc on tc.encounter = en.id
    left join total_careplan         cp on cp.encounter = en.id
    left join main.patients          pt on pt.id        = en.patient
    left join main.payers            py on py.id        = en.payer
    left join main.providers         pr on pr.id        = en.provider
    left join main.closest_hospital  og on og.id        = en.organization
where
    co.age_days < 365.25 * 18
    --and co.los_hour between 1 and 20 * 24 -- LOS from 2 hours to 14 days
   -- and lower(en.reasondescription) like '%heart failure%'
    -- and lower(en.reasondescription) <> 'primary small cell malignant neoplasm of lung  tnm stage 1 (disorder)'
--group by 1
