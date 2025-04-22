create or replace table fact_encounter as

with
cohort as (
    select 
        id,
        description,
        count(*) over (partition by description) as n_visit_reason
    from 
        main.encounters           en
    where
        en.description not ilike  '%death%'
        and en.description not ilike  '%natal%'
    group by 1,2
        --and age_days / 365.25 < 105
) --select * from cohort;visit_group, count(*) from cohort group by 1;

, encounter_events as (
    select
        co.id,
        max(case when ob.code = '29463-7' then value end)::int as weight,                
        max(case when ob.code = '8302-2' then value end) as height,
        max(case when ob.code = '8867-4' then value end)::int as heart_rate,
        max(case when ob.code = '72514-3' then value end)::int as pain_score,        
        max(case when ob.category = 'laboratory' then 1 else 0 end) as labs_ind,
        max(case when im.encounter is not null then 1 else 0 end) as immunizaiton_ind,
        max(case when md.encounter is not null then 1 else 0 end) as medication_ind,
        max(case when ob.code in ('70274-6', '89204-2') then 1 else 0 end) as patient_pro_ind
    from 
        cohort                       co
        left join main.observations  ob on ob.encounter = co.id
        left join main.immunizations im on im.encounter = co.id
        left join main.medications   md on md.encounter = co.id
    --where encounter = '76ebaf2e-5513-739d-e8cd-9ca45ade73ea'
    group by 1
) --select * from encounter_events;

, total_careplan as (
    select
        cp.encounter,
        cp.description,
        cp.reasondescription
    from
        main.careplans    cp
    where 
        cp.description != 'Infectious disease care plan (record artifact)'
        --inner join cohort co on co.id = cp.encounter
) --select * from total_careplan;

, total_procs as (--select encounter, count(*) as n from main.procedures group by 1)
    select
        encounter,
        --description,
        max(case when description like 'Patient referral%' then 1 else 0 end) as referral_ind,
        max(case when description like 'Depression screening%' then 1 else 0 end) as depression_screening_ind,
        max(case when code = 428211000124100 then 1 else 0 end) as substance_abuse_hx_ind,
        max(case when code = 370995009 then 1 else 0 end) as education_ind
    from main.procedures
    group by 1
) --select * from total_procs;

select 
    en.id as encounter_id,
    en.patient as patient_id,
--    -- patient
    pt.birthdate as dob,
    pt.race as race,
    pt.ethnicity as ethnicity,
    pt.gender as gender,
    (en."start"::date - pt.birthdate)::int as age_days,
    round(extract(epoch from en.start - pt.birthdate) / 60.0 / 60.0 / 24.0 / 365.25, 1) as age_years,
    case when en.start = min(en.start) over (partition by en.patient) then 1 else 0 end as first_visit_ind,
    en.start::date as encounter_date,
    en.start as appointment_start,
    en.stop as appointment_end,
    round(extract('epoch' from age(en.stop, en.start)) / 60.0, 1) - 3 + minute(en.start) % 3 as los_min, -- add a little variation
    year(en.start) as calendar_year,
    -- visit
    og.name as organization_name,
    pr.name as provider_name,-- en.provider,
    py.name as payor_name, --en.payer,
    en.encounterclass as encounter_class,
    regexp_replace(co.description, '(.*) \(.*', '\1') as reason_for_visit,
    regexp_replace(co.description, '.*\(([^\)]+).*', '\1') as reason_group,
    case
        when co.description ilike '%emerg%' then 'Emergency'
        when co.description ilike '%admission%' then 'Admission'
        
        when co.description ilike '%urgent%' then 'Urgent Care'
        else 'Visit'
    end as encounter_type,
    en.description,
    dx.description as reason_description,
    -- observations / indicators
    ea.weight,
    ea.height,
    ea.heart_rate,
    ea.pain_score,
    ea.labs_ind,
    ea.immunizaiton_ind,
    ea.medication_ind,
    ea.patient_pro_ind,
    coalesce(tp.referral_ind, 0) as referral_ind,
    coalesce(tp.depression_screening_ind, 0) as depression_screening_ind,
    coalesce(tp.substance_abuse_hx_ind, 0) as substance_abuse_hx_ind,
    coalesce(tp.education_ind, 0) as education_ind,
    -- location
    pt.lon as patient_x,
    pt.lat as patient_y,
    og.lon as location_x,
    og.lat as location_y,
    round(
        acos(-- distance in miles given curvature of earth
            cos(radians(90 - og.lat)) * cos(radians(90 - pt.lat)) +
            sin(radians(90 - og.lat)) * sin(radians(90 - pt.lat)) * cos(radians(og.lon - pt.lon))
        ) * 3959,
        1
    ) as distance_miles,
    en.provider as provider_id
    
from
    cohort                           co
    inner join main.encounters       en on en.id        = co.id
    inner join encounter_events       ea on ea.id        = en.id
    left join total_procs            tp on tp.encounter = en.id  --  1,433  
    left join total_careplan         cp on cp.encounter = en.id
    left join main.patients          pt on pt.id        = en.patient
    left join main.payers            py on py.id        = en.payer
    left join main.providers         pr on pr.id        = en.provider
    left join main.organizations     og on og.id        = en.organization
    left join main.code_ids         dx on dx.code::text      = en.reasoncode::text
--where
--    co.n_visit_reason > 20
--    and en.reasoncode is not null
   -- and en.patient = '38da232d-927b-fc21-056a-9cf30bcb198d'
--    and en.id in (
--        '0c7e2558-ff6c-e293-bb45-d899ba44af15',
--        'b84671cb-e2bd-44a3-465c-398bce75afc2'
--    )
;
select * 
from code_ids
where 
    description ilike '%sore%'

;
select count(*)
from clams
where code = '267102003'
;
select * 
from fact_encounter
where weight is not null+
