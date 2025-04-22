with
cohort as (
    select 
        *
    from 
        main.encounters
    where
        code = '185345009'
        and reasoncode in ('195662009', '43878008')
--        diagnosis_name in (
--            'Acute viral pharyngitis (disorder)',
--            'Streptococcal sore throat (disorder)'
--        )
    limit 2000
        --and age_days / 365.25 < 105
) --select * from cohort;visit_group, count(*) from cohort group by 1;

, encounter_events as (
    select
        co.id,
        co.patient,
        co.start::date as encounter_date,
        --co.stop,
        max(case when ob.code = '8310-5' then value end)::int as body_temp,
        max(case when md.code = '834061' then 1 else 0 end) as abx_med_ind,
        max(case when md.code = '834061' then md.stop end) as abx_end_date,
        max(case when pc.code = '117015009' then 1 else 0 end) as need_culture_ind
--        ,
--        min(case when )
    from 
        cohort                       co
        --left join main.conditions    cn on cn.encounter = co.id and cn.code in ('195662009', '43878008')
        left join main.observations  ob on ob.encounter = co.id
        left join main.medications   md on md.encounter = co.id and md.start::date = co.start::date
        left join main.procedures    pc on pc.encounter = co.id
        
        --left join main.conditions    cn
    --where encounter = '76ebaf2e-5513-739d-e8cd-9ca45ade73ea'
    group by 1,2,3
) --select * from encounter_events where patient = '5e493ec4-03b4-f011-8d61-46fb7aa1d1ea';
--
--select * from conditions where patient = '5e493ec4-03b4-f011-8d61-46fb7aa1d1ea'; --2023-08-08 18:05:40.000
--select * from main.encounters where reasoncode = '267102003';
--select * from code_ids where code in ('185345009');
--;
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
    en.start::date as encounter_date,
    en.start as appointment_start,
    en.stop as appointment_end,
    round(extract('epoch' from age(en.stop, en.start)) / 60.0, 1) - 3 + minute(en.start) % 15 as los_min, -- add a little variation
    year(en.start) as calendar_year,
    -- visit
    og.name as organization_name,
    pr.name as provider_name,
    py.name as payor_name,
    en.reasondescription as diagnosis_name,
    -- observations / indicators
    ee.body_temp,
    ee.need_culture_ind,
    ee.abx_med_ind,
    ee.abx_end_date,
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
--select count(*)
from
    cohort                           co
    inner join main.encounters       en on en.id        = co.id
    left join encounter_events       ee on ee.id        = en.id
    left join main.patients          pt on pt.id        = en.patient
    left join main.payers            py on py.id        = en.payer
    left join main.providers         pr on pr.id        = en.provider
    left join main.organizations     og on og.id        = en.organization
    --left join main.code_ids         dx on dx.code::text      = en.reasoncode::text
where
    en.reasoncode is not null
   -- and en.patient = '38da232d-927b-fc21-056a-9cf30bcb198d'
--    and en.id in (
--        '0c7e2558-ff6c-e293-bb45-d899ba44af15',
--        'b84671cb-e2bd-44a3-465c-398bce75afc2'
--    )
;
select * 
from encounter_diagnosis
where 
    description ilike '%sore%'

;
select count(*)
from main.encounters
where code = '185345009'