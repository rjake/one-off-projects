/*
Describes claims and financial transactions
*/

copy fake_claims to '../output/fake_claims.csv' (header, delimiter ',');
drop table fake_claims if exists;
create table fake_claims as 

with
claim_info as (
    select
        ct.claimid as claim_id,
        sum(case when ct.type = 'CHARGE' then 1 else 0 end) as n_charges,
        sum(case when ct.type != 'CHARGE' then 1 else 0 end) as n_payment,
        max(ct.fromdate + (month(fromdate) || ' day')::interval)::date as max_charge_date,
        max(fromdate + (month(fromdate) * day(fromdate) || ' day')::interval)::date as max_payment_date,
        sum(round(ct.amount, 2)) as charges,
        sum(round(payments, 2)) as payments
    from
        main.claims_transactions as ct
    where
        ct.type in ('CHARGE', 'PAYMENT')
        --and ct.claimid = '8e091bfe-3b7e-06d9-4334-8244d4da75cc'--'afae7a63-3abe-c509-e2fb-bbcb23e1109f'
    group by
        ct.claimid
) --select * from claim_info; where total_charged != total_payment ;

select
    --cl.id,
    --cl.patientid,
    rank() over(order by cl.id) as claim_id,
    --rank() over(order by cl.patientid) as patient_id,
    regexp_replace(pt.first || ' ' || pt.last, '\d+', '', 'g') as patient_name,
    --max(cl.servicedate) over(partition by 1) as max_date,
    round((cl.servicedate::date - pt.birthdate::date) / 365.25, 1) as age_years,
    --pt.birthdate,
    pt.race as race,
    pt.ethnicity as ethnicity,
    pt.gender as gender,
    --pt.city,
    --pt.birthplace,
    case cl.healthcareclaimtypeid1
        when 1 then 'PB'
        when 2 then 'HB'
        end as claim_type,
    hs.name as cost_center,
    regexp_replace(pr.name, '\d+', '', 'g') as provider_name,
    --year(cl.servicedate + interval 6 month) as fiscal_year,
    --date_trunc('month', servicedate) as calendar_month,
    --hour(cl.servicedate) as hour_date,
    case
        when md.max_date::date - cl.servicedate::date >= 90 and minute(cl.servicedate) < 3 then 'BILLED'
        else cl.status1
        end as claim_status,
    coalesce(p1.name, '(no insurance)') as payor_name,
    case
        when payor_name in ('Dual Eligible', 'Medicaid', 'Medicare') then 'Government'
        when payor_name = '(no insurance)' then 'Self Pay'
        else 'Private'
        end as payor_group,
    cl.servicedate::date as service_date,
    en.encounterclass as encounter_class,
    greatest(ci.max_charge_date, cl.lastbilleddate1)::date as last_charge_date,
    --ci.max_charge_date, lastbilleddate1,
    ci.max_payment_date as last_paid_date,
--    coalesce(cl.outstanding1, 0) as payor_outstanding,
--    cl.outstandingp as patient_outstanding,
    --date_diff('day', service_date, last_charge_date) as n_days_to_charge,
    --date_diff('day', last_charge_date, max_payment_date) as n_days_open,
    --en.base_encounter_cost,
    en.total_claim_cost,
    ci.n_charges,
    ci.charges as total_charged,
--    hour(cl.servicedate),
--    hour(cl.servicedate) / 23.0 as pct,
    case
        when claim_status = 'CLOSED' then ci.charges
        when hour(cl.servicedate) % 3 = 0 then 0
        else round(ci.payments * (1 - hour(cl.servicedate) / 23.0), 2)
    end as total_payment,
    total_charged - total_payment as outstanding--,
    --case when n_days_open > 90 then 1 else 0 end as open_gt90_ind
from
    main.claims                      cl
    cross join (select max(servicedate) as max_date from main.claims) as md
    inner join main.patients         pt on pt.id = cl.patientid
    left join main.payers            p1 on p1.id = cl.primarypatientinsuranceid
    inner join claim_info            ci on ci.claim_id = cl.id
    inner join main.encounters       en on en.id = cl.appointmentid
    left join main.providers         pr on pr.id = cl.providerid
    left join main.closest_hospital  hs on hs.id = en.organization
    --left join main.payers      p2 on p2.id = cl.secondarypatientinsuranceid
where
--    coalesce(ci.max_payment_date, current_date) > '1970-01-01'
    year(cl.servicedate + interval 6 month) between 2021 and 2022
    --and claim_status = 'BILLED'
    --and (cl.servicedate::date - pt.birthdate::date) / 365.25 < 18.5
    --and cl.patientid = '000bf59d-b3ea-e0ba-b4af-035c445f390a'
    --and p2.name = 'NO_INSURANCE'
    --and
    --and claim_status = 'BILLED'
    --and pt.id = '017b350e-0060-1db6-77a6-69b9804c92ff'
    --and cl.id = '8e091bfe-3b7e-06d9-4334-8244d4da75cc'--'a138aaa1-fef3-69a3-299e-bb20351557bb'--'fd53edbd-c99a-7fed-39c6-ca9742b48eac'
    --and payor_outstanding + patient_outstanding != total_charged;
    --cl.id = 'afae7a63-3abe-c509-e2fb-bbcb23e1109f' -- 2009-08-10 13:22:13.000   total charged = 14,734.88 outstanding = 22,102.33
--
