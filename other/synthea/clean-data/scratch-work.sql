/*--- ENCOUNTERS --------------------------------------------------------------------------------------*/

--create table encounter_vitals as
select
    encounter,
    max(case when code = '72514-3' then value end)::int as pain_score,
    max(case when code = '8867-4' then value end)::int as heart_rate,
    max(case when code = '9279-1' then value end)::int as respiratory_rate,
    max(case when code = '72166-2' then value end) as smoker_history
from main.observations
--where encounter = '76ebaf2e-5513-739d-e8cd-9ca45ade73ea'
group by 1
;


select code, description, count(distinct encounter), count(*)
from main.observations
group by 1,2
;


select
    og.name,
    count(*) as n
from
    main.encounters                en
    left join main.organizations   og on og.id = en.organization
where
    en.encounterclass = 'emergency'
    and og.name not like '%HOSPIT%'
group by 1
;




/*--- CLAIMS ------------------------------------------------------------------------------------------*/

select *    --ct.claimid, max(ct.todate) as max_payment_date
from main.claims_transactions as ct
where
--    ct.claimid = 'a138aaa1-fef3-69a3-299e-bb20351557bb'--'fd53edbd-c99a-7fed-39c6-ca9742b48eac'--'afae7a63-3abe-c509-e2fb-bbcb23e1109f'
--    and
    ct.type in ('CHARGE', 'PAYMENT')
    and ct.claimid = '8e091bfe-3b7e-06d9-4334-8244d4da75cc'
--group by 1
;
select
    ct.id as transaction_id,
    ct.claimid as claim_id,
    ct.chargeid::int as charge_id,
    ct.type,
    ct.notes,
    round(ct.amount, 2) as amount,
    --ct.fromdate, ct.todate,
    round(payments, 2) as payment,
    round(adjustments, 2) as adjustment,
    round(transfers, 2) as transfer,
    round(outstanding, 2) as outstanding,
    fromdate,
    todate
--;select *
from
    main.claims_transactions as ct
where
--    ct.claimid = 'a138aaa1-fef3-69a3-299e-bb20351557bb'--'fd53edbd-c99a-7fed-39c6-ca9742b48eac'--'afae7a63-3abe-c509-e2fb-bbcb23e1109f'
--    and
    ct.type in ('CHARGE', 'PAYMENT')
    --and ct.claimid = '8e091bfe-3b7e-06d9-4334-8244d4da75cc'--'3ee00201-eaec-d4f6-f854-59011e58d9da'
    --and ct.todate != '1970-01-01'
order by chargeid
;
select max(fromdate) as max_date from main.claims_transactions
;

select *
from main.claims_transactions
where claimid in ('23523a4a-faad-9835-ab1b-bf7d861524a3', '2b1ccea9-c21a-3294-2fee-24351323d457')
;
select
    '2023-08-07'::date as service_date,
    '1984-09-01'::date as birth_date,
    (service_date - birth_date) / 365.25 as age_years,
    date_diff('hour', birth_date, service_date)
;
select 19 % 9
