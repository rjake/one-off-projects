--  you can either write to memory or write to a db file, you set this in the duckdb config

-- set the search path then code runs from here, may need to create final item: 'closest_hospital'
set file_search_path = 'C:/Users/rileyj3/github/chop-pkgs/chopdata/data-raw/scratchwork/synthea/output/n-1000/csv/';

select * from duckdb_settings() where name = 'file_search_path';
set preserve_identifier_case = false; -- this doesn't seem to work?

create or replace table allergies as select * from read_csv_auto('allergies.csv');
create or replace table careplans as select * from read_csv_auto('careplans.csv');
create or replace table claims as select * from read_csv_auto('claims.csv');
--create table claims_transactions as select * from read_csv_auto('claims_transactions.csv');
create or replace table conditions as select * from read_csv_auto('conditions.csv');
--create table devices as select * from read_csv_auto('devices.csv');
create or replace table encounters as select * from read_csv_auto('encounters.csv');
create or replace table imaging_studies as select * from read_csv_auto('imaging_studies.csv');
create or replace table immunizations as select * from read_csv_auto('immunizations.csv');
create or replace table medications as select * from read_csv_auto('medications.csv');
create or replace table observations as select * from read_csv_auto('observations.csv');
create or replace table organizations as select * from read_csv_auto('organizations.csv');
create or replace table patients as select * from read_csv_auto('patients.csv');
create or replace table payer_transitions as select * from read_csv_auto('payer_transitions.csv');
create or replace table payers as select * from read_csv_auto('payers.csv');
create or replace table procedures as select * from read_csv_auto('procedures.csv');
create or replace table providers as select * from read_csv_auto('providers.csv');
create or replace table supplies as select * from read_csv_auto('supplies.csv');

create or replace table closest_hospital as select * from read_csv_auto('../closest_hospital.csv'); 

create or replace table encounter_diagnosis as 
    select
        en.id,
        min(dx.description) as diagnosis_name
    from 
        main.encounters        en
        inner join main.claims cl on cl.patientid = en.patient and cl.servicedate::date = en.start::date and cl.providerid = en.provider
        inner join conditions  dx on dx.code = cl.diagnosis1
    group by 1
;

select diagnosis_name, count(*)
from encounter_diagnosis
group by 1
;

select * 
from information_schema.columns 
where lower(column_name) like '%code%' order by table_name
;

;

create or replace table code_ids as
with
all_codes as (
    select 'allergies' as table_name, code, description, count(*) as n from allergies group by 1,2,3
    union all select 'careplans', code, description, count(*) as n  from careplans group by 1,2,3
    union all select 'careplans-reason', reasoncode, reasondescription, count(*) as n  from careplans group by 1,2,3
    --union all select 'claims_transactions', procedurecode, notes, count(*) as n  from claims_transactions group by 1,2,3
    union all select 'conditions', code, description, count(*) as n  from conditions group by 1,2,3
    --union all select 'devices', code, description, count(*) as n  from devices group by 1,2,3
    union all select 'encounters', code, description, count(*) as n  from encounters group by 1,2,3
    union all select 'encounters-reason', reasoncode, reasondescription, count(*) as n  from encounters group by 1,2,3
    union all select 'immunizations', code, description, count(*) as n  from immunizations group by 1,2,3
    union all select 'medications', code, description, count(*) as n  from medications group by 1,2,3
    union all select 'medications-reason', reasoncode, reasondescription, count(*) as n  from medications group by 1,2,3
    union all select 'observations', code, description, count(*) as n  from observations group by 1,2,3
    union all select 'procedures', code, description, count(*) as n  from procedures group by 1,2,3
    union all select 'procedures-reason', reasoncode, reasondescription, count(*) as n  from procedures group by 1,2,3
    union all select 'supplies', code, description, count(*) as n  from supplies group by 1,2,3
)
select *
from all_codes
where description is not null
;
-- conditions
select count(*) from careplans where code = '267102003';
select count(*) from encounters where reasoncode = '267102003'