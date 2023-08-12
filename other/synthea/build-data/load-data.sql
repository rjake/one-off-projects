--  you can either write to memory or write to a db file, you set this in the duckdb config

-- set the search path then code runs from here, may need to create final item: 'closest_hospital'
set file_search_path = '~/github/one-off-projects/other/synthea/output/n-2000/csv/';


set preserve_identifier_case = false; -- this doesn't seem to work?

create table allergies as select * from read_csv_auto('allergies.csv');
create table careplans as select * from read_csv_auto('careplans.csv');
create table claims as select * from read_csv_auto('claims.csv');
create table claims_transactions as select * from read_csv_auto('claims_transactions.csv');
create table conditions as select * from read_csv_auto('conditions.csv');
create table devices as select * from read_csv_auto('devices.csv');
create table encounters as select * from read_csv_auto('encounters.csv');
create table imaging_studies as select * from read_csv_auto('imaging_studies.csv');
create table immunizations as select * from read_csv_auto('immunizations.csv');
create table medications as select * from read_csv_auto('medications.csv');
create table observations as select * from read_csv_auto('observations.csv');
create table organizations as select * from read_csv_auto('organizations.csv');
create table patients as select * from read_csv_auto('patients.csv');
create table payer_transitions as select * from read_csv_auto('payer_transitions.csv');
create table payers as select * from read_csv_auto('payers.csv');
create table procedures as select * from read_csv_auto('procedures.csv');
create table providers as select * from read_csv_auto('providers.csv');
create table supplies as select * from read_csv_auto('supplies.csv');

-- see R script in resources/ folder
create table closest_hospital as select * from read_csv_auto('../closest_hospital.csv'); 
