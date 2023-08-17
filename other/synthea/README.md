# Synthea
`synthea` is a synthetic health record generator provided by [mitre.org](https://synthea.mitre.org/). It builds the medical records for the entire lifetime of `n` patients. It uses bayesian statistics and census data to send patients through different [health event modules](https://synthetichealth.github.io/module-builder/), for example [injuries](https://synthetichealth.github.io/module-builder/#injuries). The output is 18 data files (csv, ccda, fhir, json) from encounters and allergies to claims and durable medical equipment.

In this repo:
* this README has info on how to generate the data module
* there is a script in [build-data](https://github.com/rjake/one-off-projects/blob/main/other/synthea/build-data/load-data.sql) that uses [duckdb](https://duckdb.org/) to load the data
* the scripts in [clean-data](clean-data) are used to build curated data sets by defining cohorts and joining in dimensions

---

## Running Synthea
### Installation
1) Make sure you have Java 11 or newer (select JDK not JRE) [see here]()
  * **TODO: what is the difference between on [this](https://www.oracle.com/java/technologies/downloads/) page**
    > *JDK 20 is the latest release of Java SE Platform and JDK 17 LTS is the latest long-term support release for the Java SE platform.*
    * https://www.oracle.com/java/technologies/downloads/#jdk20-windows
    * https://www.oracle.com/java/technologies/downloads/#jdk17-windows
2) Add to Java PATH (ex. `C:\Program Files\Java\jdk-20\bin`) and restart computer
3) Install synthea jar file to folder you want to work in
  * https://github.com/synthetichealth/synthea/releases/download/master-branch-latest/synthea-with-dependencies.jar

**TODO: check these instructions https://github.com/synthetichealth/synthea/wiki/Developer-Setup-and-Running so you can use `run_synthea` on the command line**


### Running
```bash
cd /directory/you/downloaded/synthea/to   # go to directory ex: `~/github/one-off-projects/other/synthea/build-data/`
java -jar synthea-with-dependencies.jar   # check that it's running

# run synthea                         n patients      export to specified directory        city or state to user for demograhics
java -jar synthea-with-dependencies.jar -c resources/synthea.properties -p 20    --exporter.baseDirectory=output/n-20    Pennsylvania Philadelphia
java -jar synthea-with-dependencies.jar -c resources/synthea.properties -p 10000 --exporter.baseDirectory=output/n-10000 Pennsylvania Philadelphia
#   |-jar file------------------------| |-config----------------------| |-pop--| |-option----------------=--output dir-| |-location---------------|
```
**TODO: not sure how to use these options `-d ./modules -m ed_fractures` (custom module)**


These are the arguments
```bash
[-h]
[-s seed]
[-r referenceDate as YYYYMMDD]
[-cs clinician seed]
[-p populationSize]
[-g gender]               # M or F
[-a minAge-maxAge]        # ex: -a 30-40
[-c localConfigFilePath]  # if custom properties in a location other than "./resources/synthea.properties", ex: -c "../config/synthea.properties"
[-d localModulesDirPath]  # you can create custom modules
[state [city]]            # ex: Utah "Salt Lake City"    or  Washington Seattle  does not use prefix
```

## Cleaning Data
* Import data into a database:
  * A good resource is [duckdb](https://duckdb.org/)
  * See [load-data.sql](build-data/load-data.sql) for an example of using duckdb to load data

## Tutorials & resources
* [wiki](https://github.com/synthetichealth/synthea/wiki)
* [data dictionary](https://github.com/synthetichealth/synthea/wiki/CSV-File-Data-Dictionary)
* [BioHackrXiv.org instructions](https://biohackrxiv.org/q4zgx/)
* [healthit.gov instructions](https://www.healthit.gov/sites/default/files/page/2022-04/Synthetic%20Health%20Data%20Challenge_Technical%20Guidance%20and%20Tips_508.pdf)
* [Other data resources](https://synthea.mitre.org/downloads)
  * SyntheticMass [paper](https://doi.org/10.1093/jamia/ocx079)
    * dowload from GitHub [here](https://github.com/synthetichealth/synthea-sample-data/tree/master/downloads)
  * COVID [paper](https://www.sciencedirect.com/science/article/pii/S2666521220300077?via%3Dihub) - either 10K records (54 MB) or 100K (512 MB)
  * Childhood Obesity (295 MB) -
  * Oh Canada! (124 MB) Synthetic Canadian patients spread across provinces.
  * Breast Cacer [info](https://confluence.hl7.org/display/COD/mCODE+Test+Data)
  * "The Coherent Data Set" (9 GB) [paper](https://www.mdpi.com/2079-9292/11/8/1199) - Cardio Vascular Disease (those with CVD and those with risk factors). FHIR links all the data types together
    * patient conditions, encounters, medications
    * DICOM images (MRI)
    * genomic data
    * physiological data (i.e., ECGs)
    * simple clinical notes
