cd C:/Users/rileyj3/github/chop-pkgs/chopdb/data-raw/synthea


https://mitre.github.io/fhir-for-research/modules/synthea-customizing

java -jar synthea-with-dependencies.jar -c resources/synthea.properties -d resources/ -p 1000 -a 0-18 --exporter.baseDirectory=output/n-1000 Pennsylvania Philadelphia

java -jar synthea-with-dependencies.jar -c resources/synthea.properties -d resources/ -p 1000 --exporter.baseDirectory=output/n-1000 Pennsylvania Philadelphia
