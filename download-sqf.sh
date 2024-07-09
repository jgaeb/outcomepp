# Desc: Download the stop-question-frisk data from NYPD website

DATA_URL="https://www.nyc.gov/assets/nypd/downloads/zip/analysis_and_planning/stop-question-frisk/sqf-"

for YEAR in {2008..2012}
do
  curl -L "${DATA_URL}${YEAR}-csv.zip" -o "data/sqf/${YEAR}.zip"
  unzip -o "data/sqf/${YEAR}.zip" -d "data/sqf"
  rm "data/sqf/${YEAR}.zip"
  touch "data/sqf/${YEAR}.csv"
done
