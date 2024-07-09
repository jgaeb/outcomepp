# Desc: Download the RIPA Stop Data 2022 dataset from the California Department
# of Justice Open Justice website and unzip it.

DATA_URL=https://data-openjustice.doj.ca.gov/sites/default/files/dataset/2023-12/RIPA-Stop-Data-2022.zip

# Check if `curl` is installed
if ! command -v curl &> /dev/null
then
  echo "curl could not be found"
  exit 1
fi

curl -L $DATA_URL -o data/ripa/data.zip
unzip data/ripa/data.zip -d data/ripa
rm data/ripa/data.zip
touch data/ripa/*
