# Desc: Download the Broward County COMPAS dataset

DATA_URL="https://github.com/propublica/compas-analysis/raw/master/compas.db"

# Check if `curl` is installed
if ! command -v curl &> /dev/null
then
  echo "curl could not be found"
  exit 1
fi

curl -L $DATA_URL -o data/compas/compas.db
