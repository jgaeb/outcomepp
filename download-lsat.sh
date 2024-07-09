# Desc: Download the LSAC bar passage data

DATA_URL="https://storage.googleapis.com/lawschool_dataset/bar_pass_prediction.csv"

# Check if `curl` is installed
if ! command -v curl &> /dev/null
then
  echo "curl could not be found"
  exit 1
fi

curl -L $DATA_URL -o data/lsat/lsac.csv
