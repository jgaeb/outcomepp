# Desc: Download all of the raw data from the s3 bucket and unzip it.
# Usage: bash download.sh
# Options: --clean: Additionally download the compas, lsat, ripa, and sqf
#          "clean" datasets. (The clean lending data are always downloaded.)

USAGE="Usage: bash download.sh [--clean]"

S3_BUCKET_URL=https://outcomepp.s3.us-east-2.amazonaws.com/

# Parse command line arguments
CLEAN=false
while [ $# -gt 0 ]; do
  case "$1" in
    --clean)
      CLEAN=true
      ;;
    *)
      echo "Unknown option: $1"
      echo $USAGE
      exit 1
      ;;
  esac
  shift
done

# Choose which datasets to download
DATASETS="clean-lending compas lsat ripa sqf"
if [ "$CLEAN" = true ]; then
  DATASETS="clean-compas clean-lsat clean-ripa clean-sqf $DATASETS"
fi

# Download and unzip the datasets
for DATASET in $DATASETS
do
  # Get the directory
  DATA_DIR="data/${DATASET%%-*}"
  curl -L "${S3_BUCKET_URL}${DATASET}.zip" -o "${DATA_DIR}/data.zip"
  unzip -o -d "${DATA_DIR}" "${DATA_DIR}/data.zip"
  rm "${DATA_DIR}/data.zip"
  if [ "$DATA_DIR" = "data/clean" ]; then
    touch "${DATA_DIR}/${DATASET##*-}"*
  else
    touch "${DATA_DIR}"/*
  fi
done
