# Desc: Download all of the raw data from the s3 bucket and unzip it.
# Usage: bash download.sh
# Options: --clean: Additionally download the compas, lsat, ripa, and sqf
#          "clean" datasets. (The clean lending data are always downloaded.)
#          --clean-arxiv: Download the version of the clean datasets that were used in
#          the arxiv paper. (NOTE: To recreate the results in that version, you
#          must revert to the commit tagged "arxiv".) This supercedes the
#          --clean option.

USAGE="Usage: bash download.sh [--clean] [--clean-arxiv]"

S3_BUCKET_URL=https://outcomepp.s3.us-east-2.amazonaws.com/

# Parse command line arguments
CLEAN=false
CLEAN_ARXIV=false
while [ $# -gt 0 ]; do
  case "$1" in
    --clean-arxiv)
      CLEAN_ARXIV=true
      ;;
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
DATASETS="lending compas lsat ripa sqf"
if [ "$CLEAN_ARXIV" = true ]; then
  DATASETS="clean-compas-arxiv clean-lsat-arxiv clean-ripa-arxiv clean-sqf-arxiv $DATASETS"
elif [ "$CLEAN" = true ]; then
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
