# NOTE: Not to be run directly, but sourced from the individual data processing
# scripts. The `groundhog` package should be loaded.

groundhog.library("argparse", "2024-07-04")

# Set the seed for reproducibility.
set.seed(8460037)

# Parse command line arguments.
parser <- ArgumentParser()
parser$add_argument("--n_buckets", type = "integer")
parser$add_argument("--n_bootstrap", type = "integer")
parser$add_argument("--n_tiles", type = "integer")
parser$add_argument("--alpha0", type = "numeric")
parser$add_argument("--alpha1", type = "numeric")
args <- parser$parse_args()

# Define the number of buckets, bootstrap iterations, and alpha level.
n_buckets <- args$n_buckets
n_bootstrap <- args$n_bootstrap
n_tiles <- args$n_tiles
alpha0 <- args$alpha0
alpha1 <- args$alpha1
