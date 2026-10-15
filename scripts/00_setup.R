# Set up the project: restore packages and create folders
# Run once from the project root (the folder containing renv.lock):
#   source("scripts/00_setup.R")

if (!file.exists("renv.lock")) {
  stop("renv.lock not found. Set the working directory to the project root.")
}

# -------------------------------------------------------------------------
# Package environment
# -------------------------------------------------------------------------

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

# Creates renv/ and .Rprofile if missing, without altering the lockfile
if (!file.exists("renv/activate.R")) {
  renv::init(bare = TRUE, restart = FALSE)
}

renv::restore(prompt = FALSE)

# -------------------------------------------------------------------------
# Folders
# -------------------------------------------------------------------------

dirs <- c("data/derived", "figures")
for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# -------------------------------------------------------------------------
# Input data check
# -------------------------------------------------------------------------

input_files <- c(
  "data/simulated-dataset-2/sub-30/ses-01/sub-30_ses-01_task-nback_behav.csv",
  "data/simulated-dataset-2/sub-30/ses-01/sub-30_ses-01_task-nback_eeg.csv"
)

missing <- input_files[!file.exists(input_files)]
if (length(missing) > 0) {
  warning(
    "Input data missing. Place the course dataset in data/ so these exist:\n",
    paste0("  ", missing, collapse = "\n")
  )
} else {
  message("Setup complete: packages restored and input data found.")
}
