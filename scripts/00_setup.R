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

# "auto":    restore the pinned versions if the R minor version matches renv.lock,
#            otherwise install current versions of the required packages
# "lockfile": always restore the pinned versions (needs matching R, and Rtools on Windows)
# "current":  always install current versions (no compiling on Windows/macOS)
# Current versions are installed into the project library only; renv.lock is not modified.
install_mode <- "auto"

required_pkgs <- c("tidyverse", "rsample", "yardstick", "glmnet")

minor <- function(v) paste(unlist(package_version(v))[1:2], collapse = ".")
lock_r <- renv::lockfile_read("renv.lock")$R$Version
r_matches <- minor(lock_r) == minor(getRversion())

use_lockfile <- switch(
  install_mode,
  auto = r_matches,
  lockfile = TRUE,
  current = FALSE,
  stop("install_mode must be \"auto\", \"lockfile\" or \"current\".")
)

if (use_lockfile) {
  if (!r_matches) {
    warning(
      "renv.lock was created with R ", lock_r, ", you are running R ",
      getRversion(), ". Old package versions may fail to compile."
    )
  }
  # Windows source builds need Rtools (provides 'make')
  if (.Platform$OS.type == "windows" && Sys.which("make") == "") {
    warning(
      "Rtools not found. If restore fails with \"'make' not found\", install ",
      "Rtools matching your R version from ",
      "https://cran.r-project.org/bin/windows/Rtools/ and restart R."
    )
  }
  renv::restore(prompt = FALSE)
} else {
  message(
    "Installing current versions of: ", paste(required_pkgs, collapse = ", "),
    " (renv.lock, made with R ", lock_r, ", is not used)."
  )
  renv::install(required_pkgs, prompt = FALSE)
}

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
