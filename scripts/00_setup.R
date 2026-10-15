# Set up the project: restore packages and create folders

# Run from the project root containing renv.lock

if (!file.exists("renv.lock")) {
  stop(
    "renv.lock not found. Set the working directory to the project root."
  )
}

# -------------------------------------------------------------------------
# Package environment
# -------------------------------------------------------------------------

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

# Create the renv infrastructure if it is missing

if (!file.exists("renv/activate.R")) {
  renv::init(
    bare = TRUE,
    restart = FALSE
  )
}

install_mode <- "auto"

required_pkgs <- c(
  "tidyverse",
  "rsample",
  "yardstick",
  "glmnet",
  "gsignal"
)

minor <- function(v) {
  paste(
    unlist(package_version(v))[1:2],
    collapse = "."
  )
}

lock_r <- renv::lockfile_read(
  "renv.lock"
)$R$Version

r_matches <- minor(lock_r) == minor(getRversion())

use_lockfile <- switch(
  install_mode,
  auto = r_matches,
  lockfile = TRUE,
  current = FALSE,
  stop(
    'install_mode must be "auto", "lockfile", or "current".'
  )
)

if (use_lockfile) {

  if (!r_matches) {
    warning(
      "renv.lock was created with R ",
      lock_r,
      ", but the current R version is ",
      getRversion(),
      "."
    )
  }

  if (
    .Platform$OS.type == "windows" &&
      Sys.which("make") == ""
  ) {
    warning(
      "Rtools was not found. Source package installation may fail."
    )
  }

  renv::restore(
    prompt = FALSE
  )

} else {

  message(
    "Installing current versions of: ",
    paste(
      required_pkgs,
      collapse = ", "
    )
  )

  renv::install(
    required_pkgs,
    prompt = FALSE
  )
}

# -------------------------------------------------------------------------
# Project folders
# -------------------------------------------------------------------------

dirs <- c(
  "data/derived",
  "results",
  "figures"
)

for (d in dirs) {
  dir.create(
    d,
    recursive = TRUE,
    showWarnings = FALSE
  )
}

# -------------------------------------------------------------------------
# Source data check
# -------------------------------------------------------------------------

source_data_dir <- file.path(
  "data",
  "simulated-dataset-2"
)

if (!dir.exists(source_data_dir)) {
  warning(
    "Source dataset not found at: ",
    source_data_dir
  )
} else {
  message(
    "Setup complete: packages available and project folders ready."
  )
}