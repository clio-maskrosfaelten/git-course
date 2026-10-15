# Clean and prepare the provided EEG and behavioural data

library(tidyverse)

# -------------------------------------------------------------------------
# Settings
# -------------------------------------------------------------------------

participant_id <- "sub-30"
session_id <- "ses-01"

file_prefix <- paste0(
  participant_id,
  "_",
  session_id
)

# -------------------------------------------------------------------------
# File paths
# -------------------------------------------------------------------------

behav_file <- file.path(
  "data",
  "simulated-dataset-2",
  participant_id,
  session_id,
  paste0(
    file_prefix,
    "_task-nback_behav.csv"
  )
)

eeg_file <- file.path(
  "data",
  "simulated-dataset-2",
  participant_id,
  session_id,
  paste0(
    file_prefix,
    "_task-nback_eeg.csv"
  )
)

output_file <- file.path(
  "data",
  "derived",
  paste0(
    file_prefix,
    "_clean.csv"
  )
)

# -------------------------------------------------------------------------
# Load data
# -------------------------------------------------------------------------

behav <- read_csv(
  behav_file,
  show_col_types = FALSE
)

eeg <- read_csv(
  eeg_file,
  show_col_types = FALSE
)

# -------------------------------------------------------------------------
# Check trial structure before joining
# -------------------------------------------------------------------------

stopifnot(
  nrow(behav) == nrow(eeg),
  n_distinct(behav$trial_id) == nrow(behav),
  n_distinct(eeg$trial_id) == nrow(eeg),
  setequal(behav$trial_id, eeg$trial_id)
)

# -------------------------------------------------------------------------
# Join behavioural and EEG data
# -------------------------------------------------------------------------

trial_data <- behav |>
  left_join(
    eeg,
    by = "trial_id"
  )

stopifnot(
  nrow(trial_data) == nrow(behav)
)

# Inspect the joined data
glimpse(trial_data)

# Check target distribution
trial_data |>
  count(is_target) |>
  print()

# -------------------------------------------------------------------------
# Identify EEG time-point columns
# -------------------------------------------------------------------------

eeg_cols <- names(trial_data) |>
  str_subset(
    "^t_-?\\d+ms$"
  )

head(eeg_cols)
tail(eeg_cols)

stopifnot(
  length(eeg_cols) > 0
)

# Check missing EEG values
stopifnot(
  sum(
    is.na(
      trial_data[eeg_cols]
    )
  ) == 0
)

# -------------------------------------------------------------------------
# Baseline correction
# -------------------------------------------------------------------------

# Use the pre-stimulus interval from -100 to 0 ms
baseline_cols <- eeg_cols |>
  keep(
    ~ {
      time <- as.numeric(
        str_extract(
          .x,
          "-?\\d+"
        )
      )

      time >= -100 &
        time <= 0
    }
  )

stopifnot(
  length(baseline_cols) > 0
)

# Calculate the mean pre-stimulus amplitude for each trial
baseline_mean <- trial_data |>
  select(
    all_of(baseline_cols)
  ) |>
  rowMeans()

# Inspect baseline offsets before correction
summary(baseline_mean)

# Subtract each trial's baseline mean from all EEG time points
trial_data[eeg_cols] <- sweep(
  trial_data[eeg_cols],
  1,
  baseline_mean,
  FUN = "-"
)

# Verify that baseline means are approximately zero after correction
baseline_mean_corrected <- trial_data |>
  select(
    all_of(baseline_cols)
  ) |>
  rowMeans()

summary(
  baseline_mean_corrected
)

# -------------------------------------------------------------------------
# Save cleaned data
# -------------------------------------------------------------------------

dir.create(
  "data/derived",
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  trial_data,
  output_file
)

stopifnot(
  file.exists(output_file)
)

cat(
  "Saved cleaned data to:",
  output_file,
  "\n"
)