# Clean and prepare the provided EEG and behavioural data

library(tidyverse)

# -------------------------------------------------------------------------
# File paths
# -------------------------------------------------------------------------

behav_file <- "data/simulated-dataset-2/sub-30/ses-01/sub-30_ses-01_task-nback_behav.csv"
eeg_file <- "data/simulated-dataset-2/sub-30/ses-01/sub-30_ses-01_task-nback_eeg.csv"

# -------------------------------------------------------------------------
# Load data
# -------------------------------------------------------------------------

behav <- read_csv(behav_file)
eeg <- read_csv(eeg_file)

# -------------------------------------------------------------------------
# Check trial structure before joining
# -------------------------------------------------------------------------

stopifnot(nrow(behav) == nrow(eeg))
stopifnot(n_distinct(behav$trial_id) == nrow(behav))
stopifnot(n_distinct(eeg$trial_id) == nrow(eeg))
stopifnot(setequal(behav$trial_id, eeg$trial_id))

# -------------------------------------------------------------------------
# Join behavioural and EEG data
# -------------------------------------------------------------------------

trial_data <- behav |>
  left_join(eeg, by = "trial_id")

stopifnot(nrow(trial_data) == nrow(behav))

# Inspect the joined data
glimpse(trial_data)

# Check target distribution
trial_data |>
  count(is_target)

# Identify EEG time-point columns
eeg_cols <- names(trial_data) |>
  str_subset("^t_-?\\d+ms$")

head(eeg_cols)
tail(eeg_cols)

# Check missing EEG values
stopifnot(sum(is.na(trial_data[eeg_cols])) == 0)

# -------------------------------------------------------------------------
# Baseline correction
# -------------------------------------------------------------------------

# Use the pre-stimulus interval from -100 to 0 ms
baseline_cols <- eeg_cols |>
  keep(~ {
    time <- as.numeric(str_extract(.x, "-?\\d+"))
    time >= -100 & time <= 0
  })

# Calculate the mean pre-stimulus amplitude for each trial
baseline_mean <- trial_data |>
  select(all_of(baseline_cols)) |>
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
  select(all_of(baseline_cols)) |>
  rowMeans()

summary(baseline_mean_corrected)

# -------------------------------------------------------------------------
# Save cleaned data
# -------------------------------------------------------------------------

dir.create("data/derived", recursive = TRUE, showWarnings = FALSE)

output_file <- "data/derived/sub-30_ses-01_clean.csv"

write_csv(
  trial_data,
  output_file
)

stopifnot(file.exists(output_file))