# clean and prepare the provided data 

library(tidyverse)

# file paths
behav_file <- "data/simulated-dataset-2/sub-30/ses-01/sub-30_ses-01_task-nback_behav.csv"
eeg_file <- "data/simulated-dataset-2/sub-30/ses-01/sub-30_ses-01_task-nback_eeg.csv"

# Load data
behav <- read_csv(behav_file)
eeg <- read_csv(eeg_file)

# Check trial structure
stopifnot(nrow(behav) == nrow(eeg))
stopifnot(n_distinct(behav$trial_id) == nrow(behav))
stopifnot(n_distinct(eeg$trial_id) == nrow(eeg))
stopifnot(setequal(behav$trial_id, eeg$trial_id))

# Join behavioural and EEG data
trial_data <- behav |>
  left_join(eeg, by = "trial_id")

glimpse(trial_data)
nrow(trial_data)

# Check target distribution
trial_data |>
  count(is_target)


names(trial_data) |>
  str_subset("^t_")|>
  head()

names(trial_data) |>
  str_subset("^t_") |>
  tail()

# Check if baseline is treated
# Extract baseline names, from -100 to 0 ms
baseline_cols <- names(trial_data) |>
  str_subset("^t_-?\\d+ms$") |>
  keep(~ {
    time <- as.numeric(str_extract(.x, "-?\\d+"))
    time >= -100 & time <= 0
  })

baseline_mean <- trial_data |>
  select(all_of(baseline_cols)) |>
  rowMeans()

summary(baseline_mean)


# Baseline correction
eeg_cols <- names(trial_data) |>
  str_subset("^t_-?\\d+ms$")

trial_data_bc <- trial_data

trial_data_bc[eeg_cols] <- sweep(  # for each trial, subtract the baseline mean from each EEG time point
  trial_data[eeg_cols],
  1,
  baseline_mean,
  FUN = "-"
)

baseline_mean_bc <- trial_data_bc |>
  select(all_of(baseline_cols)) |>
  rowMeans()

summary(baseline_mean_bc)

# Save cleaned data

dir.create("data/derived", showWarnings = FALSE)

trial_data <- trial_data_bc

write_csv(
  trial_data,
  "data/derived/sub-30_ses-01_clean.csv"
)

file.exists("data/derived/sub-30_ses-01_clean.csv")
