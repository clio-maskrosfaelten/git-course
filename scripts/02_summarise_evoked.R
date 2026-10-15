# Summarise evoked EEG responses and behaviour for participant 30

library(tidyverse)

# -------------------------------------------------------------------------
# Settings and input
# -------------------------------------------------------------------------

participant_id <- "sub-30"
session_id <- "ses-01"

input_file <- file.path(
  "data",
  "derived",
  paste0(participant_id, "_", session_id, "_clean.csv")
)

trial_data <- read_csv(
  input_file,
  col_types = cols(
    trial_id = col_character(),
    n_back = col_integer(),
    is_target = col_logical(),
    response_given = col_logical(),
    correct = col_logical(),
    rt_ms = col_double(),
    .default = col_guess()
  ),
  na = c("", "NA")
)

# -------------------------------------------------------------------------
# Check the input
# -------------------------------------------------------------------------

required_cols <- c(
  "trial_id", "n_back", "is_target",
  "response_given", "correct", "rt_ms"
)

stopifnot(all(required_cols %in% names(trial_data)))
stopifnot(nrow(trial_data) > 0)
stopifnot(!anyNA(trial_data$trial_id))
stopifnot(n_distinct(trial_data$trial_id) == nrow(trial_data))
stopifnot(!anyNA(trial_data$n_back))
stopifnot(setequal(trial_data$n_back, 1:5))
stopifnot(!anyNA(trial_data$is_target))
stopifnot(!anyNA(trial_data$response_given))
stopifnot(!anyNA(trial_data$correct))

eeg_cols <- names(trial_data) |>
  str_subset("^t_-?\\d+ms$")

stopifnot(length(eeg_cols) > 0)
stopifnot(all(map_lgl(trial_data[eeg_cols], is.numeric)))
stopifnot(all(map_lgl(
  trial_data[eeg_cols],
  ~ all(is.finite(.x))
)))

trial_data <- trial_data |>
  mutate(
    participant = participant_id,
    session = session_id,
    .before = 1
  )

# -------------------------------------------------------------------------
# Count trials by condition and target status
# -------------------------------------------------------------------------

trial_counts <- trial_data |>
  group_by(participant, session, n_back, is_target) |>
  summarise(
    n_trials = n(),
    n_correct = sum(correct),
    n_incorrect = sum(!correct),
    .groups = "drop"
  ) |>
  arrange(participant, session, n_back, is_target)

print(trial_counts)

# -------------------------------------------------------------------------
# Summarise behavioural performance
# -------------------------------------------------------------------------

# Return NA when no observations are available.
mean_or_na <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }

  mean(x, na.rm = TRUE)
}

behavioural_summary <- trial_data |>
  group_by(participant, session, n_back) |>
  summarise(
    n_trials = n(),
    n_targets = sum(is_target),
    n_nontargets = sum(!is_target),
    accuracy = mean(correct),
    hit_rate = mean_or_na(response_given[is_target]),
    false_alarm_rate = mean_or_na(response_given[!is_target]),
    n_correct_target_rt = sum(
      is_target & correct & response_given & !is.na(rt_ms)
    ),
    mean_correct_target_rt_ms = mean_or_na(
      rt_ms[is_target & correct & response_given]
    ),
    .groups = "drop"
  ) |>
  arrange(participant, session, n_back)

print(behavioural_summary)

# -------------------------------------------------------------------------
# Convert EEG samples to long format
# -------------------------------------------------------------------------

# The input is already baseline-corrected by 01_clean_data.R.
# Retain both correct and incorrect trials for this primary summary.

eeg_long <- trial_data |>
  select(
    participant,
    session,
    trial_id,
    n_back,
    is_target,
    all_of(eeg_cols)
  ) |>
  pivot_longer(
    cols = all_of(eeg_cols),
    names_to = "time_ms",
    names_pattern = "^t_(-?\\d+)ms$",
    names_transform = list(time_ms = as.integer),
    values_to = "amplitude_uv"
  )

# -------------------------------------------------------------------------
# Calculate average evoked responses
# -------------------------------------------------------------------------

evoked_summary <- eeg_long |>
  group_by(
    participant,
    session,
    n_back,
    is_target,
    time_ms
  ) |>
  summarise(
    n_trials = n(),
    mean_amplitude_uv = mean(amplitude_uv),
    sd_amplitude_uv = sd(amplitude_uv),
    .groups = "drop"
  ) |>
  arrange(
    participant,
    session,
    n_back,
    is_target,
    time_ms
  )

# -------------------------------------------------------------------------
# Save summaries
# -------------------------------------------------------------------------

output_dir <- "results"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

file_prefix <- paste0(participant_id, "_", session_id)

write_csv(
  trial_counts,
  file.path(output_dir, paste0(file_prefix, "_trial_counts.csv"))
)

write_csv(
  behavioural_summary,
  file.path(output_dir, paste0(file_prefix, "_behavioural_summary.csv"))
)

write_csv(
  evoked_summary,
  file.path(output_dir, paste0(file_prefix, "_evoked_summary.csv"))
)