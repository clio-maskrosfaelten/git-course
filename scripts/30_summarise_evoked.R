# Summarise evoked EEG responses

library(tidyverse)

# -------------------------------------------------------------------------
# Check pipeline settings
# -------------------------------------------------------------------------

stopifnot(
  exists("participant_id"),
  exists("session_id")
)

file_prefix <- paste0(
  participant_id,
  "_",
  session_id
)

# -------------------------------------------------------------------------
# Input file
# -------------------------------------------------------------------------

input_file <- file.path(
  "data",
  "derived",
  paste0(
    file_prefix,
    "_clean.csv"
  )
)

stopifnot(
  file.exists(input_file)
)

# -------------------------------------------------------------------------
# Load cleaned trial-level data
# -------------------------------------------------------------------------

trial_data <- read_csv(
  input_file,
  col_types = cols(
    trial_id = col_character(),
    n_back = col_integer(),
    is_target = col_logical(),
    .default = col_guess()
  ),
  show_col_types = FALSE
)

# -------------------------------------------------------------------------
# Check input data
# -------------------------------------------------------------------------

required_cols <- c(
  "trial_id",
  "n_back",
  "is_target"
)

stopifnot(
  all(required_cols %in% names(trial_data)),
  nrow(trial_data) > 0,
  !anyNA(trial_data$trial_id),
  n_distinct(trial_data$trial_id) == nrow(trial_data),
  !anyNA(trial_data$n_back),
  setequal(trial_data$n_back, 1:5),
  !anyNA(trial_data$is_target)
)

# Identify EEG columns from their time-point names

eeg_cols <- names(trial_data) |>
  str_subset(
    "^t_-?\\d+ms$"
  )

stopifnot(
  length(eeg_cols) > 0,
  all(
    map_lgl(
      trial_data[eeg_cols],
      is.numeric
    )
  ),
  all(
    map_lgl(
      trial_data[eeg_cols],
      ~ all(is.finite(.x))
    )
  )
)

# Add participant and session identifiers

trial_data <- trial_data |>
  mutate(
    participant = participant_id,
    session = session_id,
    .before = 1
  )

# -------------------------------------------------------------------------
# Convert EEG samples to long format
# -------------------------------------------------------------------------

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
    names_transform = list(
      time_ms = as.integer
    ),
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
# Save evoked summary
# -------------------------------------------------------------------------

output_dir <- "results"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

output_file <- file.path(
  output_dir,
  paste0(
    file_prefix,
    "_evoked_summary.csv"
  )
)

write_csv(
  evoked_summary,
  output_file
)

stopifnot(
  file.exists(output_file)
)

cat(
  "Saved evoked summary to:",
  output_file,
  "\n"
)