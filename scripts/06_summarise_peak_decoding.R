# Summarise peak target decoding latency by n-back level

library(tidyverse)

# -------------------------------------------------------------------------
# Settings
# -------------------------------------------------------------------------

participant_id <- "sub-30"
session_id <- "ses-01"

peak_search_start_ms <- 200
peak_search_end_ms <- 500

file_prefix <- paste0(
  participant_id,
  "_",
  session_id
)

input_file <- file.path(
  "results",
  paste0(
    file_prefix,
    "_target_decoding_by_nback.csv"
  )
)

output_file <- file.path(
  "results",
  paste0(
    file_prefix,
    "_peak_decoding_by_nback.csv"
  )
)


# -------------------------------------------------------------------------
# Load decoding results
# -------------------------------------------------------------------------

decoding_data <- read_csv(
  input_file,
  show_col_types = FALSE
)


# -------------------------------------------------------------------------
# Check input
# -------------------------------------------------------------------------

required_cols <- c(
  "participant",
  "session",
  "n_back",
  "window_start",
  "window_end",
  "window_midpoint",
  "balanced_accuracy"
)

stopifnot(
  all(required_cols %in% names(decoding_data)),
  nrow(decoding_data) > 0,
  !anyNA(decoding_data[required_cols]),
  all(decoding_data$participant == participant_id),
  all(decoding_data$session == session_id),
  setequal(decoding_data$n_back, 1:5),
  all(is.finite(decoding_data$balanced_accuracy))
)

# Check that each n-back condition has the same number of time windows

window_count_check <- decoding_data |>
  count(
    n_back,
    name = "n_windows"
  )

stopifnot(
  n_distinct(window_count_check$n_windows) == 1
)


# -------------------------------------------------------------------------
# Restrict data to the peak-search interval
# -------------------------------------------------------------------------

# Peak latency is searched only within the predefined post-stimulus interval.
# This avoids selecting an accidental maximum from baseline or late periods
# that are not relevant to the target-related response of interest.

peak_search_data <- decoding_data |>
  filter(
    window_midpoint >= peak_search_start_ms,
    window_midpoint <= peak_search_end_ms
  )

stopifnot(
  nrow(peak_search_data) > 0
)

# Check that every n-back level is represented in the search interval

stopifnot(
  setequal(
    peak_search_data$n_back,
    1:5
  )
)


# -------------------------------------------------------------------------
# Extract peak decoding for each n-back level
# -------------------------------------------------------------------------

# For each n-back condition, select the time window with the highest
# balanced decoding accuracy.
#
# Because decoding is currently calculated in non-overlapping 50-ms
# windows, peak latency refers to the midpoint of the best-performing
# window rather than an exact millisecond-level neural latency.

peak_summary <- peak_search_data |>
  group_by(
    participant,
    session,
    n_back
  ) |>
  slice_max(
    order_by = balanced_accuracy,
    n = 1,
    with_ties = FALSE
  ) |>
  ungroup() |>
  transmute(
    participant,
    session,
    n_back,
    peak_window_start = window_start,
    peak_window_end = window_end,
    peak_latency_ms = window_midpoint,
    peak_accuracy = balanced_accuracy
  ) |>
  arrange(
    n_back
  )


# -------------------------------------------------------------------------
# Inspect peak summary
# -------------------------------------------------------------------------

print(peak_summary)


# -------------------------------------------------------------------------
# Calculate descriptive change in peak latency
# -------------------------------------------------------------------------

# This is descriptive only at the current single-participant stage.
# Group-level inference should be performed only after the analysis
# has been repeated across participants and sessions.

latency_change <- peak_summary |>
  summarise(
    earliest_peak_ms = min(peak_latency_ms),
    latest_peak_ms = max(peak_latency_ms),
    peak_latency_range_ms =
      latest_peak_ms - earliest_peak_ms
  )

print(latency_change)


# -------------------------------------------------------------------------
# Save peak summary
# -------------------------------------------------------------------------

dir.create(
  "results",
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  peak_summary,
  output_file
)