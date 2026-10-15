# Summarise peak target decoding latency by n-back level

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
# Peak-search settings
# -------------------------------------------------------------------------

peak_search_start_ms <- 200
peak_search_end_ms <- 500

# -------------------------------------------------------------------------
# File paths
# -------------------------------------------------------------------------

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

stopifnot(
  file.exists(input_file)
)

# -------------------------------------------------------------------------
# Load decoding results
# -------------------------------------------------------------------------

decoding_data <- read_csv(
  input_file,
  col_types = cols(
    participant = col_character(),
    session = col_character(),
    n_back = col_integer(),
    window_start = col_integer(),
    window_end = col_integer(),
    window_midpoint = col_double(),
    balanced_accuracy = col_double()
  ),
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
  all(is.finite(decoding_data$balanced_accuracy)),
  all(
    decoding_data$balanced_accuracy >= 0 &
      decoding_data$balanced_accuracy <= 1
  )
)

# Check that each n-back level and time window has one result

decoding_keys <- decoding_data |>
  select(
    participant,
    session,
    n_back,
    window_midpoint
  )

stopifnot(
  anyDuplicated(decoding_keys) == 0
)

# Check that each n-back level contains the same number of windows

window_count_check <- decoding_data |>
  count(
    n_back,
    name = "n_windows"
  )

stopifnot(
  n_distinct(
    window_count_check$n_windows
  ) == 1
)

# Check that all decoding windows have the same width

window_widths <- decoding_data |>
  mutate(
    window_width =
      window_end - window_start + 1
  ) |>
  pull(window_width) |>
  unique()

stopifnot(
  length(window_widths) == 1
)

# Check that window starts progress at a constant step

window_structure <- decoding_data |>
  distinct(
    window_start,
    window_end,
    window_midpoint
  ) |>
  arrange(
    window_start
  )

window_steps <- diff(
  window_structure$window_start
)

stopifnot(
  length(window_steps) > 0,
  length(unique(window_steps)) == 1
)

# -------------------------------------------------------------------------
# Restrict data to the peak-search interval
# -------------------------------------------------------------------------

peak_search_data <- decoding_data |>
  dplyr::filter(
    window_midpoint >= peak_search_start_ms,
    window_midpoint <= peak_search_end_ms
  )

stopifnot(
  nrow(peak_search_data) > 0,
  setequal(
    peak_search_data$n_back,
    1:5
  )
)

# Check that each n-back level contains the same number of search windows

search_window_count <- peak_search_data |>
  count(
    n_back,
    name = "n_windows"
  )

stopifnot(
  n_distinct(
    search_window_count$n_windows
  ) == 1
)

# -------------------------------------------------------------------------
# Extract peak decoding for each n-back level
# -------------------------------------------------------------------------

# Identify all windows sharing the maximum balanced accuracy

peak_candidates <- peak_search_data |>
  group_by(
    participant,
    session,
    n_back
  ) |>
  dplyr::filter(
    balanced_accuracy ==
      max(balanced_accuracy)
  ) |>
  arrange(
    participant,
    session,
    n_back,
    window_midpoint
  )

# Count tied peak windows and retain the earliest maximum

peak_summary <- peak_candidates |>
  group_by(
    participant,
    session,
    n_back
  ) |>
  mutate(
    n_peak_windows = n()
  ) |>
  slice_head(
    n = 1
  ) |>
  ungroup() |>
  transmute(
    participant,
    session,
    n_back,
    peak_window_start = window_start,
    peak_window_end = window_end,
    peak_latency_ms = window_midpoint,
    peak_accuracy = balanced_accuracy,
    n_peak_windows
  ) |>
  arrange(
    n_back
  )

# -------------------------------------------------------------------------
# Check peak summary
# -------------------------------------------------------------------------

stopifnot(
  nrow(peak_summary) == 5,
  setequal(
    peak_summary$n_back,
    1:5
  ),
  all(
    peak_summary$peak_latency_ms >= peak_search_start_ms &
      peak_summary$peak_latency_ms <= peak_search_end_ms
  ),
  all(
    peak_summary$n_peak_windows >= 1
  )
)

# -------------------------------------------------------------------------
# Inspect peak summary
# -------------------------------------------------------------------------

print(peak_summary)

# -------------------------------------------------------------------------
# Summarise peak-latency range
# -------------------------------------------------------------------------

latency_change <- peak_summary |>
  summarise(
    earliest_peak_ms =
      min(peak_latency_ms),
    latest_peak_ms =
      max(peak_latency_ms),
    peak_latency_range_ms =
      latest_peak_ms -
      earliest_peak_ms
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

stopifnot(
  file.exists(output_file)
)

cat(
  "Saved peak decoding summary to:",
  output_file,
  "\n"
)