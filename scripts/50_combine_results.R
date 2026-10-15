# Combine participant- and session-level results

library(tidyverse)

# -------------------------------------------------------------------------
# File patterns
# -------------------------------------------------------------------------

results_dir <- "results"

behaviour_files <- list.files(
  results_dir,
  pattern = "^sub-.*_ses-.*_behavioural_summary\\.csv$",
  full.names = TRUE
)

evoked_files <- list.files(
  results_dir,
  pattern = "^sub-.*_ses-.*_evoked_summary\\.csv$",
  full.names = TRUE
)

decoding_files <- list.files(
  results_dir,
  pattern = "^sub-.*_ses-.*_target_decoding_by_nback\\.csv$",
  full.names = TRUE
)

peak_files <- list.files(
  results_dir,
  pattern = "^sub-.*_ses-.*_peak_decoding_by_nback\\.csv$",
  full.names = TRUE
)

# -------------------------------------------------------------------------
# Check input files
# -------------------------------------------------------------------------

stopifnot(
  length(behaviour_files) > 0,
  length(evoked_files) > 0,
  length(decoding_files) > 0,
  length(peak_files) > 0
)

cat(
  "Found",
  length(behaviour_files),
  "behaviour files,\n",
  length(evoked_files),
  "evoked files,\n",
  length(decoding_files),
  "decoding files, and\n",
  length(peak_files),
  "peak-decoding files.\n"
)

# -------------------------------------------------------------------------
# Column specifications
# -------------------------------------------------------------------------

behaviour_col_types <- cols(
  participant = col_character(),
  session = col_character(),
  n_back = col_integer(),
  n_trials = col_integer(),
  n_targets = col_integer(),
  n_nontargets = col_integer(),
  accuracy = col_double(),
  hit_rate = col_double(),
  false_alarm_rate = col_double(),
  n_correct_target_rt = col_integer(),
  mean_correct_target_rt_ms = col_double()
)

evoked_col_types <- cols(
  participant = col_character(),
  session = col_character(),
  n_back = col_integer(),
  is_target = col_logical(),
  time_ms = col_integer(),
  n_trials = col_integer(),
  mean_amplitude_uv = col_double(),
  sd_amplitude_uv = col_double()
)

decoding_col_types <- cols(
  participant = col_character(),
  session = col_character(),
  n_back = col_integer(),
  window_start = col_integer(),
  window_end = col_integer(),
  window_midpoint = col_double(),
  balanced_accuracy = col_double()
)

peak_col_types <- cols(
  participant = col_character(),
  session = col_character(),
  n_back = col_integer(),
  peak_window_start = col_integer(),
  peak_window_end = col_integer(),
  peak_latency_ms = col_double(),
  peak_accuracy = col_double(),
  n_peak_windows = col_integer()
)

# -------------------------------------------------------------------------
# Combine behavioural summaries
# -------------------------------------------------------------------------

behaviour_all <- behaviour_files |>
  map_dfr(
    ~ read_csv(
      .x,
      col_types = behaviour_col_types,
      show_col_types = FALSE
    )
  ) |>
  arrange(
    participant,
    session,
    n_back
  )

stopifnot(
  nrow(behaviour_all) > 0,
  !anyNA(
    behaviour_all[
      c(
        "participant",
        "session",
        "n_back"
      )
    ]
  )
)

# -------------------------------------------------------------------------
# Combine evoked summaries
# -------------------------------------------------------------------------

evoked_all <- evoked_files |>
  map_dfr(
    ~ read_csv(
      .x,
      col_types = evoked_col_types,
      show_col_types = FALSE
    )
  ) |>
  arrange(
    participant,
    session,
    n_back,
    is_target,
    time_ms
  )

stopifnot(
  nrow(evoked_all) > 0,
  !anyNA(
    evoked_all[
      c(
        "participant",
        "session",
        "n_back",
        "is_target",
        "time_ms"
      )
    ]
  )
)

# -------------------------------------------------------------------------
# Combine decoding time courses
# -------------------------------------------------------------------------

decoding_all <- decoding_files |>
  map_dfr(
    ~ read_csv(
      .x,
      col_types = decoding_col_types,
      show_col_types = FALSE
    )
  ) |>
  arrange(
    participant,
    session,
    n_back,
    window_midpoint
  )

stopifnot(
  nrow(decoding_all) > 0,
  !anyNA(
    decoding_all[
      c(
        "participant",
        "session",
        "n_back",
        "window_midpoint",
        "balanced_accuracy"
      )
    ]
  )
)

# -------------------------------------------------------------------------
# Combine peak decoding summaries
# -------------------------------------------------------------------------

peak_all <- peak_files |>
  map_dfr(
    ~ read_csv(
      .x,
      col_types = peak_col_types,
      show_col_types = FALSE
    )
  ) |>
  arrange(
    participant,
    session,
    n_back
  )

stopifnot(
  nrow(peak_all) > 0,
  !anyNA(
    peak_all[
      c(
        "participant",
        "session",
        "n_back",
        "peak_latency_ms",
        "peak_accuracy"
      )
    ]
  )
)

# -------------------------------------------------------------------------
# Check participant-session coverage
# -------------------------------------------------------------------------

behaviour_units <- behaviour_all |>
  distinct(
    participant,
    session
  )

evoked_units <- evoked_all |>
  distinct(
    participant,
    session
  )

decoding_units <- decoding_all |>
  distinct(
    participant,
    session
  )

peak_units <- peak_all |>
  distinct(
    participant,
    session
  )

stopifnot(
  setequal(
    behaviour_units,
    evoked_units
  ),
  setequal(
    behaviour_units,
    decoding_units
  ),
  setequal(
    behaviour_units,
    peak_units
  )
)

# -------------------------------------------------------------------------
# Save combined results
# -------------------------------------------------------------------------

behaviour_output <- file.path(
  results_dir,
  "group_behavioural_summary.csv"
)

evoked_output <- file.path(
  results_dir,
  "group_evoked_summary.csv"
)

decoding_output <- file.path(
  results_dir,
  "group_target_decoding_by_nback.csv"
)

peak_output <- file.path(
  results_dir,
  "group_peak_decoding_by_nback.csv"
)

write_csv(
  behaviour_all,
  behaviour_output
)

write_csv(
  evoked_all,
  evoked_output
)

write_csv(
  decoding_all,
  decoding_output
)

write_csv(
  peak_all,
  peak_output
)

stopifnot(
  file.exists(behaviour_output),
  file.exists(evoked_output),
  file.exists(decoding_output),
  file.exists(peak_output)
)

cat(
  "Combined results saved for",
  n_distinct(peak_all$participant),
  "participants and",
  nrow(peak_units),
  "participant-session combinations.\n"
)