# Summarise group-level target decoding results

library(tidyverse)

# -------------------------------------------------------------------------
# File paths
# -------------------------------------------------------------------------

results_dir <- "results"

decoding_file <- file.path(
  results_dir,
  "group_target_decoding_by_nback.csv"
)

peak_file <- file.path(
  results_dir,
  "group_peak_decoding_by_nback.csv"
)

stopifnot(
  file.exists(decoding_file),
  file.exists(peak_file)
)

# -------------------------------------------------------------------------
# Load combined decoding results
# -------------------------------------------------------------------------

decoding_all <- read_csv(
  decoding_file,
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

peak_all <- read_csv(
  peak_file,
  col_types = cols(
    participant = col_character(),
    session = col_character(),
    n_back = col_integer(),
    peak_window_start = col_integer(),
    peak_window_end = col_integer(),
    peak_latency_ms = col_double(),
    peak_accuracy = col_double(),
    n_peak_windows = col_integer()
  ),
  show_col_types = FALSE
)

# -------------------------------------------------------------------------
# Check input
# -------------------------------------------------------------------------

stopifnot(
  nrow(decoding_all) > 0,
  nrow(peak_all) > 0,
  setequal(decoding_all$n_back, 1:5),
  setequal(peak_all$n_back, 1:5),
  all(is.finite(decoding_all$balanced_accuracy)),
  all(is.finite(peak_all$peak_latency_ms)),
  all(is.finite(peak_all$peak_accuracy)),
  all(
    decoding_all$balanced_accuracy >= 0 &
      decoding_all$balanced_accuracy <= 1
  ),
  all(
    peak_all$peak_accuracy >= 0 &
      peak_all$peak_accuracy <= 1
  )
)

# -------------------------------------------------------------------------
# Average decoding time courses across sessions within participant
# -------------------------------------------------------------------------

participant_decoding <- decoding_all |>
  group_by(
    participant,
    n_back,
    window_start,
    window_end,
    window_midpoint
  ) |>
  summarise(
    n_sessions = n_distinct(session),
    balanced_accuracy = mean(balanced_accuracy),
    .groups = "drop"
  ) |>
  arrange(
    participant,
    n_back,
    window_midpoint
  )

# -------------------------------------------------------------------------
# Summarise decoding time courses across participants
# -------------------------------------------------------------------------

group_decoding <- participant_decoding |>
  group_by(
    n_back,
    window_start,
    window_end,
    window_midpoint
  ) |>
  summarise(
    n_participants = n_distinct(participant),

    mean_balanced_accuracy =
      mean(balanced_accuracy),

    sd_balanced_accuracy =
      if (
        n_distinct(participant) > 1
      ) {
        sd(balanced_accuracy)
      } else {
        NA_real_
      },

    .groups = "drop"
  ) |>
  mutate(
    se_balanced_accuracy = if_else(
      n_participants > 1,
      sd_balanced_accuracy /
        sqrt(n_participants),
      NA_real_
    ),

    ci_95_lower = if_else(
      n_participants > 1,
      mean_balanced_accuracy -
        qt(
          0.975,
          df = n_participants - 1
        ) *
        se_balanced_accuracy,
      NA_real_
    ),

    ci_95_upper = if_else(
      n_participants > 1,
      mean_balanced_accuracy +
        qt(
          0.975,
          df = n_participants - 1
        ) *
        se_balanced_accuracy,
      NA_real_
    )
  ) |>
  arrange(
    n_back,
    window_midpoint
  )

# -------------------------------------------------------------------------
# Average peak measures across sessions within participant
# -------------------------------------------------------------------------

participant_peaks <- peak_all |>
  group_by(
    participant,
    n_back
  ) |>
  summarise(
    n_sessions = n_distinct(session),

    participant_peak_latency_ms =
      mean(peak_latency_ms),

    participant_peak_accuracy =
      mean(peak_accuracy),

    .groups = "drop"
  ) |>
  arrange(
    participant,
    n_back
  )

# -------------------------------------------------------------------------
# Summarise peak measures across participants
# -------------------------------------------------------------------------

group_peaks <- participant_peaks |>
  group_by(
    n_back
  ) |>
  summarise(
    n_participants =
      n_distinct(participant),

    mean_peak_latency_ms =
      mean(participant_peak_latency_ms),

    sd_peak_latency_ms =
      if (
        n_distinct(participant) > 1
      ) {
        sd(participant_peak_latency_ms)
      } else {
        NA_real_
      },

    mean_peak_accuracy =
      mean(participant_peak_accuracy),

    sd_peak_accuracy =
      if (
        n_distinct(participant) > 1
      ) {
        sd(participant_peak_accuracy)
      } else {
        NA_real_
      },

    .groups = "drop"
  ) |>
  mutate(
    se_peak_latency_ms = if_else(
      n_participants > 1,
      sd_peak_latency_ms /
        sqrt(n_participants),
      NA_real_
    ),

    peak_latency_ci_95_lower = if_else(
      n_participants > 1,
      mean_peak_latency_ms -
        qt(
          0.975,
          df = n_participants - 1
        ) *
        se_peak_latency_ms,
      NA_real_
    ),

    peak_latency_ci_95_upper = if_else(
      n_participants > 1,
      mean_peak_latency_ms +
        qt(
          0.975,
          df = n_participants - 1
        ) *
        se_peak_latency_ms,
      NA_real_
    ),

    se_peak_accuracy = if_else(
      n_participants > 1,
      sd_peak_accuracy /
        sqrt(n_participants),
      NA_real_
    ),

    peak_accuracy_ci_95_lower = if_else(
      n_participants > 1,
      mean_peak_accuracy -
        qt(
          0.975,
          df = n_participants - 1
        ) *
        se_peak_accuracy,
      NA_real_
    ),

    peak_accuracy_ci_95_upper = if_else(
      n_participants > 1,
      mean_peak_accuracy +
        qt(
          0.975,
          df = n_participants - 1
        ) *
        se_peak_accuracy,
      NA_real_
    )
  ) |>
  arrange(
    n_back
  )

# -------------------------------------------------------------------------
# Prepare participant-level peak output
# -------------------------------------------------------------------------

participant_peaks_output <- participant_peaks |>
  transmute(
    participant,
    n_back,
    n_sessions,
    mean_peak_latency_ms =
      participant_peak_latency_ms,
    mean_peak_accuracy =
      participant_peak_accuracy
  )

# -------------------------------------------------------------------------
# Check summaries
# -------------------------------------------------------------------------

stopifnot(
  all(is.finite(group_decoding$mean_balanced_accuracy)),
  all(is.finite(group_peaks$mean_peak_latency_ms)),
  all(is.finite(group_peaks$mean_peak_accuracy))
)

if (
  all(
    group_peaks$n_participants > 1
  )
) {
  stopifnot(
    all(is.finite(group_peaks$sd_peak_latency_ms)),
    all(is.finite(group_peaks$se_peak_latency_ms)),
    all(is.finite(group_peaks$peak_latency_ci_95_lower)),
    all(is.finite(group_peaks$peak_latency_ci_95_upper)),
    all(is.finite(group_peaks$sd_peak_accuracy)),
    all(is.finite(group_peaks$se_peak_accuracy)),
    all(is.finite(group_peaks$peak_accuracy_ci_95_lower)),
    all(is.finite(group_peaks$peak_accuracy_ci_95_upper))
  )
}

# -------------------------------------------------------------------------
# Inspect summaries
# -------------------------------------------------------------------------

print(group_peaks)

# -------------------------------------------------------------------------
# Save participant- and group-level summaries
# -------------------------------------------------------------------------

participant_decoding_file <- file.path(
  results_dir,
  "participant_target_decoding_by_nback.csv"
)

group_decoding_file <- file.path(
  results_dir,
  "group_target_decoding_summary.csv"
)

participant_peaks_file <- file.path(
  results_dir,
  "participant_peak_decoding_by_nback.csv"
)

group_peaks_file <- file.path(
  results_dir,
  "group_peak_decoding_summary.csv"
)

write_csv(
  participant_decoding,
  participant_decoding_file
)

write_csv(
  group_decoding,
  group_decoding_file
)

write_csv(
  participant_peaks_output,
  participant_peaks_file
)

write_csv(
  group_peaks,
  group_peaks_file
)

stopifnot(
  file.exists(participant_decoding_file),
  file.exists(group_decoding_file),
  file.exists(participant_peaks_file),
  file.exists(group_peaks_file)
)

cat(
  "Saved group decoding summaries for",
  n_distinct(participant_peaks$participant),
  "participants.\n"
)