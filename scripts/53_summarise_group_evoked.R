# Summarise group-level evoked EEG responses

library(tidyverse)

# -------------------------------------------------------------------------
# File paths
# -------------------------------------------------------------------------

results_dir <- "results"

input_file <- file.path(
  results_dir,
  "group_evoked_summary.csv"
)

stopifnot(
  file.exists(input_file)
)

# -------------------------------------------------------------------------
# Load combined evoked summaries
# -------------------------------------------------------------------------

evoked_all <- read_csv(
  input_file,
  col_types = cols(
    participant = col_character(),
    session = col_character(),
    n_back = col_integer(),
    is_target = col_logical(),
    time_ms = col_integer(),
    n_trials = col_integer(),
    mean_amplitude_uv = col_double(),
    sd_amplitude_uv = col_double()
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
  "is_target",
  "time_ms",
  "mean_amplitude_uv"
)

stopifnot(
  all(required_cols %in% names(evoked_all)),
  nrow(evoked_all) > 0,
  !anyNA(evoked_all[required_cols]),
  setequal(evoked_all$n_back, 1:5),
  all(is.finite(evoked_all$mean_amplitude_uv))
)

# Check that each participant, session, condition, target status,
# and time point has one summary row

evoked_keys <- evoked_all |>
  select(
    participant,
    session,
    n_back,
    is_target,
    time_ms
  )

stopifnot(
  anyDuplicated(evoked_keys) == 0
)

# -------------------------------------------------------------------------
# Average evoked responses across sessions within participant
# -------------------------------------------------------------------------

participant_evoked <- evoked_all |>
  group_by(
    participant,
    n_back,
    is_target,
    time_ms
  ) |>
  summarise(
    n_sessions =
      n_distinct(session),

    participant_mean_amplitude_uv =
      mean(mean_amplitude_uv),

    .groups = "drop"
  ) |>
  arrange(
    participant,
    n_back,
    is_target,
    time_ms
  )

# -------------------------------------------------------------------------
# Summarise evoked responses across participants
# -------------------------------------------------------------------------

group_evoked <- participant_evoked |>
  group_by(
    n_back,
    is_target,
    time_ms
  ) |>
  summarise(
    n_participants =
      n_distinct(participant),

    group_mean_amplitude_uv =
      mean(participant_mean_amplitude_uv),

    sd_amplitude_uv =
      if (
        n_distinct(participant) > 1
      ) {
        sd(participant_mean_amplitude_uv)
      } else {
        NA_real_
      },

    .groups = "drop"
  ) |>
  mutate(
    se_amplitude_uv = if_else(
      n_participants > 1,
      sd_amplitude_uv /
        sqrt(n_participants),
      NA_real_
    ),

    ci_95_lower_uv = if_else(
      n_participants > 1,
      group_mean_amplitude_uv -
        qt(
          0.975,
          df = n_participants - 1
        ) *
        se_amplitude_uv,
      NA_real_
    ),

    ci_95_upper_uv = if_else(
      n_participants > 1,
      group_mean_amplitude_uv +
        qt(
          0.975,
          df = n_participants - 1
        ) *
        se_amplitude_uv,
      NA_real_
    )
  ) |>
  arrange(
    n_back,
    is_target,
    time_ms
  )

# -------------------------------------------------------------------------
# Calculate participant-level target-minus-non-target difference waves
# -------------------------------------------------------------------------

participant_difference <- participant_evoked |>
  select(
    participant,
    n_back,
    time_ms,
    is_target,
    participant_mean_amplitude_uv
  ) |>
  mutate(
    target_status = if_else(
      is_target,
      "target",
      "non_target"
    )
  ) |>
  select(
    -is_target
  ) |>
  pivot_wider(
    names_from = target_status,
    values_from = participant_mean_amplitude_uv
  ) |>
  mutate(
    difference_uv =
      target - non_target
  ) |>
  arrange(
    participant,
    n_back,
    time_ms
  )

# -------------------------------------------------------------------------
# Summarise difference waves across participants
# -------------------------------------------------------------------------

group_difference <- participant_difference |>
  group_by(
    n_back,
    time_ms
  ) |>
  summarise(
    n_participants =
      n_distinct(participant),

    group_mean_difference_uv =
      mean(difference_uv),

    sd_difference_uv =
      if (
        n_distinct(participant) > 1
      ) {
        sd(difference_uv)
      } else {
        NA_real_
      },

    .groups = "drop"
  ) |>
  mutate(
    se_difference_uv = if_else(
      n_participants > 1,
      sd_difference_uv /
        sqrt(n_participants),
      NA_real_
    ),

    ci_95_lower_uv = if_else(
      n_participants > 1,
      group_mean_difference_uv -
        qt(
          0.975,
          df = n_participants - 1
        ) *
        se_difference_uv,
      NA_real_
    ),

    ci_95_upper_uv = if_else(
      n_participants > 1,
      group_mean_difference_uv +
        qt(
          0.975,
          df = n_participants - 1
        ) *
        se_difference_uv,
      NA_real_
    )
  ) |>
  arrange(
    n_back,
    time_ms
  )

# -------------------------------------------------------------------------
# Check summaries
# -------------------------------------------------------------------------

stopifnot(
  all(is.finite(participant_evoked$participant_mean_amplitude_uv)),
  all(is.finite(group_evoked$group_mean_amplitude_uv)),
  all(is.finite(participant_difference$difference_uv)),
  all(is.finite(group_difference$group_mean_difference_uv))
)

# -------------------------------------------------------------------------
# Save summaries
# -------------------------------------------------------------------------

participant_evoked_file <- file.path(
  results_dir,
  "participant_evoked_by_nback.csv"
)

group_evoked_file <- file.path(
  results_dir,
  "group_evoked_by_nback.csv"
)

participant_difference_file <- file.path(
  results_dir,
  "participant_evoked_target_difference.csv"
)

group_difference_file <- file.path(
  results_dir,
  "group_evoked_target_difference.csv"
)

write_csv(
  participant_evoked,
  participant_evoked_file
)

write_csv(
  group_evoked,
  group_evoked_file
)

write_csv(
  participant_difference,
  participant_difference_file
)

write_csv(
  group_difference,
  group_difference_file
)

stopifnot(
  file.exists(participant_evoked_file),
  file.exists(group_evoked_file),
  file.exists(participant_difference_file),
  file.exists(group_difference_file)
)

cat(
  "Saved group evoked summaries for",
  n_distinct(participant_evoked$participant),
  "participants.\n"
)