# Summarise behavioural performance

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
    response_given = col_logical(),
    correct = col_logical(),
    rt_ms = col_double(),
    .default = col_guess()
  ),
  na = c("", "NA"),
  show_col_types = FALSE
)

# -------------------------------------------------------------------------
# Check input data
# -------------------------------------------------------------------------

required_cols <- c(
  "trial_id",
  "n_back",
  "is_target",
  "response_given",
  "correct",
  "rt_ms"
)

stopifnot(
  all(required_cols %in% names(trial_data)),
  nrow(trial_data) > 0,
  !anyNA(trial_data$trial_id),
  n_distinct(trial_data$trial_id) == nrow(trial_data),
  !anyNA(trial_data$n_back),
  setequal(trial_data$n_back, 1:5),
  !anyNA(trial_data$is_target),
  !anyNA(trial_data$response_given),
  !anyNA(trial_data$correct)
)

# Add participant and session identifiers

trial_data <- trial_data |>
  mutate(
    participant = participant_id,
    session = session_id,
    .before = 1
  )

# -------------------------------------------------------------------------
# Count trials by n-back condition and target status
# -------------------------------------------------------------------------

trial_counts <- trial_data |>
  group_by(
    participant,
    session,
    n_back,
    is_target
  ) |>
  summarise(
    n_trials = n(),
    n_correct = sum(correct),
    n_incorrect = sum(!correct),
    .groups = "drop"
  ) |>
  arrange(
    participant,
    session,
    n_back,
    is_target
  )

print(trial_counts)

# -------------------------------------------------------------------------
# Summarise behavioural performance
# -------------------------------------------------------------------------

# Return NA when no valid observations are available

mean_or_na <- function(x) {

  if (
    length(x) == 0 ||
      all(is.na(x))
  ) {
    return(NA_real_)
  }

  mean(
    x,
    na.rm = TRUE
  )
}

behavioural_summary <- trial_data |>
  group_by(
    participant,
    session,
    n_back
  ) |>
  summarise(
    n_trials = n(),
    n_targets = sum(is_target),
    n_nontargets = sum(!is_target),

    accuracy = mean(correct),

    hit_rate = mean_or_na(
      response_given[is_target]
    ),

    false_alarm_rate = mean_or_na(
      response_given[!is_target]
    ),

    n_correct_target_rt = sum(
      is_target &
        correct &
        response_given &
        !is.na(rt_ms)
    ),

    mean_correct_target_rt_ms = mean_or_na(
      rt_ms[
        is_target &
          correct &
          response_given
      ]
    ),

    .groups = "drop"
  ) |>
  arrange(
    participant,
    session,
    n_back
  )

print(behavioural_summary)

# -------------------------------------------------------------------------
# Save behavioural summaries
# -------------------------------------------------------------------------

output_dir <- "results"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

trial_counts_file <- file.path(
  output_dir,
  paste0(
    file_prefix,
    "_trial_counts.csv"
  )
)

behavioural_summary_file <- file.path(
  output_dir,
  paste0(
    file_prefix,
    "_behavioural_summary.csv"
  )
)

write_csv(
  trial_counts,
  trial_counts_file
)

write_csv(
  behavioural_summary,
  behavioural_summary_file
)

stopifnot(
  file.exists(trial_counts_file),
  file.exists(behavioural_summary_file)
)

cat(
  "Saved behavioural summaries for",
  participant_id,
  session_id,
  "\n"
)