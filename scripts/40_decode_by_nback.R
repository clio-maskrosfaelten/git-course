# Decode target vs non-target trials separately by n-back level

library(tidyverse)
library(rsample)
library(yardstick)
library(glmnet)

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
# Analysis settings
# -------------------------------------------------------------------------

window_width_ms <- 50
window_step_ms <- 25
n_folds <- 5

window_starts <- seq(
  -100,
  750,
  by = window_step_ms
)

# -------------------------------------------------------------------------
# File paths
# -------------------------------------------------------------------------

input_file <- file.path(
  "data",
  "derived",
  paste0(
    file_prefix,
    "_clean.csv"
  )
)

output_file <- file.path(
  "results",
  paste0(
    file_prefix,
    "_target_decoding_by_nback.csv"
  )
)

stopifnot(
  file.exists(input_file)
)

# -------------------------------------------------------------------------
# Load cleaned trial-level data
# -------------------------------------------------------------------------

dat <- read_csv(
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
  all(required_cols %in% names(dat)),
  nrow(dat) > 0,
  !anyNA(dat$trial_id),
  n_distinct(dat$trial_id) == nrow(dat),
  !anyNA(dat$n_back),
  setequal(dat$n_back, 1:5),
  !anyNA(dat$is_target)
)

# Identify EEG columns from their time-point names

eeg_cols <- names(dat) |>
  str_subset(
    "^t_-?\\d+ms$"
  )

stopifnot(
  length(eeg_cols) > 0,
  all(
    map_lgl(
      dat[eeg_cols],
      is.numeric
    )
  ),
  all(
    map_lgl(
      dat[eeg_cols],
      ~ all(is.finite(.x))
    )
  )
)

# -------------------------------------------------------------------------
# Prepare target labels
# -------------------------------------------------------------------------

dat <- dat |>
  mutate(
    is_target = factor(
      is_target,
      levels = c(
        FALSE,
        TRUE
      ),
      labels = c(
        "non_target",
        "target"
      )
    )
  )

# -------------------------------------------------------------------------
# Check trial counts by n-back level and target status
# -------------------------------------------------------------------------

trial_counts <- dat |>
  count(
    n_back,
    is_target,
    name = "n_trials"
  ) |>
  arrange(
    n_back,
    is_target
  )

print(trial_counts)

# Check that every n-back level contains both target classes

class_check <- trial_counts |>
  count(
    n_back,
    name = "n_classes"
  )

stopifnot(
  all(
    class_check$n_classes == 2
  )
)

# Check that each class contains enough trials for cross-validation

stopifnot(
  all(
    trial_counts$n_trials >= n_folds
  )
)

# -------------------------------------------------------------------------
# Define decoding for one n-back level
# -------------------------------------------------------------------------

decode_nback <- function(n_back_level) {

  # Select trials from the current n-back level

  condition_data <- dat |>
  dplyr::filter(
    n_back == n_back_level
  ) |>
  mutate(
    row_id = row_number()
  )

  # Create stratified cross-validation folds

  cv_data <- condition_data |>
    select(
      row_id,
      is_target
    )

  folds <- vfold_cv(
    cv_data,
    v = n_folds,
    strata = is_target
  )

  # -----------------------------------------------------------------------
  # Define decoding for one sliding time window
  # -----------------------------------------------------------------------

  decode_window <- function(window_start) {

    window_end <- window_start + window_width_ms - 1

    window_cols <- paste0(
      "t_",
      window_start:window_end,
      "ms"
    )

    # Check that all EEG samples in the current window are available

    stopifnot(
      all(
        window_cols %in% names(condition_data)
      )
    )

    # Select EEG samples used as predictors

    x <- condition_data |>
      select(
        all_of(window_cols)
      )

    # ---------------------------------------------------------------------
    # Train and test the classifier across folds
    # ---------------------------------------------------------------------

    fold_results <- map_dfr(
      folds$splits,
      function(split) {

        # Identify training and test trials

        train_ids <- analysis(split)$row_id
        test_ids <- assessment(split)$row_id

        # Convert EEG predictors to matrices

        train_x <- x[train_ids, ] |>
          as.matrix()

        test_x <- x[test_ids, ] |>
          as.matrix()

        # Convert training labels to binary values

        train_y <- if_else(
          condition_data$is_target[train_ids] == "target",
          1,
          0
        )

        # Fit ridge-regularized logistic regression

        model <- cv.glmnet(
          x = train_x,
          y = train_y,
          family = "binomial",
          alpha = 0
        )

        # Predict target probabilities for held-out trials

        probs <- predict(
          model,
          newx = test_x,
          s = "lambda.min",
          type = "response"
        ) |>
          as.numeric()

        # Convert probabilities to target-status predictions

        preds <- if_else(
          probs >= 0.5,
          "target",
          "non_target"
        )

        preds <- factor(
          preds,
          levels = levels(
            condition_data$is_target
          )
        )

        # Store true and predicted labels

        tibble(
          truth = condition_data$is_target[test_ids],
          estimate = preds
        )
      }
    )

    # Calculate balanced accuracy for the current window

    accuracy <- bal_accuracy(
      fold_results,
      truth = truth,
      estimate = estimate
    ) |>
      pull(.estimate)

    # Return decoding result for the current window

    tibble(
      participant = participant_id,
      session = session_id,
      n_back = n_back_level,
      window_start = window_start,
      window_end = window_end,
      window_midpoint =
        (window_start + window_end) / 2,
      balanced_accuracy = accuracy
    )
  }

  # Decode all sliding windows within the current n-back level

  map_dfr(
    window_starts,
    decode_window
  )
}

# -------------------------------------------------------------------------
# Decode all n-back levels
# -------------------------------------------------------------------------

set.seed(261015)

decoding_by_nback <- map_dfr(
  1:5,
  decode_nback
)

# -------------------------------------------------------------------------
# Check decoding results
# -------------------------------------------------------------------------

stopifnot(
  nrow(decoding_by_nback) ==
    length(window_starts) * 5,
  !anyNA(decoding_by_nback$balanced_accuracy),
  all(
    is.finite(
      decoding_by_nback$balanced_accuracy
    )
  ),
  all(
    decoding_by_nback$balanced_accuracy >= 0 &
      decoding_by_nback$balanced_accuracy <= 1
  )
)

print(decoding_by_nback)

# -------------------------------------------------------------------------
# Save decoding results
# -------------------------------------------------------------------------

dir.create(
  "results",
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  decoding_by_nback,
  output_file
)

stopifnot(
  file.exists(output_file)
)

cat(
  "Saved n-back decoding results to:",
  output_file,
  "\n"
)