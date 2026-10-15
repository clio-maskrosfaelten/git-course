# Decode target vs non-target trials separately by n-back level

library(tidyverse)
library(rsample)
library(yardstick)
library(glmnet)

# -------------------------------------------------------------------------
# Settings
# -------------------------------------------------------------------------

participant_id <- "sub-30"
session_id <- "ses-01"

clean_file <- file.path(
  "data",
  "derived",
  paste0(
    participant_id,
    "_",
    session_id,
    "_clean.csv"
  )
)

output_file <- file.path(
  "results",
  paste0(
    participant_id,
    "_",
    session_id,
    "_target_decoding_by_nback.csv"
  )
)

window_starts <- seq(
  -100,
  750,
  by = 50
)

# Search for peak decoding later within this post-stimulus interval.
# This is not used during classification itself.
peak_search_start_ms <- 200
peak_search_end_ms <- 500

# -------------------------------------------------------------------------
# Load cleaned data
# -------------------------------------------------------------------------

dat <- read_csv(
  clean_file,
  show_col_types = FALSE
)

# -------------------------------------------------------------------------
# Prepare outcome labels
# -------------------------------------------------------------------------

dat <- dat |>
  mutate(
    is_target = factor(
      is_target,
      levels = c(FALSE, TRUE),
      labels = c("non_target", "target")
    )
  )

# -------------------------------------------------------------------------
# Check trial counts by condition
# -------------------------------------------------------------------------

trial_counts <- dat |>
  count(
    n_back,
    is_target,
    name = "n_trials"
  )

print(trial_counts)

# Check that all five n-back conditions are present
stopifnot(
  setequal(
    unique(dat$n_back),
    1:5
  )
)

# Check that each n-back condition contains both target classes
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

# -------------------------------------------------------------------------
# Define decoding function for one n-back level
# -------------------------------------------------------------------------

decode_nback <- function(n_back_level) {

  # Select trials from one n-back condition
  condition_data <- dat |>
    filter(
      n_back == n_back_level
    ) |>
    mutate(
      row_id = row_number()
    )

  # Check class balance within this condition
  condition_counts <- condition_data |>
    count(
      is_target,
      name = "n_trials"
    )

  print(
    condition_counts |>
      mutate(
        n_back = n_back_level,
        .before = 1
      )
  )

  # Five-fold CV requires at least five trials in each class
  stopifnot(
    min(
      condition_counts$n_trials
    ) >= 5
  )

  # Create one set of stratified folds for this n-back condition.
  # The same folds are reused across all time windows.
  cv_data <- condition_data |>
    select(
      row_id,
      is_target
    )

  folds <- vfold_cv(
    cv_data,
    v = 5,
    strata = is_target
  )

  # -----------------------------------------------------------------------
  # Define decoding function for one 50-ms window
  # -----------------------------------------------------------------------

  decode_window <- function(window_start) {

    window_end <- window_start + 49

    window_cols <- paste0(
      "t_",
      window_start:window_end,
      "ms"
    )

    # Check that the requested EEG columns exist
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

        train_ids <- analysis(split)$row_id
        test_ids <- assessment(split)$row_id

        train_x <- x[train_ids, ] |>
          as.matrix()

        test_x <- x[test_ids, ] |>
          as.matrix()

        train_y <- if_else(
          condition_data$is_target[train_ids] == "target",
          1,
          0
        )

        # Ridge-regularized logistic regression
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

        # Convert probabilities to class predictions
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

        tibble(
          truth = condition_data$is_target[test_ids],
          estimate = preds
        )
      }
    )

    # Calculate balanced accuracy for this time window
    accuracy <- bal_accuracy(
      fold_results,
      truth = truth,
      estimate = estimate
    ) |>
      pull(.estimate)

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

  # Apply decoding across the full epoch
  map_dfr(
    window_starts,
    decode_window
  )
}

# -------------------------------------------------------------------------
# Decode all n-back conditions
# -------------------------------------------------------------------------

set.seed(261015)

decoding_by_nback <- map_dfr(
  1:5,
  decode_nback
)

# -------------------------------------------------------------------------
# Inspect results
# -------------------------------------------------------------------------

print(decoding_by_nback)

# Preview peak decoding within the planned search interval
peak_preview <- decoding_by_nback |>
  filter(
    window_midpoint >= peak_search_start_ms,
    window_midpoint <= peak_search_end_ms
  ) |>
  group_by(
    participant,
    session,
    n_back
  ) |>
  slice_max(
    balanced_accuracy,
    n = 1,
    with_ties = FALSE
  ) |>
  ungroup()

print(peak_preview)

# -------------------------------------------------------------------------
# Save results
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