# Decode target vs non-target trials from EEG

library(tidyverse)
library(rsample)
library(yardstick)
library(glmnet)

# -------------------------------------------------------------------------
# Load cleaned data
# -------------------------------------------------------------------------

# Path to the cleaned, baseline-corrected trial-level dataset
clean_file <- "data/derived/sub-30_ses-01_clean.csv"

# Load the cleaned data
dat <- read_csv(clean_file)


# -------------------------------------------------------------------------
# Prepare outcome labels
# -------------------------------------------------------------------------

# Convert the logical target variable (FALSE / TRUE) into a factor with
# descriptive class labels for classification
dat <- dat |>
  mutate(
    is_target = factor(
      is_target,
      levels = c(FALSE, TRUE),
      labels = c("non_target", "target")
    )
  )

# Check that the two classes are balanced
table(dat$is_target)


# -------------------------------------------------------------------------
# Define time windows
# -------------------------------------------------------------------------

# Divide the EEG epoch into consecutive 50-ms windows.
# The original epoch runs from -100 to 800 ms.
#
# Using start points from -100 to 750 gives 18 complete windows:
# -100:-51, -50:-1, 0:49, ..., 750:799 ms.
#
# The final 800-ms sample is left out so that every window contains
# exactly 50 EEG samples.
window_starts <- seq(-100, 750, by = 50)


# -------------------------------------------------------------------------
# Create cross-validation folds
# -------------------------------------------------------------------------

# Give each trial a row ID so that the same trials can be selected from
# different EEG time windows later
cv_data <- dat |>
  mutate(row_id = row_number()) |>
  select(row_id, is_target)

# Fix the random seed so that the cross-validation split is reproducible
set.seed(261014)

# Divide the trials into five folds.
# Stratification keeps approximately the same proportion of target and
# non-target trials in each fold.
#
# These folds are created only once and reused for every time window.
# This makes decoding performance more comparable across time.
folds <- vfold_cv(
  cv_data,
  v = 5,
  strata = is_target
)

# Inspect the cross-validation structure
folds


# -------------------------------------------------------------------------
# Define decoding function
# -------------------------------------------------------------------------

# This function performs the complete decoding analysis for one 50-ms
# time window and returns its balanced classification accuracy.
decode_window <- function(window_start) {

  # Define the final sample of the current 50-ms window
  window_end <- window_start + 49

  # Construct the EEG column names corresponding to this time window.
  # For example, window_start = 350 produces:
  # t_350ms, t_351ms, ..., t_399ms
  window_cols <- paste0(
    "t_",
    window_start:window_end,
    "ms"
  )

  # Check that all requested EEG columns exist in the dataset
  stopifnot(all(window_cols %in% names(dat)))

  # Select the 50 EEG samples that will be used as predictors
  # for this decoding window
  x <- dat |>
    select(all_of(window_cols))

  # -----------------------------------------------------------------------
  # Train and test the classifier across the five folds
  # -----------------------------------------------------------------------

  results <- map_dfr(
    folds$splits,
    function(split) {

      # Get the trial IDs belonging to the training and test portions
      # of the current fold
      train_ids <- analysis(split)$row_id
      test_ids <- assessment(split)$row_id

      # Convert the EEG predictors to matrices because glmnet expects
      # matrix input rather than a tibble
      train_x <- x[train_ids, ] |>
        as.matrix()

      test_x <- x[test_ids, ] |>
        as.matrix()

      # Convert the training outcome into the numeric binary format
      # required by glmnet:
      # non_target = 0
      # target     = 1
      train_y <- if_else(
        dat$is_target[train_ids] == "target",
        1,
        0
      )

      # Fit ridge-regularized logistic regression.
      #
      # family = "binomial" specifies binary classification.
      # alpha = 0 specifies ridge regularization.
      #
      # cv.glmnet() performs internal cross-validation on the training
      # data to choose the regularization parameter lambda.
      model <- cv.glmnet(
        x = train_x,
        y = train_y,
        family = "binomial",
        alpha = 0
      )

      # Predict the probability that each held-out trial is a target.
      #
      # lambda.min is the value of lambda giving the lowest
      # cross-validation error within the training data.
      probs <- predict(
        model,
        newx = test_x,
        s = "lambda.min",
        type = "response"
      ) |>
        as.numeric()

      # Convert predicted probabilities into class labels.
      # Probabilities of 0.5 or higher are classified as targets.
      preds <- if_else(
        probs >= 0.5,
        "target",
        "non_target"
      )

      # Convert predictions to a factor with the same class ordering
      # as the true outcome variable
      preds <- factor(
        preds,
        levels = levels(dat$is_target)
      )

      # Store the true and predicted labels for this fold
      tibble(
        truth = dat$is_target[test_ids],
        estimate = preds
      )
    }
  )

  # -----------------------------------------------------------------------
  # Calculate decoding accuracy for this time window
  # -----------------------------------------------------------------------

  # Balanced accuracy is the average classification performance across
  # the two classes. Chance performance for this binary classification
  # problem is approximately 0.50.
  accuracy <- bal_accuracy(
    results,
    truth = truth,
    estimate = estimate
  ) |>
    pull(.estimate)

  # Return one row describing this time window and its decoding result
  tibble(
    window_start = window_start,
    window_end = window_end,
    window_midpoint = (window_start + window_end) / 2,
    balanced_accuracy = accuracy
  )
}


# -------------------------------------------------------------------------
# Decode target status across the full EEG epoch
# -------------------------------------------------------------------------

# Apply the decoding function to every 50-ms window and combine the
# resulting rows into one table
decoding_results <- map_dfr(
  window_starts,
  decode_window
)

# Inspect the time-resolved decoding results
decoding_results


# -------------------------------------------------------------------------
# Save decoding results
# -------------------------------------------------------------------------

# Save the derived decoding results so that later plotting scripts can
# use them without rerunning the classifier
output_file <- "data/derived/sub-30_ses-01_target_decoding.csv"

write_csv(
  decoding_results,
  output_file
)