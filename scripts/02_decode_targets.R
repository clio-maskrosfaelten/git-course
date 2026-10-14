# Decode target vs non-target trials from EEG

library(tidyverse)
library(rsample)
library(yardstick)
library(glmnet)

# -------------------------------------------------------------------------
# Load cleaned data
# -------------------------------------------------------------------------

clean_file <- "data/derived/sub-30_ses-01_clean.csv"

dat <- read_csv(clean_file)

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

# Check class balance
table(dat$is_target)

# -------------------------------------------------------------------------
# Select EEG features from one 50-ms window
# -------------------------------------------------------------------------

window_cols <- paste0("t_", 350:399, "ms")

x <- dat |>
  select(all_of(window_cols))

y <- dat$is_target

# Check feature dimensions and class balance
dim(x)
table(y)

# Combine predictors and outcome
decode_data <- x |>
  mutate(is_target = y)

# -------------------------------------------------------------------------
# Create cross-validation folds
# -------------------------------------------------------------------------

set.seed(261014)

folds <- vfold_cv(
  decode_data,
  v = 5,
  strata = is_target
)

folds

# -------------------------------------------------------------------------
# Train and test the classifier
# -------------------------------------------------------------------------

results <- map_dfr(
  folds$splits,
  function(split) {

    # Split the current fold into training and test data
    train <- analysis(split)
    test <- assessment(split)

    # Prepare predictor matrices and binary training labels
    train_x <- train |>
      select(-is_target) |>
      as.matrix()

    train_y <- if_else(
      train$is_target == "target",
      1,
      0
    )

    test_x <- test |>
      select(-is_target) |>
      as.matrix()

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

    # Convert probabilities into class predictions
    preds <- if_else(
      probs >= 0.5,
      "target",
      "non_target"
    )

    preds <- factor(
      preds,
      levels = levels(test$is_target)
    )

    # Store true and predicted labels
    tibble(
      truth = test$is_target,
      estimate = preds
    )
  }
)

# -------------------------------------------------------------------------
# Evaluate decoding performance
# -------------------------------------------------------------------------

bal_accuracy(
  results,
  truth = truth,
  estimate = estimate
)