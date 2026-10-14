# Decode target vs non-target trials from EEG

library(tidyverse)
library(rsample)
library(yardstick)

# File path

clean_file <- "data/derived/sub-30_ses-01_clean.csv"

# Load cleaned data

dat <- read_csv(clean_file)

# Prepare outcome labels for classification

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

# Select EEG samples from one 50-ms window (350–399 ms)

window_cols <- paste0("t_", 350:399, "ms")

# Store EEG features

x <- dat |>
select(all_of(window_cols))

# Store target labels

y <- dat$is_target

# Check feature dimensions and class balance

dim(x)
table(y)

# Combine predictors and outcome for decoding

decode_data <- x |>
mutate(is_target = y)

# Make the cross-validation split reproducible

set.seed(261014)

# Divide data into 5 folds while preserving class proportions

folds <- vfold_cv(
decode_data,
v = 5,
strata = is_target
)

folds

# Train and test the classifier across the cross-validation folds

results <- map_dfr(
folds$splits,
function(split) {

# Training and test data for the current fold

train <- analysis(split)
test <- assessment(split)

# Fit logistic regression using EEG samples as predictors

model <- glm(
  is_target ~ .,
  data = train,
  family = binomial
)

# Predict target probability for held-out trials

probs <- predict(
  model,
  newdata = test,
  type = "response"
)

preds <- if_else(
  probs >= 0.5,
  "target",
  "non_target"
)


# Convert predictions to the same factor levels as the true labels

preds <- factor(
preds,
levels = levels(test$is_target)
)

# Store true and predicted labels

tibble(truth = test$is_target,
estimate = preds)
}
)

# Calculate balanced decoding accuracy

bal_accuracy(
results,
truth = truth,
estimate = estimate
)