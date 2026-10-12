#analysis.R

values <- c(4, 8, 15, 16, 23, 42, 100, 200, 300)

mean_value <- mean(values)
max_value <- max(values)
median_value <- median(values)
sd_value <- sd(values)

cat(sprintf("Mean: %.2f\n", mean_value))
cat(sprintf("Max: %d\n", max_value))
cat(sprintf("Median: %d\n", median_value))
cat(sprintf("Standard deviation: %.2f\n", sd_value))