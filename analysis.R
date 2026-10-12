#analysis.R

values <- c(4, 8, 15, 16, 23, 42)

mean_value <- mean(values)
max_value <- max(values)

cat(sprintf("Mean: %.2f\n", mean_value))
cat(sprintf("Max: %d\n", max_value))