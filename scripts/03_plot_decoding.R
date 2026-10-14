# Plot time-resolved target decoding results

library(tidyverse)

# -------------------------------------------------------------------------
# Load decoding results
# -------------------------------------------------------------------------

decoding_file <- "data/derived/sub-30_ses-01_target_decoding.csv"
decoding_results <- read_csv(decoding_file)

# Inspect the results
decoding_results


# -------------------------------------------------------------------------
# Plot decoding accuracy
# -------------------------------------------------------------------------

decoding_plot <- ggplot(
  decoding_results,
  aes(
    x = window_midpoint,
    y = balanced_accuracy
  )
) +
  geom_hline(
    yintercept = 0.5,
    linetype = "dashed"
  ) +
  geom_line() +
  geom_point() +
  labs(
    x = "Time (ms)",
    y = "Balanced accuracy",
    title = "Time-resolved target decoding"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    )
  )


# -------------------------------------------------------------------------
# Save figure
# -------------------------------------------------------------------------

dir.create(
  "figures",
  showWarnings = FALSE
)

figure_file <- "figures/sub-30_ses-01_target_decoding.png"

ggsave(
  filename = figure_file,
  plot = decoding_plot,
  width = 7,
  height = 5,
  dpi = 300,
  bg = "white"
)