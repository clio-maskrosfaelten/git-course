# Plot group-level target decoding results

library(tidyverse)

# -------------------------------------------------------------------------
# File paths
# -------------------------------------------------------------------------

results_dir <- "results"
output_dir <- "figures"

group_decoding_file <- file.path(
  results_dir,
  "group_target_decoding_summary.csv"
)

participant_peaks_file <- file.path(
  results_dir,
  "participant_peak_decoding_by_nback.csv"
)

group_peaks_file <- file.path(
  results_dir,
  "group_peak_decoding_summary.csv"
)

stopifnot(
  file.exists(group_decoding_file),
  file.exists(participant_peaks_file),
  file.exists(group_peaks_file)
)

# -------------------------------------------------------------------------
# Load group summaries
# -------------------------------------------------------------------------

group_decoding <- read_csv(
  group_decoding_file,
  col_types = cols(
    n_back = col_integer(),
    window_start = col_integer(),
    window_end = col_integer(),
    window_midpoint = col_double(),
    n_participants = col_integer(),
    mean_balanced_accuracy = col_double(),
    sd_balanced_accuracy = col_double(),
    se_balanced_accuracy = col_double(),
    ci_95_lower = col_double(),
    ci_95_upper = col_double()
  ),
  show_col_types = FALSE
)

participant_peaks <- read_csv(
  participant_peaks_file,
  col_types = cols(
    participant = col_character(),
    n_back = col_integer(),
    n_sessions = col_integer(),
    mean_peak_latency_ms = col_double(),
    mean_peak_accuracy = col_double()
  ),
  show_col_types = FALSE
)

group_peaks <- read_csv(
  group_peaks_file,
  col_types = cols(
    n_back = col_integer(),
    n_participants = col_integer(),
    mean_peak_latency_ms = col_double(),
    sd_peak_latency_ms = col_double(),
    mean_peak_accuracy = col_double(),
    sd_peak_accuracy = col_double(),
    se_peak_latency_ms = col_double(),
    peak_latency_ci_95_lower = col_double(),
    peak_latency_ci_95_upper = col_double(),
    se_peak_accuracy = col_double(),
    peak_accuracy_ci_95_lower = col_double(),
    peak_accuracy_ci_95_upper = col_double()
  ),
  show_col_types = FALSE
)

# -------------------------------------------------------------------------
# Check input
# -------------------------------------------------------------------------

stopifnot(
  nrow(group_decoding) > 0,
  nrow(participant_peaks) > 0,
  nrow(group_peaks) > 0,
  setequal(group_decoding$n_back, 1:5),
  setequal(participant_peaks$n_back, 1:5),
  setequal(group_peaks$n_back, 1:5),
  all(is.finite(group_decoding$mean_balanced_accuracy)),
  all(is.finite(participant_peaks$mean_peak_latency_ms)),
  all(is.finite(group_peaks$mean_peak_latency_ms))
)

# -------------------------------------------------------------------------
# Prepare plotting variables
# -------------------------------------------------------------------------

condition_colours <- c(
  "1-back" = "#2C7BB6",
  "2-back" = "#74ADD1",
  "3-back" = "#AB8AC4",
  "4-back" = "#E0827A",
  "5-back" = "#D7191C"
)

group_decoding <- group_decoding |>
  mutate(
    condition = factor(
      n_back,
      levels = 1:5,
      labels = paste0(
        1:5,
        "-back"
      )
    )
  )

participant_peaks <- participant_peaks |>
  mutate(
    condition = factor(
      n_back,
      levels = 1:5,
      labels = paste0(
        1:5,
        "-back"
      )
    )
  )

group_peaks <- group_peaks |>
  mutate(
    condition = factor(
      n_back,
      levels = 1:5,
      labels = paste0(
        1:5,
        "-back"
      )
    )
  )

# -------------------------------------------------------------------------
# Shared plot settings
# -------------------------------------------------------------------------

plot_theme <- theme_minimal(
  base_size = 12
) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title.position = "plot",
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    panel.background = element_rect(
      fill = "white",
      colour = NA
    )
  )

# -------------------------------------------------------------------------
# Prepare confidence-interval data
# -------------------------------------------------------------------------

decoding_ci_data <- group_decoding |>
  dplyr::filter(
    is.finite(ci_95_lower),
    is.finite(ci_95_upper)
  )

peak_ci_data <- group_peaks |>
  dplyr::filter(
    is.finite(peak_latency_ci_95_lower),
    is.finite(peak_latency_ci_95_upper)
  )

# -------------------------------------------------------------------------
# Plot group decoding time course
# -------------------------------------------------------------------------

group_decoding_plot <- ggplot(
  group_decoding,
  aes(
    x = window_midpoint,
    y = mean_balanced_accuracy,
    colour = condition
  )
) +
  geom_hline(
    yintercept = 0.5,
    linetype = "dashed",
    colour = "grey50",
    linewidth = 0.4
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dotted",
    colour = "grey60",
    linewidth = 0.4
  ) +
  geom_ribbon(
    data = decoding_ci_data,
    aes(
      x = window_midpoint,
      ymin = ci_95_lower,
      ymax = ci_95_upper,
      fill = condition,
      group = condition
    ),
    inherit.aes = FALSE,
    alpha = 0.12,
    colour = NA
  ) +
  geom_line(
    linewidth = 0.9
  ) +
  scale_colour_manual(
    values = condition_colours
  ) +
  scale_fill_manual(
    values = condition_colours
  ) +
  scale_x_continuous(
    breaks = seq(
      -100,
      800,
      by = 100
    )
  ) +
  scale_y_continuous(
    limits = c(0, 1)
  ) +
  labs(
    title = "Group target decoding across n-back levels",
    x = "Time relative to stimulus onset (ms)",
    y = "Mean balanced accuracy",
    colour = "Condition"
  ) +
  guides(
    fill = "none"
  ) +
  plot_theme

print(group_decoding_plot)

# -------------------------------------------------------------------------
# Plot peak decoding latency
# -------------------------------------------------------------------------

peak_latency_plot <- ggplot() +

  # Participant-level peak latencies

  geom_point(
    data = participant_peaks,
    aes(
      x = n_back,
      y = mean_peak_latency_ms
    ),
    position = position_jitter(
      width = 0.06,
      height = 0
    ),
    colour = "grey65",
    size = 1.8,
    alpha = 0.45
  ) +

  # Group 95% confidence intervals

  geom_errorbar(
    data = peak_ci_data,
    aes(
      x = n_back,
      ymin = peak_latency_ci_95_lower,
      ymax = peak_latency_ci_95_upper,
      colour = condition
    ),
    width = 0.12,
    linewidth = 0.8
  ) +

  # Group mean trend

  geom_line(
    data = group_peaks,
    aes(
      x = n_back,
      y = mean_peak_latency_ms,
      group = 1
    ),
    colour = "grey30",
    linewidth = 1
  ) +

  # Group mean values

  geom_point(
    data = group_peaks,
    aes(
      x = n_back,
      y = mean_peak_latency_ms,
      colour = condition
    ),
    size = 3.4
  ) +

  scale_colour_manual(
    values = condition_colours
  ) +
  scale_x_continuous(
    breaks = 1:5
  ) +
  labs(
    title = "Peak target decoding latency across n-back levels",
    subtitle = "Points show participant means across sessions; coloured points show group means",
    x = "N-back level",
    y = "Peak decoding latency (ms)",
    colour = "Condition"
  ) +
  plot_theme

print(peak_latency_plot)

# -------------------------------------------------------------------------
# Save figures
# -------------------------------------------------------------------------

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

group_decoding_plot_file <- file.path(
  output_dir,
  "group_target_decoding_by_nback.png"
)

peak_latency_plot_file <- file.path(
  output_dir,
  "group_peak_decoding_latency_by_nback.png"
)

ggsave(
  filename = group_decoding_plot_file,
  plot = group_decoding_plot,
  width = 9,
  height = 5.5,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = peak_latency_plot_file,
  plot = peak_latency_plot,
  width = 8,
  height = 5.5,
  units = "in",
  dpi = 300,
  bg = "white"
)

stopifnot(
  file.exists(group_decoding_plot_file),
  file.exists(peak_latency_plot_file)
)

cat(
  "Saved group decoding figures.\n"
)