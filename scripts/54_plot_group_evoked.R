# Plot group-level evoked EEG responses

library(tidyverse)
library(gsignal)

# -------------------------------------------------------------------------
# File paths
# -------------------------------------------------------------------------

results_dir <- "results"
output_dir <- "figures"

group_evoked_file <- file.path(
  results_dir,
  "group_evoked_by_nback.csv"
)

group_difference_file <- file.path(
  results_dir,
  "group_evoked_target_difference.csv"
)

stopifnot(
  file.exists(group_evoked_file),
  file.exists(group_difference_file)
)

# -------------------------------------------------------------------------
# Plot settings
# -------------------------------------------------------------------------

sampling_rate_hz <- 1000
lowpass_hz <- 10
filter_order <- 2L

# -------------------------------------------------------------------------
# Load group summaries
# -------------------------------------------------------------------------

group_evoked <- read_csv(
  group_evoked_file,
  col_types = cols(
    n_back = col_integer(),
    is_target = col_logical(),
    time_ms = col_integer(),
    n_participants = col_integer(),
    group_mean_amplitude_uv = col_double(),
    sd_amplitude_uv = col_double(),
    se_amplitude_uv = col_double(),
    ci_95_lower_uv = col_double(),
    ci_95_upper_uv = col_double()
  ),
  show_col_types = FALSE
)

group_difference <- read_csv(
  group_difference_file,
  col_types = cols(
    n_back = col_integer(),
    time_ms = col_integer(),
    n_participants = col_integer(),
    group_mean_difference_uv = col_double(),
    sd_difference_uv = col_double(),
    se_difference_uv = col_double(),
    ci_95_lower_uv = col_double(),
    ci_95_upper_uv = col_double()
  ),
  show_col_types = FALSE
)

# -------------------------------------------------------------------------
# Check input
# -------------------------------------------------------------------------

stopifnot(
  nrow(group_evoked) > 0,
  nrow(group_difference) > 0,
  setequal(group_evoked$n_back, 1:5),
  setequal(group_difference$n_back, 1:5),
  all(is.finite(group_evoked$group_mean_amplitude_uv)),
  all(is.finite(group_difference$group_mean_difference_uv))
)

# Check time sampling

time_points <- sort(
  unique(
    group_evoked$time_ms
  )
)

stopifnot(
  length(time_points) > 1,
  all(
    diff(time_points) ==
      1000 / sampling_rate_hz
  )
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

group_evoked <- group_evoked |>
  mutate(
    condition = factor(
      n_back,
      levels = 1:5,
      labels = paste0(
        1:5,
        "-back"
      )
    ),
    target_status = factor(
      is_target,
      levels = c(
        FALSE,
        TRUE
      ),
      labels = c(
        "Non-target",
        "Target"
      )
    )
  )

group_difference <- group_difference |>
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
# Low-pass filter for display
# -------------------------------------------------------------------------

display_filter <- butter(
  n = filter_order,
  w = lowpass_hz /
    (sampling_rate_hz / 2),
  type = "low"
)

filter_for_display <- function(
  x,
  time_ms
) {

  stopifnot(
    length(x) == length(time_ms),
    length(x) >
      3L * filter_order + 1L,
    all(is.finite(x)),
    all(
      diff(time_ms) ==
        1000 / sampling_rate_hz
    )
  )

  as.numeric(
    filtfilt(
      display_filter,
      x
    )
  )
}

# -------------------------------------------------------------------------
# Filter group evoked responses for display
# -------------------------------------------------------------------------

group_evoked_plot_data <- group_evoked |>
  arrange(
    n_back,
    is_target,
    time_ms
  ) |>
  group_by(
    n_back,
    is_target,
    condition,
    target_status
  ) |>
  mutate(
    display_mean_amplitude_uv =
      filter_for_display(
        group_mean_amplitude_uv,
        time_ms
      )
  ) |>
  ungroup()

# -------------------------------------------------------------------------
# Filter group difference waves for display
# -------------------------------------------------------------------------

group_difference_plot_data <- group_difference |>
  arrange(
    n_back,
    time_ms
  ) |>
  group_by(
    n_back,
    condition
  ) |>
  mutate(
    display_mean_difference_uv =
      filter_for_display(
        group_mean_difference_uv,
        time_ms
      )
  ) |>
  ungroup()

# -------------------------------------------------------------------------
# Prepare confidence intervals
# -------------------------------------------------------------------------

evoked_ci_data <- group_evoked |>
  dplyr::filter(
    is.finite(ci_95_lower_uv),
    is.finite(ci_95_upper_uv)
  )

difference_ci_data <- group_difference |>
  dplyr::filter(
    is.finite(ci_95_lower_uv),
    is.finite(ci_95_upper_uv)
  )

# -------------------------------------------------------------------------
# Shared plot settings
# -------------------------------------------------------------------------

filter_caption <- str_wrap(
  paste(
    "Display only:",
    paste0(
      lowpass_hz,
      "-Hz Butterworth low-pass filter,"
    ),
    paste0(
      "order ",
      filter_order,
      " per pass, applied forward and backward."
    )
  ),
  width = 110
)

plot_theme <- theme_minimal(
  base_size = 12
) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title.position = "plot",
    plot.caption = element_text(
      hjust = 0,
      margin = margin(
        t = 10
      )
    ),
    plot.margin = margin(
      10,
      20,
      15,
      20
    ),
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
# Plot group evoked responses
# -------------------------------------------------------------------------

group_evoked_plot <- ggplot(
  group_evoked_plot_data,
  aes(
    x = time_ms,
    y = display_mean_amplitude_uv,
    colour = condition
  )
) +
  geom_hline(
    yintercept = 0,
    colour = "grey75",
    linewidth = 0.4
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "grey50",
    linewidth = 0.4
  ) +
  geom_line(
    linewidth = 0.8
  ) +
  facet_wrap(
    ~ target_status,
    nrow = 1
  ) +
  scale_colour_manual(
    values = condition_colours
  ) +
  scale_x_continuous(
    breaks = seq(
      -100,
      800,
      by = 100
    )
  ) +
  labs(
    title = "Group evoked EEG responses across n-back levels",
    x = "Time relative to stimulus onset (ms)",
    y = "Mean amplitude (µV)",
    colour = "Condition",
    caption = filter_caption
  ) +
  plot_theme

print(group_evoked_plot)

# -------------------------------------------------------------------------
# Plot group target-minus-non-target difference waves
# -------------------------------------------------------------------------

group_difference_plot <- ggplot(
  group_difference_plot_data,
  aes(
    x = time_ms,
    y = display_mean_difference_uv,
    colour = condition
  )
) +
  geom_hline(
    yintercept = 0,
    colour = "grey75",
    linewidth = 0.4
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "grey50",
    linewidth = 0.4
  ) +
  geom_line(
    linewidth = 0.8
  ) +
  scale_colour_manual(
    values = condition_colours
  ) +
  scale_x_continuous(
    breaks = seq(
      -100,
      800,
      by = 100
    )
  ) +
  labs(
    title = "Group target-minus-non-target EEG responses",
    x = "Time relative to stimulus onset (ms)",
    y = "Amplitude difference (µV)",
    colour = "Condition",
    caption = str_wrap(
      paste(
        "Positive values indicate a more positive response",
        "for targets than non-targets.",
        filter_caption
      ),
      width = 100
    )
  ) +
  plot_theme

print(group_difference_plot)

# -------------------------------------------------------------------------
# Save figures
# -------------------------------------------------------------------------

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

group_evoked_plot_file <- file.path(
  output_dir,
  "group_evoked_by_nback.png"
)

group_difference_plot_file <- file.path(
  output_dir,
  "group_evoked_target_difference.png"
)

ggsave(
  filename = group_evoked_plot_file,
  plot = group_evoked_plot,
  width = 11,
  height = 5.5,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = group_difference_plot_file,
  plot = group_difference_plot,
  width = 10,
  height = 5.5,
  units = "in",
  dpi = 300,
  bg = "white"
)

stopifnot(
  file.exists(group_evoked_plot_file),
  file.exists(group_difference_plot_file)
)

cat(
  "Saved group evoked figures.\n"
)