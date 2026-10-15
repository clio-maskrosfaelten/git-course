# Plot evoked EEG responses for one participant and session

library(tidyverse)
library(gsignal)

# -------------------------------------------------------------------------
# Settings
# -------------------------------------------------------------------------

participant_id <- "sub-30"
session_id <- "ses-01"

sampling_rate_hz <- 1000
lowpass_hz <- 10
filter_order <- 2L

file_prefix <- paste0(participant_id, "_", session_id)

input_file <- file.path(
  "results",
  paste0(file_prefix, "_evoked_summary.csv")
)

output_dir <- "figures"


# -------------------------------------------------------------------------
# Load evoked summary
# -------------------------------------------------------------------------

evoked_summary <- read_csv(
  input_file,
  col_types = cols(
    participant = col_character(),
    session = col_character(),
    n_back = col_integer(),
    is_target = col_logical(),
    time_ms = col_integer(),
    n_trials = col_integer(),
    mean_amplitude_uv = col_double(),
    sd_amplitude_uv = col_double()
  )
)


# -------------------------------------------------------------------------
# Check input
# -------------------------------------------------------------------------

required_cols <- c(
  "participant",
  "session",
  "n_back",
  "is_target",
  "time_ms",
  "n_trials",
  "mean_amplitude_uv",
  "sd_amplitude_uv"
)

stopifnot(
  all(required_cols %in% names(evoked_summary)),
  nrow(evoked_summary) > 0,
  !anyNA(evoked_summary[required_cols]),
  all(evoked_summary$participant == participant_id),
  all(evoked_summary$session == session_id),
  setequal(evoked_summary$n_back, 1:5),
  all(evoked_summary$n_trials > 0),
  all(is.finite(evoked_summary$mean_amplitude_uv)),
  all(is.finite(evoked_summary$sd_amplitude_uv))
)

# Each condition, target status, and time point should have one summary row
summary_keys <- evoked_summary |>
  select(
    participant,
    session,
    n_back,
    is_target,
    time_ms
  )

stopifnot(anyDuplicated(summary_keys) == 0)

# Check that the time axis is regularly sampled at 1000 Hz
time_points <- sort(unique(evoked_summary$time_ms))

stopifnot(
  length(time_points) > 1,
  all(diff(time_points) == 1000 / sampling_rate_hz)
)

# Check that all combinations of condition, target status, and time exist
expected_cells <- crossing(
  n_back = 1:5,
  is_target = c(FALSE, TRUE),
  time_ms = time_points
)

missing_cells <- expected_cells |>
  anti_join(
    evoked_summary,
    by = c("n_back", "is_target", "time_ms")
  )

stopifnot(nrow(missing_cells) == 0)

# The number of contributing trials should remain constant across time
trial_count_check <- evoked_summary |>
  group_by(n_back, is_target) |>
  summarise(
    n_trial_counts = n_distinct(n_trials),
    .groups = "drop"
  )

stopifnot(all(trial_count_check$n_trial_counts == 1))


# -------------------------------------------------------------------------
# Prepare plotting variables
# -------------------------------------------------------------------------

evoked_data <- evoked_summary |>
  mutate(
    condition = factor(
      n_back,
      levels = 1:5,
      labels = paste0(1:5, "-back")
    ),
    target_status = factor(
      is_target,
      levels = c(FALSE, TRUE),
      labels = c("Non-target", "Target")
    )
  )


# -------------------------------------------------------------------------
# Low-pass filter for display only
# -------------------------------------------------------------------------

# The filtering is applied only to the plotted waveforms.
# All numerical analyses remain based on the unfiltered summary data.

display_filter <- butter(
  n = filter_order,
  w = lowpass_hz / (sampling_rate_hz / 2),
  type = "low"
)

filter_for_display <- function(x, time_ms) {

  stopifnot(
    length(x) == length(time_ms),
    length(x) > 3L * filter_order + 1L,
    all(is.finite(x)),
    all(diff(time_ms) == 1000 / sampling_rate_hz)
  )

  as.numeric(
    filtfilt(display_filter, x)
  )
}

evoked_plot_data <- evoked_data |>
  arrange(n_back, is_target, time_ms) |>
  group_by(
    participant,
    session,
    n_back,
    is_target,
    condition,
    target_status
  ) |>
  mutate(
    display_amplitude_uv = filter_for_display(
      mean_amplitude_uv,
      time_ms
    )
  ) |>
  ungroup()

filter_caption <- paste(
  "Display only: 30-Hz Butterworth low-pass filter,",
  "order 2 per pass, applied forward and backward.",
  "Analysis data remain unfiltered."
)


# -------------------------------------------------------------------------
# Calculate target-minus-non-target difference waves
# -------------------------------------------------------------------------

difference_data <- evoked_data |>
  select(
    participant,
    session,
    n_back,
    condition,
    time_ms,
    target_status,
    mean_amplitude_uv
  ) |>
  pivot_wider(
    names_from = target_status,
    values_from = mean_amplitude_uv
  ) |>
  mutate(
    difference_uv = Target - `Non-target`
  ) |>
  arrange(n_back, time_ms) |>
  group_by(
    participant,
    session,
    n_back,
    condition
  ) |>
  mutate(
    display_difference_uv = filter_for_display(
      difference_uv,
      time_ms
    )
  ) |>
  ungroup()


# -------------------------------------------------------------------------
# Shared plot settings
# -------------------------------------------------------------------------

condition_colours <- c(
  "1-back" = "#2C7BB6",
  "2-back" = "#74ADD1",
  "3-back" = "#AB8AC4",
  "4-back" = "#E0827A",
  "5-back" = "#D7191C"
)


plot_theme <- theme_minimal(base_size = 12) +
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
# Plot evoked responses by n-back level and target status
# -------------------------------------------------------------------------

evoked_plot <- ggplot(
  evoked_plot_data,
  aes(
    x = time_ms,
    y = display_amplitude_uv,
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
  geom_line(linewidth = 0.7) +
  facet_wrap(
    ~ target_status,
    nrow = 1
  ) +
  scale_colour_manual(
    values = condition_colours
  ) +
  scale_x_continuous(
    breaks = seq(-100, 800, by = 100)
  ) +
  labs(
    title = "Evoked EEG responses across n-back levels",
    subtitle = paste(
      participant_id,
      session_id,
      sep = " | "
    ),
    x = "Time relative to stimulus onset (ms)",
    y = "Mean amplitude (µV)",
    colour = "Condition",
    caption = filter_caption
  ) +
  plot_theme

print(evoked_plot)


# -------------------------------------------------------------------------
# Plot target-minus-non-target difference waves
# -------------------------------------------------------------------------

difference_plot <- ggplot(
  difference_data,
  aes(
    x = time_ms,
    y = display_difference_uv,
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
  geom_line(linewidth = 0.7) +
  scale_colour_manual(
    values = condition_colours
  ) +
  scale_x_continuous(
    breaks = seq(-100, 800, by = 100)
  ) +
  labs(
    title = "Target-minus-non-target EEG responses",
    subtitle = paste(
      participant_id,
      session_id,
      sep = " | "
    ),
    x = "Time relative to stimulus onset (ms)",
    y = "Amplitude difference (µV)",
    colour = "Condition",
    caption = paste(
      "Positive values indicate a more positive response",
      "for targets than non-targets.",
      filter_caption
    )
  ) +
  plot_theme

print(difference_plot)


# -------------------------------------------------------------------------
# Save figures
# -------------------------------------------------------------------------

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

ggsave(
  filename = file.path(
    output_dir,
    paste0(file_prefix, "_evoked_by_condition.png")
  ),
  plot = evoked_plot,
  width = 11,
  height = 5,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(
    output_dir,
    paste0(file_prefix, "_evoked_target_difference.png")
  ),
  plot = difference_plot,
  width = 8,
  height = 5,
  units = "in",
  dpi = 300,
  bg = "white"
)