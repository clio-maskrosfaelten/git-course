# Plot behavioural performance across n-back levels

library(tidyverse)

# -------------------------------------------------------------------------
# Check pipeline settings
# -------------------------------------------------------------------------

stopifnot(
  exists("participant_id"),
  exists("session_id")
)

file_prefix <- paste0(
  participant_id,
  "_",
  session_id
)

# -------------------------------------------------------------------------
# File paths
# -------------------------------------------------------------------------

input_file <- file.path(
  "results",
  paste0(
    file_prefix,
    "_behavioural_summary.csv"
  )
)

output_dir <- "figures"

stopifnot(
  file.exists(input_file)
)

# -------------------------------------------------------------------------
# Load behavioural summary
# -------------------------------------------------------------------------

behavioural_summary <- read_csv(
  input_file,
  col_types = cols(
    participant = col_character(),
    session = col_character(),
    n_back = col_integer(),
    n_trials = col_integer(),
    n_targets = col_integer(),
    n_nontargets = col_integer(),
    accuracy = col_double(),
    hit_rate = col_double(),
    false_alarm_rate = col_double(),
    n_correct_target_rt = col_integer(),
    mean_correct_target_rt_ms = col_double()
  ),
  show_col_types = FALSE
)

# -------------------------------------------------------------------------
# Check input
# -------------------------------------------------------------------------

required_cols <- c(
  "participant",
  "session",
  "n_back",
  "accuracy",
  "hit_rate",
  "false_alarm_rate",
  "mean_correct_target_rt_ms"
)

stopifnot(
  all(required_cols %in% names(behavioural_summary)),
  nrow(behavioural_summary) > 0,
  all(behavioural_summary$participant == participant_id),
  all(behavioural_summary$session == session_id),
  setequal(behavioural_summary$n_back, 1:5),
  all(is.finite(behavioural_summary$accuracy)),
  all(is.finite(behavioural_summary$hit_rate)),
  all(is.finite(behavioural_summary$false_alarm_rate)),
  all(is.finite(behavioural_summary$mean_correct_target_rt_ms))
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

behavioural_data <- behavioural_summary |>
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
    plot.margin = margin(
      10,
      15,
      10,
      15
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
# Prepare behavioural rate data
# -------------------------------------------------------------------------

rate_data <- behavioural_data |>
  select(
    participant,
    session,
    n_back,
    condition,
    accuracy,
    hit_rate,
    false_alarm_rate
  ) |>
  pivot_longer(
    cols = c(
      accuracy,
      hit_rate,
      false_alarm_rate
    ),
    names_to = "measure",
    values_to = "proportion"
  ) |>
  mutate(
    measure = factor(
      measure,
      levels = c(
        "accuracy",
        "hit_rate",
        "false_alarm_rate"
      ),
      labels = c(
        "Accuracy",
        "Hit rate",
        "False-alarm rate"
      )
    )
  )

# -------------------------------------------------------------------------
# Plot behavioural rates
# -------------------------------------------------------------------------

performance_plot <- ggplot(
  rate_data,
  aes(
    x = n_back,
    y = proportion,
    group = measure
  )
) +
  geom_line(
    aes(
      linetype = measure
    ),
    colour = "grey40",
    linewidth = 0.8
  ) +
  geom_point(
    aes(
      colour = condition
    ),
    size = 2.4
  ) +
  scale_colour_manual(
    values = condition_colours
  ) +
  scale_x_continuous(
    breaks = 1:5
  ) +
  scale_y_continuous(
    limits = c(0, 1)
  ) +
  guides(
    colour = guide_legend(
      order = 1,
      nrow = 1
    ),
    linetype = guide_legend(
      order = 2,
      nrow = 1
    )
  ) +
  labs(
    title = "Behavioural performance across n-back levels",
    subtitle = paste(
      participant_id,
      session_id,
      sep = " | "
    ),
    x = "N-back level",
    y = "Proportion",
    colour = "Condition",
    linetype = "Measure"
  ) +
  plot_theme +
  theme(
    legend.box = "vertical",
    legend.box.just = "center"
  )

print(performance_plot)

# -------------------------------------------------------------------------
# Plot reaction time
# -------------------------------------------------------------------------

rt_plot <- ggplot(
  behavioural_data,
  aes(
    x = n_back,
    y = mean_correct_target_rt_ms
  )
) +
  geom_line(
    aes(
      group = 1
    ),
    colour = "grey40",
    linewidth = 0.8
  ) +
  geom_point(
    aes(
      colour = condition
    ),
    size = 2.4
  ) +
  scale_colour_manual(
    values = condition_colours
  ) +
  scale_x_continuous(
    breaks = 1:5
  ) +
  labs(
    title = "Correct target reaction time across n-back levels",
    subtitle = paste(
      participant_id,
      session_id,
      sep = " | "
    ),
    x = "N-back level",
    y = "Mean reaction time (ms)",
    colour = "Condition"
  ) +
  plot_theme

print(rt_plot)

# -------------------------------------------------------------------------
# Save figures
# -------------------------------------------------------------------------

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

performance_file <- file.path(
  output_dir,
  paste0(
    file_prefix,
    "_behavioural_performance.png"
  )
)

rt_file <- file.path(
  output_dir,
  paste0(
    file_prefix,
    "_behavioural_rt.png"
  )
)

ggsave(
  filename = performance_file,
  plot = performance_plot,
  width = 9,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = rt_file,
  plot = rt_plot,
  width = 8,
  height = 5,
  units = "in",
  dpi = 300,
  bg = "white"
)

stopifnot(
  file.exists(performance_file),
  file.exists(rt_file)
)

cat(
  "Saved behavioural figures for",
  participant_id,
  session_id,
  "\n"
)