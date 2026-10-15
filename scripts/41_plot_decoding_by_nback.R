# Plot target decoding separately by n-back level

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
    "_target_decoding_by_nback.csv"
  )
)

output_dir <- "figures"

stopifnot(
  file.exists(input_file)
)

# -------------------------------------------------------------------------
# Load decoding results
# -------------------------------------------------------------------------

decoding_data <- read_csv(
  input_file,
  col_types = cols(
    participant = col_character(),
    session = col_character(),
    n_back = col_integer(),
    window_start = col_integer(),
    window_end = col_integer(),
    window_midpoint = col_double(),
    balanced_accuracy = col_double()
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
  "window_start",
  "window_end",
  "window_midpoint",
  "balanced_accuracy"
)

stopifnot(
  all(required_cols %in% names(decoding_data)),
  nrow(decoding_data) > 0,
  !anyNA(decoding_data[required_cols]),
  all(decoding_data$participant == participant_id),
  all(decoding_data$session == session_id),
  setequal(decoding_data$n_back, 1:5),
  all(is.finite(decoding_data$balanced_accuracy)),
  all(
    decoding_data$balanced_accuracy >= 0 &
      decoding_data$balanced_accuracy <= 1
  )
)

# Check that each n-back level and time window has one result

decoding_keys <- decoding_data |>
  select(
    participant,
    session,
    n_back,
    window_midpoint
  )

stopifnot(
  anyDuplicated(decoding_keys) == 0
)

# Check that all decoding windows have the same width

window_widths <- decoding_data |>
  mutate(
    window_width = window_end - window_start + 1
  ) |>
  pull(window_width) |>
  unique()

stopifnot(
  length(window_widths) == 1
)

# Check that window midpoints progress regularly

window_midpoints <- decoding_data |>
  distinct(
    window_start,
    window_midpoint
  ) |>
  arrange(window_start)

window_steps <- diff(
  window_midpoints$window_start
)

stopifnot(
  length(unique(window_steps)) == 1
)

# -------------------------------------------------------------------------
# Prepare plotting variables
# -------------------------------------------------------------------------

plot_data <- decoding_data |>
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
# Define colours
# -------------------------------------------------------------------------

condition_colours <- c(
  "1-back" = "#2C7BB6",
  "2-back" = "#74ADD1",
  "3-back" = "#AB8AC4",
  "4-back" = "#E0827A",
  "5-back" = "#D7191C"
)

# -------------------------------------------------------------------------
# Plot decoding accuracy by n-back level
# -------------------------------------------------------------------------

decoding_plot <- ggplot(
  plot_data,
  aes(
    x = window_midpoint,
    y = balanced_accuracy,
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
  geom_line(
    linewidth = 0.8
  ) +
  geom_point(
    size = 1.6
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
  scale_y_continuous(
    limits = c(0, 1)
  ) +
  labs(
    title = "Target decoding across n-back levels",
    subtitle = paste(
      participant_id,
      session_id,
      sep = " | "
    ),
    x = "Time relative to stimulus onset (ms)",
    y = "Balanced accuracy",
    colour = "Condition"
  ) +
  theme_minimal(
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

print(decoding_plot)

# -------------------------------------------------------------------------
# Save figure
# -------------------------------------------------------------------------

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

figure_file <- file.path(
  output_dir,
  paste0(
    file_prefix,
    "_target_decoding_by_nback.png"
  )
)

ggsave(
  filename = figure_file,
  plot = decoding_plot,
  width = 8,
  height = 5,
  units = "in",
  dpi = 300,
  bg = "white"
)

stopifnot(
  file.exists(figure_file)
)

cat(
  "Saved decoding figure to:",
  figure_file,
  "\n"
)