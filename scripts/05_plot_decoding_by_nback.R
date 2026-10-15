# Plot target decoding separately by n-back level

library(tidyverse)

# -------------------------------------------------------------------------
# Settings
# -------------------------------------------------------------------------

participant_id <- "sub-30"
session_id <- "ses-01"

file_prefix <- paste0(
  participant_id,
  "_",
  session_id
)

input_file <- file.path(
  "results",
  paste0(
    file_prefix,
    "_target_decoding_by_nback.csv"
  )
)

output_dir <- "figures"


# -------------------------------------------------------------------------
# Load decoding results
# -------------------------------------------------------------------------

decoding_data <- read_csv(
  input_file,
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
  all(is.finite(decoding_data$balanced_accuracy))
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
  geom_line(
    linewidth = 0.8
  ) +
  geom_point(
    size = 1.8
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
    title = "Target decoding across n-back levels",
    subtitle = paste(
      participant_id,
      session_id,
      sep = " | "
    ),
    x = "Time (ms)",
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