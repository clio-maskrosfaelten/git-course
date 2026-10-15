# Run the analysis pipeline for all participants and sessions

library(tidyverse)

# -------------------------------------------------------------------------
# Project setup
# -------------------------------------------------------------------------

source("scripts/00_setup.R")

# -------------------------------------------------------------------------
# Find participant and session combinations
# -------------------------------------------------------------------------

data_root <- file.path(
  "data",
  "simulated-dataset-2"
)

participant_dirs <- list.dirs(
  data_root,
  recursive = FALSE,
  full.names = FALSE
)

run_table <- map_dfr(
  participant_dirs,
  function(participant_id) {

    participant_dir <- file.path(
      data_root,
      participant_id
    )

    session_dirs <- list.dirs(
      participant_dir,
      recursive = FALSE,
      full.names = FALSE
    )

    tibble(
      participant_id = participant_id,
      session_id = session_dirs
    )
  }
) |>
  arrange(
    participant_id,
    session_id
  )

stopifnot(
  nrow(run_table) > 0
)

print(run_table)

# -------------------------------------------------------------------------
# Analysis scripts
# -------------------------------------------------------------------------

scripts <- c(
  "scripts/10_clean_data.R",
  "scripts/20_summarise_behaviour.R",
  "scripts/21_plot_behaviour.R",
  "scripts/30_summarise_evoked.R",
  "scripts/31_plot_evoked.R",
  "scripts/40_decode_by_nback.R",
  "scripts/41_plot_decoding_by_nback.R",
  "scripts/42_summarise_peak_decoding.R"
)

stopifnot(
  all(file.exists(scripts))
)

# -------------------------------------------------------------------------
# Run pipeline for all participant-session combinations
# -------------------------------------------------------------------------

for (i in seq_len(nrow(run_table))) {

  participant_id <- run_table$participant_id[i]
  session_id <- run_table$session_id[i]

  cat(
    "\n",
    "============================================================\n",
    "Running pipeline\n",
    "Participant:", participant_id, "\n",
    "Session:", session_id, "\n",
    "============================================================\n"
  )

  for (script in scripts) {

    cat(
      "\n",
      "------------------------------------------------------------\n",
      "Running:", script, "\n",
      "------------------------------------------------------------\n"
    )

    sys.source(
      script,
      envir = environment()
    )
  }
}

cat(
  "\nAll participant-session pipelines completed.\n"
)