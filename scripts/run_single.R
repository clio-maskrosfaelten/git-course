# Run the analysis pipeline for one participant and session

participant_id <- "sub-29"
session_id <- "ses-01"

source("scripts/00_setup.R")

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

for (script in scripts) {

  cat(
    "\n",
    "------------------------------------------------------------\n",
    "Running:", script, "\n",
    "Participant:", participant_id, "\n",
    "Session:", session_id, "\n",
    "------------------------------------------------------------\n"
  )

  sys.source(
    script,
    envir = environment()
  )
}

cat(
  "\nPipeline completed for",
  participant_id,
  session_id,
  "\n"
)