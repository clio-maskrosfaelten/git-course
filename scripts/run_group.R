# Run group-level analyses from existing participant-level results

# -------------------------------------------------------------------------
# Project setup
# -------------------------------------------------------------------------

source("scripts/00_setup.R")

# -------------------------------------------------------------------------
# Group-level analysis scripts
# -------------------------------------------------------------------------

group_scripts <- c(
  "scripts/50_combine_results.R",
  "scripts/51_summarise_group_decoding.R",
  "scripts/52_plot_group_decoding.R",
  "scripts/53_test_peak_latency.R"
)

stopifnot(
  all(file.exists(group_scripts))
)

# -------------------------------------------------------------------------
# Run group-level pipeline
# -------------------------------------------------------------------------

for (script in group_scripts) {

  cat(
    "\n",
    "============================================================\n",
    "Running:", script, "\n",
    "============================================================\n"
  )

  sys.source(
    script,
    envir = environment()
  )
}

cat(
  "\n",
  "Group-level pipeline completed.\n"
)