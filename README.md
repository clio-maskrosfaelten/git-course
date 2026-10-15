# Time-resolved target decoding from simulated EEG
This project analyses a simulate dEEG dataset from an n-back task. 
The main aim is to examine whether target and non-target trials can be discrimated from single-trial EEG activity, and when decoding performance is strongest across the epoch. 

## Data
Simulated dataset is provided by the Open Science in Practice course. 

Each trial contains:
- behavioural informaiton
- a simulated EEG time seriese from -100 to 800 ms, no channel dimension

## Setup
The package environment is managed with [renv](https://rstudio.github.io/renv/) (versions are recorded in `renv.lock`).

1. Open the project folder in R/Positron so that the working directory is the project root.
2. Run the setup script:
   ```r
   source("scripts/00_setup.R")
   ```
   This installs renv if needed, restores the packages from `renv.lock`, and creates `data/derived/` and `figures/`.
3. Place the course dataset in `data/` so that the following exists (the data are not tracked in git):
   `data/simulated-dataset-2/sub-30/ses-01/sub-30_ses-01_task-nback_{behav,eeg}.csv`
4. Run the analysis scripts in order (`01_clean_data.R`, `02_decode_targets.R`, `03_plot_decoding.R`), always from the project root.

By default (`install_mode <- "auto"` at the top of the package section in `00_setup.R`), the pinned versions are restored only if your R minor version matches `renv.lock` (R 4.5). On other R versions, current versions of the required packages (`tidyverse`, `rsample`, `yardstick`, `glmnet`) are installed instead, and `renv.lock` is left unchanged. Set `install_mode` to `"lockfile"` or `"current"` to force either behaviour. Restoring old versions from source on Windows needs [Rtools](https://cran.r-project.org/bin/windows/Rtools/).

Manual alternative: `install.packages("renv"); renv::restore()`.

## Analysis

### `00_setup.R`
Restores the renv environment, creates output folders and checks that the input data are present.

### `01_clean_data.R`
Loads the behavioural and EEG files for the current participant and session.
- checks that behavioural and EEG trials correspond correctly
- joins the two datasets by `trial_id`
- identifies EEG time-point columns
- applies trial-wise baseline correction using the -100 to 0 ms interval
- saves the cleaned trial-level dataset

Output: data/derived/sub-30_ses-01_clean.csv


### 02_decode_targets.R
Uses the cleaned single-trial EEG data to classify target and non-target trials across time.

- divides the EEG epoch into consecutive 50-ms windows
- uses the 50 EEG samples within each window as predictors
- creates 5-fold stratified cross-validation splits
- classifies target and non-target trials using ridge-regularized logistic regression
- evaluates predictions using balanced accuracy
- repeats the decoding procedure across the full EEG epoch
- saves the time-resolved decoding results

Output: data/derived/sub-30_ses-01_target_decoding.csv


### 03_plot_decoding.R
Loads the time-resolved decoding results and creates a decoding accuracy plot.
The figure shows:

Output: figures/sub-30_ses-01_target_decoding.png


## Current status
The workflow is currently being developed using participant sub-30, session ses-01.