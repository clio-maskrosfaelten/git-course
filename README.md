# Time-resolved target decoding from simulated EEG

This project analyses a simulated EEG n-back dataset for the *Open Science in Practice* course.

## Data

The dataset contains trial-level behavioural and EEG data from a simulated n-back experiment.

- Five working-memory load conditions: 1-back to 5-back
- Target and non-target trials
- Behavioural measures including response accuracy and reaction time
- EEG epochs from -100 to 800 ms relative to stimulus onset

The EEG data contain a single time series per trial without channel dimension.

## Research question

**Does increased n-back load shift the timing of peak target decoding?**

Target vs non-target trials are decoded separately within each n-back level using sliding 50-ms EEG windows with a 25-ms step. For each n-back level, the analysis identifies the post-stimulus time window in which target vs non-target decoding accuracy is highest and compared across working-memory loads.

Behavioural performance and evoked target-related responses are analysed as complementary measures of task difficulty and neural response dynamics.

## Analysis

- `00_setup.R`  
  Restores the package environment and creates project folders.

- `run_single.R`  
  Runs the full pipeline for one participant and session.

- `run_all.R`  
  Runs the full pipeline across all available participants and sessions.

- `10_clean_data.R`  
  Joins behavioural and EEG data and applies baseline correction.

- `20_summarise_behaviour.R`  
  Summarises behavioural performance by n-back level.

- `21_plot_behaviour.R`  
  Plots behavioural accuracy and reaction time.

- `30_summarise_evoked.R`  
  Calculates average evoked responses by n-back level and target status.

- `31_plot_evoked.R`  
  Plots evoked responses and target-minus-non-target difference waves.

- `40_decode_by_nback.R`  
  Performs time-resolved target decoding using ridge-regularized logistic regression and stratified cross-validation.

- `41_plot_decoding_by_nback.R`  
  Plots decoding accuracy across time and n-back levels.

- `42_summarise_peak_decoding.R`  
  Extracts peak decoding latency and accuracy.

## Reproducibility

The project uses `renv` for package management.

```r
renv::restore()