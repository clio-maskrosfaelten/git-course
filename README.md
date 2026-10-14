# Time-resolved target decoding from simulated EEG
This project analyses a simulate dEEG dataset from an n-back task. 
The main aim is to examine whether target and non-target trials can be discrimated from single-trial EEG activity, and when decoding performance is strongest across the epoch. 

## Data
Simulated dataset is provided by the Open Science in Practice course. 

Each trial contains:
- behavioural informaiton
- a simulated EEG time seriese from -100 to 800 ms, no channel dimension

## Analysis

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