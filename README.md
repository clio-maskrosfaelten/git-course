# Time-resolved target decoding from simulated EEG

This project analyses a simulated EEG n-back dataset for the *Open Science in Practice* course.

## Data

The dataset contains trial-level behavioural and EEG data from a simulated n-back experiment.

- Five working-memory load conditions: 1-back to 5-back
- Target and non-target trials
- Behavioural measures including accuracy and reaction time
- EEG epochs from -100 to 800 ms relative to stimulus onset

The EEG data contain one time series per trial rather than multichannel recordings. The analysis therefore focuses on temporal rather than spatial EEG patterns.

## Research question

**Does increased n-back load shift the timing of peak target decoding?**

Target versus non-target status is decoded separately within each n-back level to estimate when target-related information is most discriminable in the EEG signal. For each load level, the analysis identifies the post-stimulus time window with the highest decoding accuracy, and tests whether this peak occurs later as n-back load increases.
Behavioural performance and evoked target-related responses are analysed as complementary measures of task performance and neural response dynamics.

## Flowchart
<img width="688" height="752" alt="flowchart" src="https://github.com/user-attachments/assets/e64f3f81-42cd-4fe5-8165-e994317826f3" />

## Scripts

### Participant and session level

- `10_clean_data.R`  
  Joins behavioural and EEG data and applies trial-wise baseline correction.

- `20_summarise_behaviour.R` / `21_plot_behaviour.R`  
  Summarise and plot behavioural performance across n-back levels.

- `30_summarise_evoked.R` / `31_plot_evoked.R`  
  Summarise and plot evoked responses and target-minus-non-target difference waves.

- `40_decode_by_nback.R`  
  Performs time-resolved target decoding separately for each n-back level.

- `41_plot_decoding_by_nback.R`  
  Plots decoding accuracy over time.

- `42_summarise_peak_decoding.R`  
  Extracts peak decoding latency and accuracy.

### Group level

- `50_combine_results.R`  
  Combines participant- and session-level results.

- `51_summarise_group_decoding.R` / `52_plot_group_decoding.R`  
  Summarise and plot group-level decoding time courses and peak latencies.

- `53_summarise_group_evoked.R` / `54_plot_group_evoked.R`  
  Summarise and plot group-level evoked responses.

- `55_test_peak_latency.R`  
  Tests the linear change in peak decoding latency with increasing n-back load.

Sessions are first averaged within participants before group-level summaries are calculated.

Evoked waveforms are low-pass filtered at 10 Hz with a Butterworth filter for display only; numerical analyses use the unfiltered summary data.

## Running

The project uses `renv` for package management.

```r
renv::restore()
```

## Preliminary results

### Target-related evoked response
<img width="3000" height="1650" alt="group_evoked_target_difference" src="https://github.com/user-attachments/assets/47604d13-a1e9-44ba-8802-e5d2ae9ad75f" />

### Target decoding across n-back levels
<img width="2700" height="1650" alt="group_target_decoding_by_nback" src="https://github.com/user-attachments/assets/b96dc429-f8dc-406d-8193-02b207cad15c" />



