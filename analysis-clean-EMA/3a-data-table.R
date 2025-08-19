################################################################################
# Trial summary table
# Date created: August 15 2025
# Description: This script generates a summary table of trial information from
#              the prawn survival experiment, combining data from trial_data
#              and survival_data spreadsheets.
################################################################################

library(here)


# Clear workspace
rm(list = ls())

# Set working directory (adjust as needed)
# setwd("your/path/here")

# Read in data files
trial_data <- read.csv(here("data-clean","2024-04-30-EMA_prawn_combined_trial_data.csv"), stringsAsFactors = FALSE)
survival_data <- read.csv(here("data-clean","2024-04-30-EMA_prawn_combined_survival_data.csv"), stringsAsFactors = FALSE)

# Get unique trial numbers
trial_numbers <- sort(unique(trial_data$trial_number))

# Initialize the summary table
summary_table <- data.frame(
  trial_number = trial_numbers,
  air_temperature = NA,
  salinity_ppm = NA,
  n_treatment_0 = 0,
  n_treatment_30 = 0,
  n_treatment_60 = 0,
  n_treatment_90 = 0,
  n_treatment_100 = 0,
  n_treatment_120 = 0,
  n_total_start = 0,
  n_total_end = 0,
  n_lost = 0,
  n_unbanded = 0
)

# Process each trial
for (i in 1:length(trial_numbers)) {
  trial_num <- trial_numbers[i]
  
  # Get trial-specific data
  trial_info <- trial_data[trial_data$trial_number == trial_num, ]
  survival_trial <- survival_data[survival_data$trial_number == trial_num, ]
  
  # Air temperature (at start of experiment)
  summary_table$air_temperature[i] <- trial_info$exp_set_temp_air[1]
  
  # Salinity - use 0m salinity, or tote salinity if available
  if (!is.na(trial_info$exp_set_tote_sal[1]) && trial_info$exp_set_tote_sal[1] != "") {
    summary_table$salinity_ppm[i] <- trial_info$exp_set_tote_sal[1]
  } else {
    summary_table$salinity_ppm[i] <- trial_info$exp_set_sal_0m[1]
  }
  
  # Count prawns by treatment at start of trial
  # We'll count all prawns in the survival data for this trial
  treatments <- survival_trial$treatment
  
  # Handle NA treatments (unbanded prawns)
  na_mask <- is.na(treatments) | treatments == "NA" | treatments == ""
  summary_table$n_unbanded[i] <- sum(na_mask)
  
  # Count each treatment
  summary_table$n_treatment_0[i] <- sum(treatments == 0, na.rm = TRUE)
  summary_table$n_treatment_30[i] <- sum(treatments == 30, na.rm = TRUE)
  summary_table$n_treatment_60[i] <- sum(treatments == 60, na.rm = TRUE)
  summary_table$n_treatment_90[i] <- sum(treatments == 90, na.rm = TRUE)
  summary_table$n_treatment_100[i] <- sum(treatments == 100, na.rm = TRUE)
  summary_table$n_treatment_120[i] <- sum(treatments == 120, na.rm = TRUE)
  
  # Total prawns at start (all rows in survival data for this trial)
  summary_table$n_total_start[i] <- sum(trial_info[,37:41], na.rm=TRUE)
  
  # Total prawns remaining at end (those  prawns recorded at the end of the trial, including unbanded)
  summary_table$n_total_end[i] <- length(treatments)
  
  # Number of lost prawns
  summary_table$n_lost[i] <- summary_table$n_total_start[i] - summary_table$n_total_end[i]
}

# Round numeric columns to reasonable precision
summary_table$air_temperature <- round(summary_table$air_temperature, 1)
summary_table$salinity_ppm <- round(summary_table$salinity_ppm, 1)

# Create a more readable version with better column names
final_table <- summary_table
names(final_table) <- c(
  "Trial",
  "Air Temp (°C)",
  "Salinity (ppm)",
  "Treatment 0 min",
  "Treatment 30 min",
  "Treatment 60 min",
  "Treatment 90 min",
  "Treatment 100 min",
  "Treatment 120 min",
  "Total Start",
  "Total End",
  "Lost",
  "Unbanded"
)

# Display the table
print("Summary Table of Trial Information:")
print("====================================")
print(final_table)

# Save the table to CSV
write.csv(final_table, here("data-clean","18Aug2025-trial_summary_table.csv"), row.names = FALSE)
