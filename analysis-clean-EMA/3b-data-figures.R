### generating data figures for prawnz experiment paper ###

# Load packages #
library(qpdf)
library(viridis)
library(here)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(AICcmodavg)
library(rnaturalearth)
library(grid)
library(devtools)
library(purrr)
library(raster)
library(sf)
library(tolerance)
library(colorRamps)
library(colorspace)
library(Hmisc)
library(PNWColors)

rm(list=ls())

# Read in data #
reflexes<-read.csv(here("data-clean","2024-04-30-EMA_prawn_combined_reflex_data.csv"))
survival<-read.csv(here("data-clean","2024-04-30-EMA_prawn_combined_survival_data.csv"))
trial<-read.csv(here("data-clean","2024-04-30-EMA_prawn_combined_trial_data.csv"))

model.dat = read.csv(here("data-clean","2024-04-29_model_dataframe.csv"))

### set up colour palettes ###
sunsetcols = pnw_palette(name="Sunset2",n=5, type="discrete")
mothcols = pnw_palette(name="Moth", n=12)
anemcols = pnw_palette(name="Anemone", n=7)

### 1. Bar plots of % survival across temperature and treatments ### -----------

# Create temperature lookup from trial data
trial_temps <- trial %>%
  dplyr::mutate(temperature = exp_set_temp_air) %>%
  dplyr::select(trial_number, temperature)

# Merge with survival data
survival_with_temp <- survival %>%
  left_join(trial_temps, by = "trial_number") %>%
  filter(!is.na(treatment) & treatment != "NA" & !is.na(temperature))

# Calculate survival statistics by treatment
treatment_stats <- survival_with_temp %>%
  group_by(treatment) %>%
  summarise(
    n = n(),
    n_alive = sum(alive == 1, na.rm = TRUE),
    percent_alive = (n_alive / n) * 100,
    se = sqrt((n_alive/n) * (1 - n_alive/n) / n),
    ci_lower = pmax(0, percent_alive - 1.96 * se * 100),
    ci_upper = pmin(100, percent_alive + 1.96 * se * 100)
  ) %>%
  arrange(treatment)

# Create temperature bins (adjust range based on actual air temp data)
# First check the temperature range
temp_range <- range(survival_with_temp$temperature, na.rm = TRUE)
cat("Air temperature range:", temp_range[1], "to", temp_range[2], "°C\n")

# Create appropriate bins (1°C bins for air temperature which has wider range)
min_temp <- floor(temp_range[1])  # Remove the /2 * 2 for 1°C bins
max_temp <- ceiling(temp_range[2])  # Remove the /2 * 2 for 1°C bins
survival_with_temp$temp_bin <- cut(survival_with_temp$temperature,
                                   breaks = seq(min_temp, max_temp, by = 1),
                                   labels = seq(min_temp, max_temp - 1, by = 1),  # Note: max_temp - 1
                                   include.lowest = TRUE)

# Calculate survival statistics by temperature bin
temp_stats <- survival_with_temp %>%
  dplyr::group_by(temp_bin) %>%
  dplyr::summarise(
    n = n(),
    n_alive = sum(alive == 1, na.rm = TRUE),
    percent_alive = (n_alive / n) * 100,
    se = sqrt((n_alive/n) * (1 - n_alive/n) / n),
    ci_lower = pmax(0, percent_alive - 1.96 * se * 100),
    ci_upper = pmin(100, percent_alive + 1.96 * se * 100)
  ) %>%
  dplyr::filter(!is.na(temp_bin))

# Convert temp_bin to numeric for plotting (ADD THIS LINE HERE)
temp_stats$temperature <- as.numeric(as.character(temp_stats$temp_bin))

# Set up the plot
png("figures-EMA/figure1_survival_temp_treatment.png", width = 10, height = 5, units = "in", res = 300)

# Create two-panel plot
par(mfrow = c(1, 2), mar = c(5, 5, 3, 2), mgp = c(3, 1, 0))

# Panel A: Temperature vs Percent Alive
# Create the barplot
bar_heights_temp <- temp_stats$percent_alive

bp_temp <- barplot(bar_heights_temp,
                   ylim = c(0, 100),
                   xlab = "Air Temperature (°C)",
                   ylab = "Percent Alive (%)",
                   col = sunsetcols[2],
                   border = "black",
                   las = 1,
                   cex.main = 1.5,
                   cex.lab = 1.2,
                   cex.axis = 1,
                   xaxt = "n")  # Suppress default x-axis

# Add custom x-axis
axis(1, at = bp_temp, labels = FALSE)  # Add tick marks
axis(1, at = bp_temp[seq(1,10,2)], 
     labels = temp_stats$temperature[seq(1,10,2)], 
     cex.axis = 1)

# Add error bars
arrows(x0 = bp_temp,
       y0 = temp_stats$ci_lower,
       x1 = bp_temp,
       y1 = temp_stats$ci_upper,
       angle = 90,
       code = 3,
       length = 0.05,
       lwd = 1.5)
box(bty="L")

# Panel B: Treatment (Time out of water) vs Percent Alive
# Create the barplot
bar_heights_treat <- treatment_stats$percent_alive
bp_treat <- barplot(bar_heights_treat,
                    ylim = c(0, 100),
                    xlab = "Time out of water (minutes)",
                    ylab = "Percent alive (%)",
                    col = sunsetcols[4],  # Using different color from same palette
                    border = "black",
                    las = 1,
                    cex.main = 1.5,
                    cex.lab = 1.2,
                    cex.axis = 1,
                    xaxt = "n")  # Suppress default x-axis

# Add custom x-axis
axis(1, at = bp_treat, labels = FALSE)  # Add tick marks
axis(1, at = bp_treat,  # Show all treatment labels since there are only 6
     labels = treatment_stats$treatment, 
     cex.axis = 1)

# Add error bars
arrows(x0 = bp_treat,
       y0 = treatment_stats$ci_lower,
       x1 = bp_treat,
       y1 = treatment_stats$ci_upper,
       angle = 90,
       code = 3,
       length = 0.05,
       lwd = 1.5)
box(bty="L")

dev.off()

### 2. Length frequency histograms (2 panel) ### -------------------------------

# Create a combined dead category (dead + scavenged)
survival$status <- ifelse(survival$alive == 1, "Alive", "Dead")

# Remove NAs from length data
length_data <- survival[!is.na(survival$length), ]

# Separate data for alive and dead
alive_lengths <- length_data$length[length_data$status == "Alive"]
dead_lengths <- length_data$length[length_data$status == "Dead"]

# Get lengths by stage (remove NAs)
stage_data <- length_data[!is.na(length_data$stage), ]

# Set up the plot
png("figures-EMA/figure2_length_frequency.png", width = 10, height = 7, units = "in", res = 300)

par(mfrow = c(2, 1), mar = c(5, 5, 3, 2), mgp = c(3, 1, 0))

# Panel A: Overlaid histograms for alive vs dead
# Determine common breaks for both histograms
all_lengths <- c(alive_lengths, dead_lengths)
# Add small buffer to ensure all values are included
breaks <- seq(floor(min(all_lengths)) - 0.5, 
              ceiling(max(all_lengths)) + 0.5, 
              by = 0.5)

# Create histograms
hist_alive <- hist(alive_lengths, breaks = breaks, plot = FALSE)
hist_dead <- hist(dead_lengths, breaks = breaks, plot = FALSE)

# Plot with transparency for overlay
hist(alive_lengths, 
     breaks = breaks,
     col = adjustcolor(sunsetcols[2], alpha.f = 0.6),
     border = "black",
     main = "",
     xlab = "Carapace Length (mm)",
     ylab = "Frequency",
     ylim = c(0, max(c(hist_alive$counts, hist_dead$counts)) * 1.1),
     las = 1,
     cex.lab = 1.2,
     cex.axis = 1, bty="L")
box(bty="l")

# Add dead histogram overlay
hist(dead_lengths,
     breaks = breaks,
     col = adjustcolor(sunsetcols[4], alpha.f = 0.6),
     border = "black",
     add = TRUE)

# Add legend
legend("topright", 
       legend = c("Alive", "Dead"),
       fill = adjustcolor(sunsetcols[c(2, 4)], alpha.f = 0.6),
       border = "black",
       bty = "n",
       cex = 1)

# Panel B: Stacked histogram by stage
# Create matrix for stacked histogram
stage_levels <- sort(unique(stage_data$stage))
hist_matrix <- matrix(0, nrow = length(stage_levels), ncol = length(breaks) - 1)

# Fill matrix with counts for each stage
for(i in 1:length(stage_levels)) {
  stage_lengths <- stage_data$length[stage_data$stage == stage_levels[i]]
  if(length(stage_lengths) > 0) {
    hist_temp <- hist(stage_lengths, breaks = breaks, plot = FALSE)
    hist_matrix[i, ] <- hist_temp$counts
  }
}

# Plot first stage as base
plot(x = breaks[-length(breaks)], 
     y = hist_matrix[1,],
     type = "n",
     xlim = range(breaks),
     ylim = c(0, max(colSums(hist_matrix)) * 1.1),
     xlab = "Carapace Length (mm)",
     ylab = "Frequency",
     las = 1,
     cex.lab = 1.2,
     cex.axis = 1, bty="l")

# Add stacked bars manually
for(j in 1:(length(breaks)-1)) {
  bottom <- 0
  for(i in 1:length(stage_levels)) {
    rect(xleft = breaks[j],
         xright = breaks[j+1],
         ybottom = bottom,
         ytop = bottom + hist_matrix[i,j],
         col = sunsetcols[i],
         border = "black")
    bottom <- bottom + hist_matrix[i,j]
  }
}

abline(v=33, col="black", lwd=3, lty=2)

# Add legend with stage names
stage_names <- c("Juvenile", "Male", "Transitional", "Female", "Egged female", "Spent female")
legend("topright",
       legend = stage_names[stage_levels + 1],  # +1 because stages are 0-indexed
       fill = sunsetcols[1:length(stage_levels)],
       border = "black",
       bty = "n",
       cex = 1)

# Close device
dev.off()

# Print summary statistics
cat("\n=== Figure 2 Summary Statistics ===\n")
cat("\nLength distribution by survival status:\n")
cat("Alive: n =", length(alive_lengths), 
    ", mean =", round(mean(alive_lengths), 1),
    ", range =", round(min(alive_lengths), 1), "-", round(max(alive_lengths), 1), "mm\n")
cat("Dead/Scavenged: n =", length(dead_lengths),
    ", mean =", round(mean(dead_lengths), 1),
    ", range =", round(min(dead_lengths), 1), "-", round(max(dead_lengths), 1), "mm\n")

cat("\nLength distribution by stage:\n")
for(i in 1:length(stage_levels)) {
  stage_lengths <- stage_data$length[stage_data$stage == stage_levels[i]]
  cat("Stage", stage_levels[i], "(", stage_names[stage_levels[i] + 1], "): n =", length(stage_lengths),
      ", mean =", round(mean(stage_lengths), 1), "mm\n")
}

### 3. Prawn counts by trial and stage ### -------------------------------------

# Data preparation using base R
# Count prawns by trial and stage
trial_stage_counts <- table(survival$trial_number, survival$stage)
trial_stage_df <- as.data.frame.matrix(trial_stage_counts)

# Get unique trials and stages
trials <- sort(unique(survival$trial_number[!is.na(survival$trial_number)]))
stages <- sort(unique(survival$stage[!is.na(survival$stage)]))

# Set up the plot
png("figures-EMA/figure3_prawns_by_trial_stage.png", width = 12, height = 8, units = "in", res = 300)

# Create layout with space for legend on right
layout(matrix(c(1,2,5,3,4,5), ncol=3, byrow=TRUE), widths=c(1,1,0.3))
par(mar = c(5, 5, 3, 2), mgp = c(3, 1, 0))

# Stage names for titles
stage_names <- c("Juvenile", "Male", "Transitional", "Female", "Egged female", "Spent female")

# Determine y-axis limit for consistency across panels
max_count <- max(trial_stage_counts)

# Create a panel for each of the first 4 stages (0-3)
for(stage in 0:3) {
  # Get counts for this stage
  if(as.character(stage) %in% colnames(trial_stage_df)) {
    counts <- trial_stage_df[, as.character(stage)]
    trial_numbers <- as.numeric(rownames(trial_stage_df))
  } else {
    counts <- rep(0, length(trials))
    trial_numbers <- trials
  }
  
  if (stage == 0 ){
    # Create empty barplot to set up axes
    barplot(counts,
            names.arg = trial_numbers,
            ylim = c(0, 30 * 1.1),
            xlab = "Trial number",
            ylab = "Number of prawns",
            col = "white",
            border = NA,
            las = 1,
            cex.main = 1.3,
            cex.lab = 1.2,
            cex.axis = 1,
            xaxt="n")
    # Add grid lines for easier reading
    abline(h = seq(0, 30, by = 5), col = "gray80", lty = 2)
  # Create barplot
  bp <- barplot(counts,
                names.arg = trial_numbers,
                ylim = c(0, 30 * 1.1),
                col = sunsetcols[stage + 1],
                border = "black",
                las = 1,
                cex.main = 1.3,
                cex.lab = 1.2,
                cex.axis = 1,
                add=TRUE,
                xaxt="n")
  
  # Add custom x-axis with tick marks for all trials
  axis(1, at = bp, labels = FALSE)  # Add tick marks for all bars
  
  # Add labels for every second trial
  label_indices <- seq(1, length(trial_numbers), by = 2)
  axis(1, at = bp[label_indices], 
       labels = trial_numbers[label_indices],
       cex.axis = 1)
 
  box(bty="l")
  
  } else {
    # Create empty barplot to set up axes
    barplot(counts,
            names.arg = trial_numbers,
            ylim = c(0, max_count * 1.1),
            xlab = "Trial number",
            ylab = "Number of prawns",
            col = "white",
            border = NA,
            las = 1,
            cex.main = 1.3,
            cex.lab = 1.2,
            cex.axis = 1,
            xaxt="n")
    # Add grid lines for easier reading
    abline(h = seq(0, max_count, by = 50), col = "gray80", lty = 2)
    
    # Create barplot
    bp <- barplot(counts,
                  names.arg = trial_numbers,
                  ylim = c(0, max_count * 1.1),
                  col = sunsetcols[stage + 1],
                  border = "black",
                  las = 1,
                  cex.main = 1.3,
                  cex.lab = 1.2,
                  cex.axis = 1,
                  add=TRUE,
                  xaxt="n")
    # Add custom x-axis with tick marks for all trials
    axis(1, at = bp, labels = FALSE)  # Add tick marks for all bars
    
    # Add labels for every second trial
    label_indices <- seq(1, length(trial_numbers), by = 2)
    axis(1, at = bp[label_indices], 
         labels = trial_numbers[label_indices],
         cex.axis = 1)
    
    box(bty="l")
  }
}

# Add legend in the fifth panel area
par(mar = c(0, 0, 0, 0))
plot.new()
legend("center",
       legend = stage_names[1:4],
       fill = sunsetcols[1:4],
       border = "black",
       bty = "n",
       cex = 1.3,
       title = "")

# Close device
dev.off()

# Print summary statistics
cat("\n=== Figure 3 Summary Statistics ===\n")
cat("\nTotal prawns by stage across all trials:\n")
for(stage in stages) {
  total <- sum(trial_stage_counts[, as.character(stage)])
  cat("Stage", stage, "(", stage_names[stage + 1], "):", total, "prawns\n")
}

cat("\nTrials with most prawns by stage:\n")
for(stage in 0:3) {
  if(as.character(stage) %in% colnames(trial_stage_df)) {
    max_trial <- which.max(trial_stage_df[, as.character(stage)])
    max_count_stage <- trial_stage_df[max_trial, as.character(stage)]
    cat(stage_names[stage + 1], ": Trial", rownames(trial_stage_df)[max_trial], 
        "with", max_count_stage, "prawns\n")
  }
}