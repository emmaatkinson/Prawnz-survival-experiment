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

# correct type in column name
names(trial)[names(trial) == "X1h30min_numner"] <- "X1h30min_number"

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

### 4. Length frequency distribution by treatment ### ---------------------------

# Data preparation using base R
# Create separate datasets for each treatment including NA (unbanded)
treatments <- c(0, 30, 60, 90, 120)
treatment_labels <- c("0 min", "30 min", "60 min", "90 min", "120 min")

# Get unbanded prawns (treatment = NA)
unbanded_lengths <- survival$length[is.na(survival$treatment) | survival$treatment == "NA"]
unbanded_lengths <- unbanded_lengths[!is.na(unbanded_lengths)]

# Get lengths for each treatment
treatment_lengths <- list()
for(i in 1:length(treatments)) {
  treatment_lengths[[i]] <- survival$length[survival$treatment == treatments[i] & !is.na(survival$length)]
}

# Determine common breaks for all histograms
all_lengths_combined <- c(unbanded_lengths, unlist(treatment_lengths))
breaks <- seq(floor(min(all_lengths_combined, na.rm=TRUE)) - 0.5, 
              ceiling(max(all_lengths_combined, na.rm=TRUE)) + 0.5, 
              by = 0.5)  # 2mm bins

# Determine y-axis limit for consistency
max_freq <- 0
for(i in 1:length(treatments)) {
  if(length(treatment_lengths[[i]]) > 0) {
    h <- hist(treatment_lengths[[i]], breaks = breaks, plot = FALSE)
    max_freq <- max(max_freq, max(h$counts))
  }
}
h_unbanded <- hist(unbanded_lengths, breaks = breaks, plot = FALSE)
max_freq <- max(max_freq, max(h_unbanded$counts))

# Set up the plot
png("figures-EMA/figure4_length_by_treatment.png", width = 12, height = 8, units = "in", res = 300)

# Create layout with space for legend at bottom
layout(matrix(c(1:6, 7, 7, 7), ncol = 3, byrow = TRUE), heights = c(1, 1, 0.2))
par(mar = c(5, 5, 3, 2), mgp = c(3, 1, 0))

# Colors from anemone palette
anemcols <- pnw_palette(name="Anemone", n=7)

# Plot histograms for each treatment
for(i in 1:length(treatments)) {
  if(length(treatment_lengths[[i]]) > 0) {
    
    hist(treatment_lengths[[i]],
         breaks = breaks,
         col = "white",
         border = "white",
         ylim = c(0, max_freq * 1.1),
         xlab = "Carapace length (mm)",
         ylab = "Frequency",
         main="",
         las = 1,
         cex.lab = 1.1,
         cex.axis = 1)
    
    # Add grid lines
    abline(h = seq(0, max_freq, by = 10), col = "gray80", lty = 2)
    
    hist(treatment_lengths[[i]],
         breaks = breaks,
         col = anemcols[i],
         border = "black",
         ylim = c(0, max_freq * 1.1),
         las = 1, xlab="", ylab="",
         main="",
         cex.lab = 1.1,
         cex.axis = 1, add=TRUE)
    
    box(bty="l")
    
    # Add sample size to panel
    mtext(paste("n =", length(treatment_lengths[[i]])), 
          side = 3, line = -2, adj = 0.95, cex = 0.9)
    

  } else {
    # Empty plot if no data
    plot.new()
    text(0.5, 0.5, paste(treatment_labels[i], "\n(No data)"), cex = 1.2)
  }
}

# Plot histogram for unbanded prawns in the 6th panel
hist(unbanded_lengths,
     breaks = breaks,
     col = "white",
     border = "white",
     main = "",
     xlab = "Carapace length (mm)",
     ylab = "Frequency",
     ylim = c(0, max_freq * 1.1),
     las = 1,
     cex.lab = 1.1,
     cex.axis = 1)


# Add grid lines
abline(h = seq(0, max_freq, by = 10), col = "gray80", lty = 2)


hist(unbanded_lengths,
     breaks = breaks,
     col = anemcols[7],
     border = "black",
     main = "",
     xlab = "",
     ylab = "",
     ylim = c(0, max_freq * 1.1),
     las = 1,
     cex.lab = 1.1,
     cex.axis = 1, add=TRUE)
box(bty="l")

# Add sample size
mtext(paste("n =", length(unbanded_lengths)), 
      side = 3, line = -2, adj = 0.95, cex = 0.9)

# Add legend at bottom
par(mar = c(0, 0, 0, 0))
plot.new()
legend("center",
       legend = c(treatment_labels, "Unbanded"),
       fill = c(anemcols[1:6], anemcols[7]),
       border = "black",
       ncol = 7,  # Horizontal layout
       bty = "n",
       cex = 1.3,
       title = "")

# Close device
dev.off()

# Print summary statistics
cat("\n=== Figure 4 Summary Statistics ===\n")
cat("\nLength distribution by treatment:\n")
for(i in 1:length(treatments)) {
  if(length(treatment_lengths[[i]]) > 0) {
    cat(treatment_labels[i], ": n =", length(treatment_lengths[[i]]),
        ", mean =", round(mean(treatment_lengths[[i]]), 1),
        ", range =", round(min(treatment_lengths[[i]]), 1), "-", 
        round(max(treatment_lengths[[i]]), 1), "mm\n")
  }
}
cat("Unbanded: n =", length(unbanded_lengths),
    ", mean =", round(mean(unbanded_lengths), 1),
    ", range =", round(min(unbanded_lengths), 1), "-",
    round(max(unbanded_lengths), 1), "mm\n")

### Figure 5: Lost and unbanded prawns per trial ###

# Create a copy of survival data for this figure and combine 90/100 min treatments
survival_fig5 <- survival
survival_fig5$treatment[survival_fig5$treatment == 100] <- 90

# Calculate initial prawns per trial from trial dataframe
# Sum all the release numbers per trial
trial$initial_prawns <- rowSums(trial[, c("immediate_release_number", 
                                          "X30min_number", 
                                          "X1h_number", 
                                          "X1h30min_number",
                                          "X2h_number")], na.rm = TRUE)

# Create lookup for initial prawns
initial_lookup <- trial$initial_prawns
names(initial_lookup) <- trial$trial_number

# Calculate recovered prawns per trial
all_trials <- sort(unique(trial$trial_number))

# Count prawns by category for each trial
prawn_counts <- data.frame(
  trial = all_trials,
  initial = numeric(length(all_trials)),
  banded_recovered = numeric(length(all_trials)),
  unbanded = numeric(length(all_trials)),
  truly_lost = numeric(length(all_trials))
)

for(i in 1:length(all_trials)) {
  t <- all_trials[i]
  
  # Initial prawns
  prawn_counts$initial[i] <- initial_lookup[as.character(t)]
  
  # Banded recovered (prawns with known treatment)
  prawn_counts$banded_recovered[i] <- sum(survival_fig5$trial_number == t & 
                                            !is.na(survival_fig5$treatment) & 
                                            survival_fig5$treatment != "NA", na.rm = TRUE)
  
  # Unbanded prawns (treatment = NA)
  prawn_counts$unbanded[i] <- sum(survival_fig5$trial_number == t & 
                                    (is.na(survival_fig5$treatment) | survival_fig5$treatment == "NA"), 
                                  na.rm = TRUE)
  
  # Truly lost = initial - banded_recovered - unbanded
  prawn_counts$truly_lost[i] <- prawn_counts$initial[i] - 
    prawn_counts$banded_recovered[i] - 
    prawn_counts$unbanded[i]
}

# Calculate proportions and CIs for lost and unbanded
prawn_counts$prop_lost <- prawn_counts$truly_lost / prawn_counts$initial
prawn_counts$prop_unbanded <- prawn_counts$unbanded / prawn_counts$initial

# Calculate 95% CIs for proportions (using binomial approximation)
# Handle negative proportions (when we "gained" prawns)
prawn_counts$lost_ci_lower <- NA
prawn_counts$lost_ci_upper <- NA
prawn_counts$unbanded_ci_lower <- NA
prawn_counts$unbanded_ci_upper <- NA

for(i in 1:nrow(prawn_counts)) {
  if(prawn_counts$initial[i] > 0) {
    # For lost prawns - handle negative values
    if(prawn_counts$prop_lost[i] < 0) {
      # Negative loss (gained prawns) - set CI around the negative value
      prawn_counts$lost_ci_lower[i] <- prawn_counts$prop_lost[i] - 0.5/prawn_counts$initial[i]
      prawn_counts$lost_ci_upper[i] <- prawn_counts$prop_lost[i] + 0.5/prawn_counts$initial[i]
    } else if(prawn_counts$prop_lost[i] > 0 & prawn_counts$prop_lost[i] < 1) {
      se_lost <- sqrt(prawn_counts$prop_lost[i] * (1 - prawn_counts$prop_lost[i]) / prawn_counts$initial[i])
      prawn_counts$lost_ci_lower[i] <- max(0, prawn_counts$prop_lost[i] - 1.96 * se_lost)
      prawn_counts$lost_ci_upper[i] <- min(1, prawn_counts$prop_lost[i] + 1.96 * se_lost)
    } else {
      # If prop is exactly 0 or 1
      prawn_counts$lost_ci_lower[i] <- max(0, prawn_counts$prop_lost[i] - 0.5/prawn_counts$initial[i])
      prawn_counts$lost_ci_upper[i] <- min(1, prawn_counts$prop_lost[i] + 0.5/prawn_counts$initial[i])
    }
    
    # For unbanded prawns (should never be negative)
    if(prawn_counts$prop_unbanded[i] > 0 & prawn_counts$prop_unbanded[i] < 1) {
      se_unbanded <- sqrt(prawn_counts$prop_unbanded[i] * (1 - prawn_counts$prop_unbanded[i]) / prawn_counts$initial[i])
      prawn_counts$unbanded_ci_lower[i] <- max(0, prawn_counts$prop_unbanded[i] - 1.96 * se_unbanded)
      prawn_counts$unbanded_ci_upper[i] <- min(1, prawn_counts$prop_unbanded[i] + 1.96 * se_unbanded)
    } else {
      prawn_counts$unbanded_ci_lower[i] <- max(0, prawn_counts$prop_unbanded[i] - 0.5/prawn_counts$initial[i])
      prawn_counts$unbanded_ci_upper[i] <- min(1, prawn_counts$prop_unbanded[i] + 0.5/prawn_counts$initial[i])
    }
  }
}

# FIGURE 5A: Lost vs Unbanded
png("figures-EMA/figure5a_lost_unbanded_prawns.png", width = 10, height =6, units = "in", res = 300)
par(mfrow = c(1, 2), mar = c(5, 5, 3, 2), mgp = c(3, 1, 0))

# Panel A: Absolute numbers
# Handle negative values (gained prawns) - can't stack negative with positive
stacked_data <- rbind(prawn_counts$truly_lost, prawn_counts$unbanded)
rownames(stacked_data) <- c("Truly Lost", "Unbanded")

# For trials with negative loss, we need to handle differently
# Set negative values to 0 for stacking purposes
stacked_data_plot <- stacked_data
stacked_data_plot[1, stacked_data_plot[1,] < 0] <- 0

bp_abs <- barplot(stacked_data_plot,
                  names.arg = all_trials,
                  ylim = c(min(stacked_data[1,]) * 1.2, max(colSums(stacked_data_plot)) * 1.2),
                  xlab = "Trial number",
                  ylab = "Number of prawns",
                  main = "",
                  col = c(sunsetcols[5], sunsetcols[2]),
                  border = "black",
                  las = 1,
                  cex.main = 1.3,
                  cex.lab = 1.2,
                  cex.axis = 1)

# Add negative bars separately for trials with gained prawns
for(i in 1:ncol(stacked_data)) {
  if(stacked_data[1, i] < 0) {
    # Draw negative bar from 0 down
    rect(xleft = bp_abs[i] - 0.5,
         xright = bp_abs[i] + 0.5,
         ybottom = stacked_data[1, i],
         ytop = 0,
         col = sunsetcols[5],
         border = "black")
    
  }
}

# Add horizontal line at y=0
abline(h = 0, col = "black", lwd = 1)


# Panel B: Proportions with CIs

# Create grouped barplot for proportions
prop_matrix <- rbind(prawn_counts$prop_lost * 100, prawn_counts$prop_unbanded * 100)
rownames(prop_matrix) <- c("Truly Lost", "Unbanded")

# Adjust y-limits to handle potential negative values
y_min <- min(c(0, prawn_counts$lost_ci_lower * 100), na.rm = TRUE) * 1.1
y_max <- max(c(prawn_counts$lost_ci_upper, prawn_counts$unbanded_ci_upper), na.rm = TRUE) * 120

bp_prop <- barplot(prop_matrix,
                   beside = TRUE,
                   names.arg = all_trials,
                   ylim = c(y_min, y_max),
                   xlab = "Trial number",
                   ylab = "Percentage of initial prawns lost (%)",
                   main = "",
                   col = c(sunsetcols[5], sunsetcols[2]),
                   border = "black",
                   las = 1,
                   cex.main = 1.3,
                   cex.lab = 1.2,
                   cex.axis = 1)

# Add a reference line at 0 if there are negative values
if(y_min < 0) {
  abline(h = 0, col = "black", lwd = 1)
}

# Add legend
legend("topright",
       legend = c("Lost", "Unbanded"),
       fill = c(sunsetcols[5], sunsetcols[2]),
       border = "black",
       bty = "n",
       cex = 1.1)

dev.off()

# FIGURE 5B: Loss by treatment
# Calculate loss by treatment (excluding unbanded)
treatment_stats <- data.frame(
  treatment = c(0, 30, 60, 90, 120),
  expected = numeric(5),
  recovered = numeric(5),
  lost = numeric(5),
  prop_lost = numeric(5)
)

# Get expected and recovered for each treatment across all trials
columns <- c("immediate_release_number", "X30min_number", "X1h_number", 
             "X1h30min_number", "X2h_number")

for(i in 1:nrow(treatment_stats)) {
  col_name <- columns[i]
  treat <- treatment_stats$treatment[i]
  
  # Sum expected across all trials
  treatment_stats$expected[i] <- sum(trial[, col_name], na.rm = TRUE)
  
  # Sum recovered across all trials
  treatment_stats$recovered[i] <- sum(survival_fig5$treatment == treat & 
                                        !is.na(survival_fig5$treatment), na.rm = TRUE)
  
  # Calculate lost
  treatment_stats$lost[i] <- treatment_stats$expected[i] - treatment_stats$recovered[i]
  
  # Calculate proportion
  treatment_stats$prop_lost[i] <- treatment_stats$lost[i] / treatment_stats$expected[i]
}

# Calculate 95% CIs
treatment_stats$ci_lower <- pmax(0, treatment_stats$prop_lost - 
                                   1.96 * sqrt(treatment_stats$prop_lost * (1 - treatment_stats$prop_lost) / 
                                                 treatment_stats$expected))
treatment_stats$ci_upper <- pmin(1, treatment_stats$prop_lost + 
                                   1.96 * sqrt(treatment_stats$prop_lost * (1 - treatment_stats$prop_lost) / 
                                                 treatment_stats$expected))

png("figures-EMA/figure5b_loss_by_treatment.png", width = 10, height = 6, units = "in", res = 300)
par(mfrow = c(1, 2), mar = c(5, 5, 3, 2), mgp = c(3, 1, 0))

# Panel A: Absolute numbers lost by treatment
bp_treat_abs <- barplot(treatment_stats$lost,
                        names.arg = paste0(treatment_stats$treatment, " min"),
                        ylim = c(0, max(treatment_stats$lost) * 1.2),
                        xlab = "Time out of water",
                        ylab = "Prawns lost",
                        main = "",
                        col = "white",
                        border = "white",
                        las = 1,
                        cex.lab = 1.2,
                        cex.axis = 1)
# Add grid
abline(h = seq(0, max(treatment_stats$lost), by = 50), col = "gray80", lty = 2)

bp_treat_abs <- barplot(treatment_stats$lost,
                        names.arg = paste0(treatment_stats$treatment, " min"),
                        ylim = c(0, max(treatment_stats$lost) * 1.2),
                        xlab = "",
                        ylab = "",
                        main = "",
                        col = anemcols[1:5],
                        border = "black",
                        las = 1,
                        cex.main = 1.3,
                        cex.lab = 1.2,
                        cex.axis = 1, add=TRUE)
# Add values on top
text(x = bp_treat_abs,
     y = treatment_stats$lost + max(treatment_stats$lost) * 0.02,
     labels = treatment_stats$lost,
     cex = 0.9)

# Add expected numbers
text(x = bp_treat_abs,
     y = treatment_stats$lost + max(treatment_stats$lost) * 0.08,
     labels = paste0("(of ", treatment_stats$expected, ")"),
     cex = 0.8,
     col = "gray40")
box(bty="l")

# Panel B: Proportional loss by treatment with CIs
bp_treat_prop <- barplot(treatment_stats$prop_lost * 100,
                         names.arg = paste0(treatment_stats$treatment, " min"),
                         ylim = c(0, max(treatment_stats$ci_upper) * 120),
                         xlab = "Time out of water",
                         ylab = "Percentage lost (%)",
                         main = "",
                         col = "white",
                         border = "white",
                         las = 1,
                         cex.lab = 1.2,
                         cex.axis = 1)
# Add grid
abline(h = seq(0, 50, by = 5), col = "gray80", lty = 2)

bp_treat_prop <- barplot(treatment_stats$prop_lost * 100,
                         names.arg = paste0(treatment_stats$treatment, " min"),
                         ylim = c(0, max(treatment_stats$ci_upper) * 120),
                         xlab = "",
                         ylab = "",
                         main = "",
                         col = anemcols[1:5],
                         border = "black",
                         las = 1,
                         cex.main = 1.3,
                         cex.lab = 1.2,
                         cex.axis = 1, add=TRUE)


# Add error bars
arrows(x0 = bp_treat_prop,
       y0 = treatment_stats$ci_lower * 100,
       x1 = bp_treat_prop,
       y1 = treatment_stats$ci_upper * 100,
       angle = 90,
       code = 3,
       length = 0.05,
       lwd = 1.5)

# Add percentage values
text(x = bp_treat_prop,
     y = treatment_stats$ci_upper * 100 + 2,
     labels = paste0(round(treatment_stats$prop_lost * 100, 1), "%"),
     cex = 0.9)
box(bty="l")

dev.off()

# Print summary statistics
cat("\n=== Figure 5 Summary Statistics ===\n")
cat("Note: 100-minute treatment prawns have been combined with 90-minute treatment\n")
cat("      due to timer error in one trial.\n\n")

cat("Figure 5A - Lost vs Unbanded:\n")
cat("  Total initial prawns:", sum(prawn_counts$initial), "\n")
cat("  Total truly lost:", sum(prawn_counts$truly_lost), 
    "(", round(sum(prawn_counts$truly_lost)/sum(prawn_counts$initial)*100, 1), "%)\n")
cat("  Total unbanded:", sum(prawn_counts$unbanded),
    "(", round(sum(prawn_counts$unbanded)/sum(prawn_counts$initial)*100, 1), "%)\n")

cat("\nFigure 5B - Loss by Treatment:\n")
for(i in 1:nrow(treatment_stats)) {
  cat("  ", treatment_stats$treatment[i], "min: lost", treatment_stats$lost[i], 
      "of", treatment_stats$expected[i], 
      "(", round(treatment_stats$prop_lost[i]*100, 1), "%)\n")
}

### 6. Prawn condition breakdown by trial ### ----------------------------------

# Calculate condition counts per trial
trials <- sort(unique(survival$trial_number[!is.na(survival$trial_number)]))

# Create matrix for stacked barplot (rows = conditions, columns = trials)
condition_matrix <- matrix(0, nrow = 3, ncol = length(trials))
rownames(condition_matrix) <- c("Alive", "Dead", "Scavenged")
colnames(condition_matrix) <- trials

# Fill in the counts
for(i in 1:length(trials)) {
  t <- trials[i]
  trial_data <- survival[survival$trial_number == t, ]
  
  condition_matrix["Alive", i] <- sum(trial_data$alive == 1, na.rm = TRUE)
  condition_matrix["Dead", i] <- sum(trial_data$dead == 1, na.rm = TRUE)
  condition_matrix["Scavenged", i] <- sum(trial_data$scavenged == 1, na.rm = TRUE)
}

# Convert to proportions
condition_props <- prop.table(condition_matrix, margin = 2) * 100  # Convert to percentages

# Set up the plot
png("figures-EMA/figure6_condition_by_trial.png", width = 8, height = 6, units = "in", res = 300)
par(mar = c(5, 5, 3, 2), mgp = c(3, 1, 0))

# Create stacked barplot
bp_condition <- barplot(condition_props,
                        names.arg = trials,
                        ylim = c(0, 115),
                        yaxt="n",
                        xlab = "Trial number",
                        ylab = "",
                        col = c(sunsetcols[1], sunsetcols[3], sunsetcols[5]),
                        border = "black",
                        las = 1,
                        cex.main = 1.3,
                        cex.lab = 1.2,
                        cex.axis = 1)
axis(2, at=seq(0,100,20), labels = TRUE)
box(bty="l")

# Redraw bars on top of grid
barplot(condition_props,
        col = c(sunsetcols[1], sunsetcols[3], sunsetcols[5]),
        border = "black",
        add = TRUE,
        xaxt = "n",
        yaxt = "n")


# Add legend
legend("topright",
       legend = c("Alive", "Dead", "Scavenged"),
       fill = c(sunsetcols[1], sunsetcols[3], sunsetcols[5]),
       border = "black",
       bty = "n",
       cex = 0.7)
# Close device
dev.off()

# Print summary statistics
cat("\n=== Figure 6 Summary Statistics ===\n")
cat("\nOverall condition breakdown across all trials:\n")
total_conditions <- rowSums(condition_matrix)
total_prawns <- sum(total_conditions)
cat("  Alive:", total_conditions["Alive"], 
    "(", round(total_conditions["Alive"]/total_prawns * 100, 1), "%)\n")
cat("  Dead:", total_conditions["Dead"],
    "(", round(total_conditions["Dead"]/total_prawns * 100, 1), "%)\n")
cat("  Scavenged:", total_conditions["Scavenged"],
    "(", round(total_conditions["Scavenged"]/total_prawns * 100, 1), "%)\n")

cat("\nTrials with highest survival rate:\n")
survival_rates <- condition_props["Alive", ]
top_trials <- order(survival_rates, decreasing = TRUE)[1:3]
for(i in top_trials) {
  cat("  Trial", trials[i], ":", round(survival_rates[i], 1), "% alive\n")
}