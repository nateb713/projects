### SIMPLIFIED NAcc SUBJECT SPACE PLOTS - NO CUSTOM FUNCTIONS ###

# Load required libraries 
library(ggplot2)
library(reshape2)
library(plyr)
library(stringr)
library(tidyverse)
library(psych)

# Read the combined NAcc2 subject space data
d.tc_combined = read.csv('/Users/nate/Desktop/subspace/nacc_timecourses/nacc2_ses1&2_tcs_n16.csv', head=T)

# Split by session
d.tc1 = d.tc_combined[d.tc_combined$ses == 's1', ]
d.tc2 = d.tc_combined[d.tc_combined$ses == 's2', ]

# Define colors
cond_colors = c("+$5" = "black", "$0" = "grey50")
cond_fills = c("+$5" = "black", "$0" = "white")
cond_lines = c("+$5" = "solid", "$0" = "dashed")

### PROCESS TIMECOURSE DATA FOR BLOCKS ###

# Add session labels
d.tc1$session = "Session 1"
d.tc2$session = "Session 2"
d.combined = rbind(d.tc1, d.tc2)

# NAcc2 columns
nacc2_cols = grep("nacc2", colnames(d.combined), value = TRUE)

# Process NAcc2 block data
d.tc.melt = melt(d.combined, id=c('tr','condition','subject','block','session'), measure.vars = nacc2_cols)
d.tc.subject = ddply(d.tc.melt, .variables = c('tr','condition','subject','block','session'),
                     summarise, mean.subject = mean(value, na.rm = TRUE))
d.tc.summary = ddply(d.tc.subject, .variables = c('tr','condition','block','session'),
                     function(x) data.frame(
                       mean = mean(x$mean.subject, na.rm = TRUE),
                       sem = sd(x$mean.subject, na.rm = TRUE)/sqrt(length(x$mean.subject))
                     ))

# Add condition labels and filter
d.tc.summary$condition_plot = case_when(
  d.tc.summary$condition == 'neutral' ~ "$0",
  d.tc.summary$condition == 'gain' ~ "+$5"
)
nacc2_block_data = d.tc.summary[!is.na(d.tc.summary$condition_plot), ]

# Cap values at plot limits
nacc2_block_data$mean = pmax(-0.3, pmin(0.3, nacc2_block_data$mean))

### NAcc2 BLOCK TIMECOURSE PLOT ###
nacc2_block_plot = ggplot(nacc2_block_data, aes(x = tr, y = mean, group = condition_plot)) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = 0.25, colour = 'grey60') +
  geom_line(aes(linetype = condition_plot, colour = condition_plot), size = 1.5) +
  geom_point(aes(fill = condition_plot), color = "black", shape = 21, size = 3) +
  scale_linetype_manual(values = cond_lines) +
  scale_colour_manual(values = cond_colors) +
  scale_fill_manual(values = cond_fills) +
  scale_x_continuous(breaks = seq(0, 10, by = 2), labels = seq(0, 20, by = 4)) +
  scale_y_continuous(breaks = seq(-0.3, 0.3, by = 0.1), 
                     labels = c("-0.3", "-0.2", "-0.1", "0", "0.1", "0.2", "0.3"),
                     limits = c(-0.3, 0.3)) +
  facet_grid(. ~ block) +
  theme_minimal() +
  theme(text = element_text(size = 12, colour = 'black'),
        panel.grid.minor = element_blank(),
        legend.position = "bottom") +
  xlab('Seconds') +
  ylab('Percent Signal Change') +
  ggtitle('NAcc Subject Space Timecourses by block')

print(nacc2_block_plot)

# Save NAcc2 block timecourse data
write.csv(nacc2_block_data, '/Users/nate/Desktop/subspace/nacc_timecourses/nacc2_block_timecourse_data.csv', row.names = F)

### NAcc2 BLOCK ICC CALCULATION ###

# Prepare ICC data 
d.tc1$ses = "s1"
d.tc2$ses = "s2"
df_combined = rbind(d.tc1, d.tc2)

# Melt for ICC
df.long = melt(df_combined, measure.vars = nacc2_cols)
df.long$hemi = str_extract(df.long$variable, 'b|l|r')
df.long$condition_icc = case_when(
  df.long$condition == 'neutral' ~ "neutral",
  df.long$condition == 'gain' ~ "gain"
)
df.long = df.long[!is.na(df.long$condition_icc), ]

# Calculate means by subject
result = aggregate(value ~ subject + ses + condition_icc + hemi + tr + block, data = df.long, FUN = mean, na.rm = TRUE)

# Debug: Check what we have
print("Available data structure:")
print(head(result))
print("Unique sessions:", unique(result$ses))
print("Unique blocks:", unique(result$block))

# Create wider format for ICC calculation
dfinal = dcast(result, subject ~ ses + condition_icc + hemi + tr + block, value.var = "value")

# Debug: Check column names
print("Column names in dfinal:")
print(colnames(dfinal)[1:10])  # Show first 10 column names

# CORRECTED ICC CALCULATION 

icc_results = data.frame()

for(condition in c("gain", "neutral")) {
  for(hemi in c("b", "l", "r")) {
    for(tr in 1:7) {
      
      # Block 1 vs Block 3 comparison (Session 1 Block 1 vs Session 2 Block 3)
      # Note: blocks are named "b1", "b2", "b3", "b4" in the actual data
      s1_b1_col = paste("s1", condition, hemi, tr, "b1", sep = "_")
      s2_b3_col = paste("s2", condition, hemi, tr, "b3", sep = "_")
      
      if(s1_b1_col %in% colnames(dfinal) && s2_b3_col %in% colnames(dfinal)) {
        # Remove rows with NA values
        valid_rows = !is.na(dfinal[[s1_b1_col]]) & !is.na(dfinal[[s2_b3_col]])
        if(sum(valid_rows) >= 3) {  # Need at least 3 subjects for ICC
          tryCatch({
            icc_data = dfinal[valid_rows, c(s1_b1_col, s2_b3_col)]
            icc_result = ICC(icc_data)
            icc_results = rbind(icc_results, data.frame(
              comparison = "b1 vs b3",
              condition = condition,
              hemi = hemi,
              tr = tr,
              ICC = icc_result$results[6,2],
              ICC_lower = icc_result$results[6,7],
              ICC_upper = icc_result$results[6,8],
              n_subjects = sum(valid_rows)
            ))
            print(paste("Success: b1 vs b3", condition, hemi, "TR", tr, "ICC =", round(icc_result$results[6,2], 3)))
          }, error = function(e) {
            print(paste("Error in b1 vs b3", condition, hemi, "TR", tr, ":", e$message))
          })
        } else {
          print(paste("Not enough valid subjects for b1 vs b3", condition, hemi, "TR", tr, ": only", sum(valid_rows)))
        }
      } else {
        print(paste("Missing columns for b1 vs b3:", s1_b1_col, "or", s2_b3_col))
      }
      
      # Block 2 vs Block 4 comparison (Session 1 Block 2 vs Session 2 Block 4)
      s1_b2_col = paste("s1", condition, hemi, tr, "b2", sep = "_")
      s2_b4_col = paste("s2", condition, hemi, tr, "b4", sep = "_")
      
      if(s1_b2_col %in% colnames(dfinal) && s2_b4_col %in% colnames(dfinal)) {
        valid_rows = !is.na(dfinal[[s1_b2_col]]) & !is.na(dfinal[[s2_b4_col]])
        if(sum(valid_rows) >= 3) {
          tryCatch({
            icc_data = dfinal[valid_rows, c(s1_b2_col, s2_b4_col)]
            icc_result = ICC(icc_data)
            icc_results = rbind(icc_results, data.frame(
              comparison = "b2 vs b4",
              condition = condition,
              hemi = hemi,
              tr = tr,
              ICC = icc_result$results[6,2],
              ICC_lower = icc_result$results[6,7],
              ICC_upper = icc_result$results[6,8],
              n_subjects = sum(valid_rows)
            ))
            print(paste("Success: b2 vs b4", condition, hemi, "TR", tr, "ICC =", round(icc_result$results[6,2], 3)))
          }, error = function(e) {
            print(paste("Error in b2 vs b4", condition, hemi, "TR", tr, ":", e$message))
          })
        } else {
          print(paste("Not enough valid subjects for b2 vs b4", condition, hemi, "TR", tr, ": only", sum(valid_rows)))
        }
      } else {
        print(paste("Missing columns for b2 vs b4:", s1_b2_col, "or", s2_b4_col))
      }
    }
  }
}

# Check if we got any results
print(paste("Total ICC results calculated:", nrow(icc_results)))

if(nrow(icc_results) > 0) {
  # Show some example results
  print("Example ICC results:")
  print(head(icc_results))
  
  # Average across conditions and hemispheres for plotting
  icc_summary = icc_results %>%
    group_by(comparison, tr) %>%
    summarise(
      mean_ICC = mean(ICC, na.rm = TRUE),
      mean_lower = mean(ICC_lower, na.rm = TRUE),
      mean_upper = mean(ICC_upper, na.rm = TRUE),
      n_comparisons = n(),
      .groups = 'drop'
    ) %>%
    mutate(
      mean_lower = pmax(0, mean_lower),
      mean_upper = pmin(1, mean_upper)
    )
  
  print("ICC summary for plotting:")
  print(icc_summary)
  
  # Show TR 3 values specifically
  tr3_results = icc_summary[icc_summary$tr == 3, ]
  print("TR 3 ICC values:")
  print(tr3_results)
  
  # NAcc2 Block ICC plot
  nacc2_block_icc_plot = ggplot(icc_summary, aes(x = tr, y = mean_ICC, group = comparison)) +
    geom_line(aes(colour = comparison, linetype = comparison), linewidth = 1) +
    geom_errorbar(aes(ymin = mean_lower, ymax = mean_upper, colour = comparison), width = 0.3) +
    geom_point(aes(colour = comparison, fill = comparison), shape = 21, size = 3) +
    scale_linetype_manual(values = c("b1 vs b3" = "solid", "b2 vs b4" = "dashed")) +
    scale_color_manual(values = c("b1 vs b3" = "black", "b2 vs b4" = "grey50")) +
    scale_fill_manual(values = c("b1 vs b3" = "black", "b2 vs b4" = "white")) +
    scale_x_continuous(breaks = seq(1, 7, by = 1)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1.05)) +
    theme_minimal() +
    theme(text = element_text(size = 12, colour = 'black'),
          panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    xlab('Task timing (TR = 2 sec)') +
    ylab('Reliability ICC(3,k)') +
    ggtitle('NAcc Subject Space ICC: Cross-Session Block Reliability')
  
  print(nacc2_block_icc_plot)
  
  # Save data
  write.csv(icc_results, '/Users/nate/Desktop/subspace/nacc_timecourses/nacc2_block_icc_raw_data.csv', row.names = F)
  write.csv(icc_summary, '/Users/nate/Desktop/subspace/nacc_timecourses/nacc2_block_icc_summary_data.csv', row.names = F)
  
} else {
  print("ERROR: No ICC results calculated! Check the debug output above.")
  
  # Additional debugging - show what columns actually exist
  print("Available columns that contain 's1_gain_b_3':")
  print(colnames(dfinal)[grep("s1_gain_b_3", colnames(dfinal))])
  
  print("Available columns that contain 's2_gain_b_3':")
  print(colnames(dfinal)[grep("s2_gain_b_3", colnames(dfinal))])
}
### NAcc2 HIT/MISS PLOTS ###

# Filter for hit/miss data
d.tc1.hm = d.tc1[!is.na(d.tc1$hit), ]
d.tc2.hm = d.tc2[!is.na(d.tc2$hit), ]
d.tc1.hm$outcome = ifelse(d.tc1.hm$hit == 1, "Hit", "Miss")
d.tc2.hm$outcome = ifelse(d.tc2.hm$hit == 1, "Hit", "Miss")
d.combined.hm = rbind(d.tc1.hm, d.tc2.hm)

# Process hit/miss timecourse data
d.tc.melt.hm = melt(d.combined.hm, id=c('tr','condition','subject','outcome','session'), measure.vars = nacc2_cols)
d.tc.subject.hm = ddply(d.tc.melt.hm, .variables = c('tr','condition','subject','outcome','session'),
                        summarise, mean.subject = mean(value, na.rm = TRUE))
d.tc.summary.hm = ddply(d.tc.subject.hm, .variables = c('tr','condition','outcome','session'),
                        function(x) data.frame(
                          mean = mean(x$mean.subject, na.rm = TRUE),
                          sem = sd(x$mean.subject, na.rm = TRUE)/sqrt(length(x$mean.subject))
                        ))

# Add condition labels
d.tc.summary.hm$condition_plot = case_when(
  d.tc.summary.hm$condition == 'neutral' ~ "$0",
  d.tc.summary.hm$condition == 'gain' ~ "+$5"
)
nacc2_hm_data = d.tc.summary.hm[!is.na(d.tc.summary.hm$condition_plot), ]

# Cap values
nacc2_hm_data$mean = pmax(-0.3, pmin(0.3, nacc2_hm_data$mean))

# NAcc2 Hit/Miss timecourse plot
nacc2_hm_plot = ggplot(nacc2_hm_data, aes(x = tr, y = mean, group = condition_plot)) +
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = 0.25, colour = 'grey60') +
  geom_line(aes(linetype = condition_plot, colour = condition_plot), size = 1.5) +
  geom_point(aes(fill = condition_plot), color = "black", shape = 21, size = 3) +
  scale_linetype_manual(values = cond_lines) +
  scale_colour_manual(values = cond_colors) +
  scale_fill_manual(values = cond_fills) +
  scale_x_continuous(breaks = seq(0, 10, by = 2), labels = seq(0, 20, by = 4)) +
  scale_y_continuous(breaks = seq(-0.3, 0.3, by = 0.1), 
                     labels = c("-0.3", "-0.2", "-0.1", "0", "0.1", "0.2", "0.3"),
                     limits = c(-0.3, 0.3)) +
  facet_grid(outcome ~ session) +
  theme_minimal() +
  theme(text = element_text(size = 12, colour = 'black'),
        panel.grid.minor = element_blank(),
        legend.position = "bottom") +
  xlab('Seconds') +
  ylab('Percent Signal Change') +
  ggtitle('NAcc Subject Space Timecourses by Outcome and Session')

print(nacc2_hm_plot)

# Save NAcc2 hit/miss timecourse data
write.csv(nacc2_hm_data, '/Users/nate/Desktop/subspace/nacc_timecourses/nacc2_hitmiss_timecourse_data.csv', row.names = F)

### NAcc2 HIT/MISS ICC CALCULATION ###

# NAcc2 Hit/Miss ICC
df.long.nacc2.hm = melt(d.combined.hm, measure.vars = nacc2_cols)
df.long.nacc2.hm$hemi = str_extract(df.long.nacc2.hm$variable, 'b|l|r')
df.long.nacc2.hm$condition_icc = case_when(
  df.long.nacc2.hm$condition == 'neutral' ~ "neutral",
  df.long.nacc2.hm$condition == 'gain' ~ "gain"
)
df.long.nacc2.hm = df.long.nacc2.hm[!is.na(df.long.nacc2.hm$condition_icc), ]

# Calculate means by subject for NAcc2 hit/miss
result.nacc2.hm = aggregate(value ~ subject + ses + condition_icc + hemi + tr + outcome, data = df.long.nacc2.hm, FUN = mean, na.rm = TRUE)
dfinal.nacc2.hm = dcast(result.nacc2.hm, subject ~ ses + condition_icc + hemi + tr + outcome, value.var = "value")

# Calculate NAcc2 hit/miss ICC
icc_results.nacc2.hm = data.frame()
hm_groups = c("Hit", "Miss")

for(group in hm_groups) {
  for(condition in c("gain", "neutral")) {
    for(hemi in c("b", "l", "r")) {
      for(tr in 1:7) {
        s1_col = paste("s1", condition, hemi, tr, group, sep = "_")
        s2_col = paste("s2", condition, hemi, tr, group, sep = "_")
        
        if(s1_col %in% colnames(dfinal.nacc2.hm) && s2_col %in% colnames(dfinal.nacc2.hm)) {
          tryCatch({
            icc_result = ICC(dfinal.nacc2.hm[, c(s1_col, s2_col)])
            icc_results.nacc2.hm = rbind(icc_results.nacc2.hm, data.frame(
              outcome = group,
              condition = condition,
              tr = tr,
              ICC = icc_result$results[6,2],
              ICC_lower = icc_result$results[6,7],
              ICC_upper = icc_result$results[6,8]
            ))
          }, error = function(e) {})
        }
      }
    }
  }
}

if(nrow(icc_results.nacc2.hm) > 0) {
  # Average across conditions and hemispheres for NAcc2 hit/miss
  icc_summary.nacc2.hm = icc_results.nacc2.hm %>%
    group_by(outcome, tr) %>%
    summarise(
      mean_ICC = mean(ICC, na.rm = TRUE),
      mean_lower = mean(ICC_lower, na.rm = TRUE),
      mean_upper = mean(ICC_upper, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(
      mean_lower = pmax(0, mean_lower),
      mean_upper = pmin(1, mean_upper)
    )
  
  # NAcc2 Hit/Miss ICC plot
  nacc2_hm_icc_plot = ggplot(icc_summary.nacc2.hm, aes(x = tr, y = mean_ICC, group = outcome)) +
    geom_line(aes(colour = outcome, linetype = outcome), linewidth = 1) +
    geom_errorbar(aes(ymin = mean_lower, ymax = mean_upper, colour = outcome), width = 0.3) +
    geom_point(aes(colour = outcome, fill = outcome), shape = 21, size = 3) +
    scale_linetype_manual(values = c("Hit" = "solid", "Miss" = "dashed")) +
    scale_color_manual(values = c("Hit" = "black", "Miss" = "grey50")) +
    scale_fill_manual(values = c("Hit" = "black", "Miss" = "white")) +
    scale_x_continuous(breaks = seq(1, 7, by = 1)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1.05)) +
    theme_minimal() +
    theme(text = element_text(size = 12, colour = 'black'),
          panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    xlab('Task timing (TR = 2 sec)') +
    ylab('Reliability ICC(3,k)') +
    ggtitle('NAcc Subject Space ICC: Hit vs Miss Reliability')
  
  print(nacc2_hm_icc_plot)
  
  # Save NAcc2 hit/miss ICC data
  write.csv(icc_results.nacc2.hm, '/Users/nate/Desktop/subspace/nacc_timecourses/nacc2_hitmiss_icc_raw_data.csv', row.names = F)
  write.csv(icc_summary.nacc2.hm, '/Users/nate/Desktop/subspace/nacc_timecourses/nacc2_hitmiss_icc_summary_data.csv', row.names = F)
}


# Save all plots
ggsave("/Users/nate/Desktop/plots/nacc2_block_timecourses.png", plot = nacc2_block_plot, width = 8, height = 6, dpi = 300)
ggsave("/Users/nate/Desktop/plots/nacc2_block_icc.png", plot = nacc2_block_icc_plot, width = 8, height = 6, dpi = 300)
ggsave("/Users/nate/Desktop/plots/nacc2_hitmiss_timecourses.png", plot = nacc2_hm_plot, width = 8, height = 6, dpi = 300)
ggsave("/Users/nate/Desktop/plots/nacc2_hitmiss_icc.png", plot = nacc2_hm_icc_plot, width = 8, height = 6, dpi = 300)

