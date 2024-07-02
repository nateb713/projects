# MIL Choice

library(tidyverse)
library(reshape2)
library(stringr)
library(extrafont)
library(plyr)
library(dplyr)

# load data for each subject, rbind into 1 data frame. separate sessions.
subvec = c('rrl01','rrl02','rrl03','rrl04','rrl05','rrl06','rrl07',
           'rrl10','rrl11','rrl12','rrl13','rrl14','rrl15','rrl16',
           'rrl17','rrl18','rrl19','rrl20','rrl22','rrl23')

d <- read.csv('/Users/nathanielbouton/Documents/UArk/FANlab/Test-Retest/MIL/mil_subjects_ses1&2.csv')

# Subset the data to include only tr 6 through 10
df_subset <- d[d$tr %in% c(6:10), ]


# Initialize empty vectors to store better fractal values for each condition
better_fractals <- list(neutral = character(length(subvec)),
                        loss = character(length(subvec)),
                        gain = character(length(subvec)))

# Loop over subjects
for (i in 1:length(subvec)) {
  subj <- subvec[i]
  
  # Subset by TR == 1 for the current subject
  d_tr1 <- subset(df_subset[df_subset$TR == 1, ])
  
  # Subset by condition (1=neutral, 2=loss, 3=gain)
  d.1 <- subset(d_tr1, trialtype == 1)
  d.2 <- subset(d_tr1, trialtype == 2)
  d.3 <- subset(d_tr1, trialtype == 3)
  
  # Find first trial that subject picked "better" fractal for each condition
  d.1_bp <- d.1[which(d.1$hit == 1)[1], 'bp']
  d.1_better <- d.1[which(d.1$hit == 1)[1], paste0(tolower(d.1_bp), '_cue_value')]
  better_fractals$neutral[i] <- d.1_better
  
  d.2_bp <- d.2[which(d.2$hit == 1)[1], 'bp']
  d.2_better <- d.2[which(d.2$hit == 1)[1], paste0(tolower(d.2_bp), '_cue_value')]
  better_fractals$loss[i] <- d.2_better
  
  d.3_bp <- d.3[which(d.3$hit == 1)[1], 'bp']
  d.3_better <- d.3[which(d.3$hit == 1)[1], paste0(tolower(d.3_bp), '_cue_value')]
  better_fractals$gain[i] <- d.3_better
}


# Create the fractals column using case_when
d_tr1$fractals <- case_when(
  d_tr1$trialtype == 1 & d_tr1[[paste0(tolower(d_tr1$bp[1]), "_cue_value")]] == better_fractals$neutral ~ "neut1",
  d_tr1$trialtype == 1 & d_tr1[[paste0(tolower(d_tr1$bp[1]), "_cue_value")]] != better_fractals$neutral ~ "neut2",
  d_tr1$trialtype == 2 & d_tr1[[paste0(tolower(d_tr1$bp[1]), "_cue_value")]] == better_fractals$loss ~ "loss1",
  d_tr1$trialtype == 2 & d_tr1[[paste0(tolower(d_tr1$bp[1]), "_cue_value")]] != better_fractals$loss ~ "loss2",
  d_tr1$trialtype == 3 & d_tr1[[paste0(tolower(d_tr1$bp[1]), "_cue_value")]] == better_fractals$gain ~ "gain1",
  d_tr1$trialtype == 3 & d_tr1[[paste0(tolower(d_tr1$bp[1]), "_cue_value")]] != better_fractals$gain ~ "gain2",
  TRUE ~ NA_character_
)

# melt
# Define a vector containing the column names representing brain regions
brain_regions = colnames(d_tr1[24:68])

# Use the vector in the melt function
df.long <- melt(d_tr1, measure.vars = (brain_regions))

# Create a new column called region based on the variable column
df.long <- df.long %>%
  mutate(region = case_when(
    grepl("acing", variable) ~ "acing",
    grepl("ains8mmkg", variable) ~ "ains_kg",
    grepl("ains8mm", variable) ~ "ains",
    grepl("ains_mni", variable) ~ "ains_mni",
    grepl("caudate", variable) ~ "caudate",
    grepl("csf", variable) ~ "csf",
    grepl("dlpfc", variable) ~ "dlpfc",
    grepl("ins", variable) ~ "ins",
    grepl("mpfc8mm_mni", variable) ~ "mpfc_mni",
    grepl("mpfc_s", variable) ~ "mpfc",
    grepl("nacc8mm_mni", variable) ~ "nacc_mni",
    grepl("nacc8mm_s", variable) ~ "nacc",
    grepl("vta_mni_10", variable) ~ "vta_mni_10",
    grepl("vta_mni_50", variable) ~ "vta_mni_50",
    grepl("wm", variable) ~ "wm",
    TRUE ~ NA_character_
  ))



df.long$hemi <- ifelse(str_detect(df.long$variable, 'bmil'), 'b',
                       ifelse(str_detect(df.long$variable, 'rmil'), 'r',
                              ifelse(str_detect(df.long$variable, 'lmil'), 'l', NA)))



# calculate mean activity
# piping the data into the summarize mean activity function by every combination of those experimental variables.

# Group by subject, fractals, region, and hemi, and calculate mean activity
result <- aggregate(value ~ subject + fractals + region + hemi + bp + hit + trial_gain
                    + ses,
                    data = df.long, FUN = mean)


# Use dcast to convert it to wide form
dfinal <- dcast(result, subject ~ region + hemi + fractals + bp + hit + trial_gain
               + ses,
               fun.aggregate = mean,
               value.var = "value")


# nacc reliability
require(psych)

ICC(dfinal[, c("nacc_b_gain1_L_1_+$5.00_s1", "nacc_b_gain1_L_1_+$5.00_s2")])
ICC(dfinal[, c("nacc_b_gain2_L_1_+$5.00_s1", "nacc_b_gain2_L_1_+$5.00_s2")])

ICC(dfinal[, c("nacc_b_gain1_L_0_+$5.00_s1", "nacc_b_gain1_L_0_+$5.00_s2")])
ICC(dfinal[, c("nacc_b_gain2_L_0_+$5.00_s1", "nacc_b_gain2_L_0_+$5.00_s2")])

ICC(dfinal[, c("nacc_b_gain1_R_0_+$5.00_s1", "nacc_b_gain1_R_0_+$5.00_s2")])
ICC(dfinal[, c("nacc_b_gain2_R_0_+$5.00_s1", "nacc_b_gain2_R_0_+$5.00_s2")])

ICC(dfinal[, c("nacc_b_gain1_R_1_+$5.00_s1", "nacc_b_gain1_R_1_+$5.00_s2")])
ICC(dfinal[, c("nacc_b_gain2_R_1_+$5.00_s1", "nacc_b_gain2_R_1_+$5.00_s2")])
