# MIL Viewing period

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

d1 = data.frame()
for (subj in 1:length(subvec)){
  loadfilename = str_c('/Users/nathanielbouton/Documents/UArk/FANlab/Test-Retest/MIL/data/',subvec[subj],'_ses1_tc10tr.csv')
  d0 = read.csv(loadfilename, head=T)
  d0$subject = subvec[subj]
  d1 = rbind(d1,d0)
}

d2 = data.frame()
for (subj in 1:length(subvec)){
  loadfilename = str_c('/Users/nathanielbouton/Documents/UArk/FANlab/Test-Retest/MIL/data/',subvec[subj],'_ses2_tc10tr.csv')
  d0 = read.csv(loadfilename, head=T)
  d0$subject = subvec[subj]
  d2 = rbind(d2,d0)
}

# add column for "ses" to both ds1 and ds2. fill in the column with session number.
d1$ses = rep('s1',nrow(d1))
d2$ses = rep('s2',nrow(d2))

# Modify column names within the specified range
colnames(d1)[24:68] = str_sub(colnames(d1)[24:68], 1, -5)
colnames(d2)[24:68] = str_sub(colnames(d2)[24:68], 1, -5)

# rbind the 2 sessions data frames together
df = rbind(d1,d2)

# create new csv to load in for choice script
write.csv(df, '/Users/nathanielbouton/Documents/UArk/FANlab/Test-Retest/MIL/mil_subjects_ses1&2.csv', row.names = F)

# Subset the data to include only TR 3 and TR 4
df_subset <- df[df$tr %in% c(3, 4), ]

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
df.long = melt(d_tr1, measure.vars = (brain_regions))

# Create a new column called region based on the variable column
df.long = df.long %>%
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
result <- aggregate(value ~ subject + fractals + region + hemi + ses + tr,
                    data = df.long, FUN = mean)

#This is tidyverse version of code above but didn't work.
#result <- df.long %>%
  #group_by(subject, fractals, region, hemi) %>% 
  #summarise(mean_activity = mean(value))


# go to wide form
# you should end up with a very wide dataframe
# each column will be brain activity for an ROI_hemi_fractal

# Use dcast to convert it to wide form
dfinal = dcast(result, subject ~ region + hemi + fractals + ses + tr,
               fun.aggregate = mean,
               value.var = "value")


require(psych)

# nacc reliability
ICC(dfinal[, c("nacc_b_loss1_s1_3", "nacc_b_loss1_s2_3")])
ICC(dfinal[, c("nacc_b_loss2_s1_3", "nacc_b_loss2_s2_3")])
ICC(dfinal[, c("nacc_b_neut1_s1_3", "nacc_b_neut1_s2_3")])
ICC(dfinal[, c("nacc_b_neut2_s1_3", "nacc_b_neut2_s2_3")])
ICC(dfinal[, c("nacc_b_gain1_s1_3", "nacc_b_gain1_s2_3")])
ICC(dfinal[, c("nacc_b_gain2_s1_3", "nacc_b_gain2_s2_3")])
