require('stringr')
require('reshape2')
require(ggplot2)
require(reshape2)
require(extrafont)
require(plyr)
library(tidyverse)
library(dplyr)
library(irr)

# load data for each subject, rbind into 1 data frame. separate sessions.
subvec = c('rrl01','rrl02','rrl03','rrl04','rrl05','rrl06','rrl07',
           'rrl10','rrl11','rrl12','rrl13','rrl14','rrl15','rrl16',
           'rrl17','rrl18','rrl19','rrl22','rrl23')

d1 = data.frame()
for (subj in 1:length(subvec)){
  loadfilename = str_c('/Users/nathanielbouton/Documents/UArk/FANlab/Test-Retest/MID/data/',subvec[subj],'_ses1_tc10tr.csv')
  d0 = read.csv(loadfilename, head=T)
  d0$subject = subvec[subj]
  d1 = rbind(d1,d0)
}

write.csv(d1, '/Users/nathanielbouton/Documents/UArk/FANlab/Test-Retest/MID/data/mid_ses1_tcs_n19.csv', row.names = F)


d2 = data.frame()
for (subj in 1:length(subvec)){
  loadfilename = str_c('/Users/nathanielbouton/Documents/UArk/FANlab/Test-Retest/MID/data/',subvec[subj],'_ses2_tc10tr.csv')
  d0 = read.csv(loadfilename, head=T)
  d0$subject = subvec[subj]
  d2 = rbind(d2,d0)
}

write.csv(d2, '/Users/nathanielbouton/Documents/UArk/FANlab/Test-Retest/MID/data/mid_ses2_tcs_n19.csv', row.names = F)


# add column for "ses" to both ds1 and ds2. fill in the column with session number.
d1$ses = rep('s1',nrow(d1))
d2$ses = rep('s2',nrow(d2))


# Modify column names within the specified range
colnames(d1)[19:63] = str_sub(colnames(d1)[19:63], 1, -5)
colnames(d2)[19:63] = str_sub(colnames(d2)[19:63], 1, -5)

# Add condition column and rename 1, 2, and 3 to neutral, loss, and gain.
d1 <- d1 %>%
  mutate(condition = case_when(
    trialtype == 1 ~ "neutral",
    trialtype == 2 ~ "loss",
    trialtype == 3 ~ "gain",
  ))  %>%
  select(-trialtype)

d2 <- d2 %>%
  mutate(condition = case_when(
    trialtype == 1 ~ "neutral",
    trialtype == 2 ~ "loss",
    trialtype == 3 ~ "gain",
  ))  %>%
  select(-trialtype)

# rbind the 2 sessions data frames together
df = rbind(d1,d2)



# melt
# Define a vector containing the column names representing brain regions
brain_regions = colnames(df[18:62])

# Use the vector in the melt function

df.long = melt(df, measure.vars = (brain_regions))

#some string extraction to only have "nacc" and "nacc_mni", and all other ROIs
# Extract substrings based on specific patterns

df.long$region = str_extract(df.long$variable, 'acing|ains8mmkg|ains8mm|
                             ains_mni|caudate|csf|dlpfc|ins|
                             mpfc8mm_mni|mpfc|nacc8mm_mni|nacc8mm|
                             vta_mni_10|vta_mni_50|wm')

tmp1 = str_sub(df.long$variable, 11, )
tmp2 = str_sub(tmp1, 1,-3)

# df.long$region <- str_extract(df.long$variable, '\\b(acing|ains8mmkg|ains8mm|ains_mni|caudate|csf|dlpfc|ins|mpfc8mm_mni|mpfc|nacc8mm_mni|nacc8mm|vta_mni_10|vta_mni_50|wm)\\b')


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
    grepl("mpfc", variable) ~ "mpfc",
    grepl("nacc8mm_mni", variable) ~ "nacc_mni",
    grepl("nacc8mm", variable) ~ "nacc",
    grepl("vta_mni_10", variable) ~ "vta_mni_10",
    grepl("vta_mni_50", variable) ~ "vta_mni_50",
    grepl("wm", variable) ~ "wm",
    TRUE ~ NA_character_
  ))



df.long$hemi = str_extract(df.long$variable, 'b|l|r')
#df.long$ses = str_extract(df.long$ses, 's1|s2')
#df.long$condition = str_extract(df.long$condition, 'neutral|loss|gain')
#df.long$tr = str_extract(df.long$tr, '1|2|3|4|5|6|7|8|9|10')

# If a variable doesn't match any pattern, it will be assigned NA

# calculate mean NAcc activity by (ses, hemisphere, condition, tr)
# piping the data into the summarize mean activity function by every combination of those experimental variables.

result <- df.long %>%
  group_by(subject, region, value, ses, condition, hemi, tr) %>%
  summarise(mean_activity = mean(value))


# go to wide form
# you should end up with a very wide dataframe
# 19 rows (bc 19 subjects)
# each column will be brain activity for an ROI_hemi_ses_trialtype_tr

# Use dcast to convert it to wide form
dfinal <- dcast(result, subject ~ region + ses + condition
                     + hemi + tr, fun.aggregate = mean,
                     value.var = "mean_activity")

dtmp <- dcast(result, subject ~ ses + region + hemi + condition + tr, 
              fun.aggregate = mean,
              value.var = "mean_activity")


# then we can run ICC between any 2 variables under the sun
# region_ses_condition_hemi_tr
require(psych)
# nacc gain anticipation reliability
ICC(dfinal[, c("nacc_s1_gain_b_3", "nacc_s2_gain_b_3")])
ICC(dfinal[, c("nacc_s1_gain_b_4", "nacc_s2_gain_b_4")])
ICC(dfinal[, c("nacc_s1_gain_b_5", "nacc_s2_gain_b_5")])
ICC(dfinal[, c("nacc_s1_gain_b_6", "nacc_s2_gain_b_6")])
ICC(dfinal[, c("nacc_s1_gain_b_7", "nacc_s2_gain_b_7")])
ICC(dfinal[, c("nacc_s1_gain_l_3", "nacc_s2_gain_l_3")])
ICC(dfinal[, c("nacc_s1_gain_l_4", "nacc_s2_gain_l_4")])
ICC(dfinal[, c("nacc_s1_gain_l_5", "nacc_s2_gain_l_5")])
ICC(dfinal[, c("nacc_s1_gain_l_6", "nacc_s2_gain_l_6")])
ICC(dfinal[, c("nacc_s1_gain_l_7", "nacc_s2_gain_l_7")])
ICC(dfinal[, c("nacc_s1_gain_r_3", "nacc_s2_gain_r_3")])
ICC(dfinal[, c("nacc_s1_gain_r_4", "nacc_s2_gain_r_4")])
ICC(dfinal[, c("nacc_s1_gain_r_5", "nacc_s2_gain_r_5")])
ICC(dfinal[, c("nacc_s1_gain_r_6", "nacc_s2_gain_r_6")])
ICC(dfinal[, c("nacc_s1_gain_r_7", "nacc_s2_gain_r_7")])

ICC(dfinal[, c("nacc_mni_s1_gain_b_3", "nacc_mni_s2_gain_b_3")])
ICC(dfinal[, c("nacc_mni_s1_gain_b_4", "nacc_mni_s2_gain_b_4")])
ICC(dfinal[, c("nacc_mni_s1_gain_l_3", "nacc_mni_s2_gain_l_3")])
ICC(dfinal[, c("nacc_mni_s1_gain_l_4", "nacc_mni_s2_gain_l_4")])
ICC(dfinal[, c("nacc_mni_s1_gain_r_3", "nacc_mni_s2_gain_r_3")])
ICC(dfinal[, c("nacc_mni_s1_gain_r_4", "nacc_mni_s2_gain_r_4")])


# nacc loss anticipation reliability
ICC(dfinal[, c("nacc_s1_loss_b_3", "nacc_s2_loss_b_3")])
ICC(dfinal[, c("nacc_s1_loss_b_4", "nacc_s2_loss_b_4")])
ICC(dfinal[, c("nacc_s1_loss_l_3", "nacc_s2_loss_l_3")])
ICC(dfinal[, c("nacc_s1_loss_l_4", "nacc_s2_loss_l_4")])
ICC(dfinal[, c("nacc_s1_loss_r_3", "nacc_s2_loss_r_3")])
ICC(dfinal[, c("nacc_s1_loss_r_4", "nacc_s2_loss_r_4")])

ICC(dfinal[, c("nacc_mni_s1_loss_b_3", "nacc_mni_s2_loss_b_3")])
ICC(dfinal[, c("nacc_mni_s1_loss_b_4", "nacc_mni_s2_loss_b_4")])
ICC(dfinal[, c("nacc_mni_s1_loss_l_3", "nacc_mni_s2_loss_l_3")])
ICC(dfinal[, c("nacc_mni_s1_loss_l_4", "nacc_mni_s2_loss_l_4")])
ICC(dfinal[, c("nacc_mni_s1_loss_r_3", "nacc_mni_s2_loss_r_3")])
ICC(dfinal[, c("nacc_mni_s1_loss_r_4", "nacc_mni_s2_loss_r_4")])

# nacc neutral anticipation reliability
ICC(dfinal[, c("nacc_s1_neutral_b_3", "nacc_s2_neutral_b_3")])
ICC(dfinal[, c("nacc_s1_neutral_b_4", "nacc_s2_neutral_b_4")])
ICC(dfinal[, c("nacc_s1_neutral_l_3", "nacc_s2_neutral_l_3")])
ICC(dfinal[, c("nacc_s1_neutral_l_4", "nacc_s2_neutral_l_4")])
ICC(dfinal[, c("nacc_s1_neutral_r_3", "nacc_s2_neutral_r_3")])
ICC(dfinal[, c("nacc_s1_neutral_r_4", "nacc_s2_neutral_r_4")])

ICC(dfinal[, c("nacc_mni_s1_neutral_b_3", "nacc_mni_s2_neutral_b_3")])
ICC(dfinal[, c("nacc_mni_s1_neutral_b_4", "nacc_mni_s2_neutral_b_4")])
ICC(dfinal[, c("nacc_mni_s1_neutral_l_3", "nacc_mni_s2_neutral_l_3")])
ICC(dfinal[, c("nacc_mni_s1_neutral_l_4", "nacc_mni_s2_neutral_l_4")])
ICC(dfinal[, c("nacc_mni_s1_neutral_r_3", "nacc_mni_s2_neutral_r_3")])
ICC(dfinal[, c("nacc_mni_s1_neutral_r_4", "nacc_mni_s2_neutral_r_4")])


# ains gain anticipation reliability
ICC(dfinal[, c("ains_s1_gain_b_3", "ains_s2_gain_b_3")])
ICC(dfinal[, c("ains_s1_gain_b_4", "ains_s2_gain_b_4")])
ICC(dfinal[, c("ains_s1_gain_l_3", "ains_s2_gain_l_3")])
ICC(dfinal[, c("ains_s1_gain_l_4", "ains_s2_gain_l_4")])
ICC(dfinal[, c("ains_s1_gain_r_3", "ains_s2_gain_r_3")])
ICC(dfinal[, c("ains_s1_gain_r_4", "ains_s2_gain_r_4")])

ICC(dfinal[, c("ains_mni_s1_gain_b_3", "ains_mni_s2_gain_b_3")])
ICC(dfinal[, c("ains_mni_s1_gain_b_4", "ains_mni_s2_gain_b_4")])
ICC(dfinal[, c("ains_mni_s1_gain_l_3", "ains_mni_s2_gain_l_3")])
ICC(dfinal[, c("ains_mni_s1_gain_l_4", "ains_mni_s2_gain_l_4")])
ICC(dfinal[, c("ains_mni_s1_gain_r_3", "ains_mni_s2_gain_r_3")])
ICC(dfinal[, c("ains_mni_s1_gain_r_4", "ains_mni_s2_gain_r_4")])

ICC(dfinal[, c("ains_kg_s1_gain_b_3", "ains_kg_s2_gain_b_3")])
ICC(dfinal[, c("ains_kg_s1_gain_b_4", "ains_kg_s2_gain_b_4")])
ICC(dfinal[, c("ains_kg_s1_gain_l_3", "ains_kg_s2_gain_l_3")])
ICC(dfinal[, c("ains_kg_s1_gain_l_4", "ains_kg_s2_gain_l_4")])
ICC(dfinal[, c("ains_kg_s1_gain_r_3", "ains_kg_s2_gain_r_3")])
ICC(dfinal[, c("ains_kg_s1_gain_r_4", "ains_kg_s2_gain_r_4")])


# ains loss anticipation reliability
ICC(dfinal[, c("ains_s1_loss_b_3", "ains_s2_loss_b_3")])
ICC(dfinal[, c("ains_s1_loss_b_4", "ains_s2_loss_b_4")])
ICC(dfinal[, c("ains_s1_loss_l_3", "ains_s2_loss_l_3")])
ICC(dfinal[, c("ains_s1_loss_l_4", "ains_s2_loss_l_4")])
ICC(dfinal[, c("ains_s1_loss_r_3", "ains_s2_loss_r_3")])
ICC(dfinal[, c("ains_s1_loss_r_4", "ains_s2_loss_r_4")])

ICC(dfinal[, c("ains_mni_s1_loss_b_3", "ains_mni_s2_loss_b_3")])
ICC(dfinal[, c("ains_mni_s1_loss_b_4", "ains_mni_s2_loss_b_4")])
ICC(dfinal[, c("ains_mni_s1_loss_l_3", "ains_mni_s2_loss_l_3")])
ICC(dfinal[, c("ains_mni_s1_loss_l_4", "ains_mni_s2_loss_l_4")])
ICC(dfinal[, c("ains_mni_s1_loss_r_3", "ains_mni_s2_loss_r_3")])
ICC(dfinal[, c("ains_mni_s1_loss_r_4", "ains_mni_s2_loss_r_4")])

ICC(dfinal[, c("ains_kg_s1_loss_b_3", "ains_kg_s2_loss_b_3")])
ICC(dfinal[, c("ains_kg_s1_loss_b_4", "ains_kg_s2_loss_b_4")])
ICC(dfinal[, c("ains_kg_s1_loss_l_3", "ains_kg_s2_loss_l_3")])
ICC(dfinal[, c("ains_kg_s1_loss_l_4", "ains_kg_s2_loss_l_4")])
ICC(dfinal[, c("ains_kg_s1_loss_r_3", "ains_kg_s2_loss_r_3")])
ICC(dfinal[, c("ains_kg_s1_loss_r_4", "ains_kg_s2_loss_r_4")])


# ains neutral anticipation reliability
ICC(dfinal[, c("ains_s1_neutral_b_3", "ains_s2_neutral_b_3")])
ICC(dfinal[, c("ains_s1_neutral_b_4", "ains_s2_neutral_b_4")])
ICC(dfinal[, c("ains_s1_neutral_l_3", "ains_s2_neutral_l_3")])
ICC(dfinal[, c("ains_s1_neutral_l_4", "ains_s2_neutral_l_4")])
ICC(dfinal[, c("ains_s1_neutral_r_3", "ains_s2_neutral_r_3")])
ICC(dfinal[, c("ains_s1_neutral_r_4", "ains_s2_neutral_r_4")])

ICC(dfinal[, c("ains_mni_s1_neutral_b_3", "ains_mni_s2_neutral_b_3")])
ICC(dfinal[, c("ains_mni_s1_neutral_b_4", "ains_mni_s2_neutral_b_4")])
ICC(dfinal[, c("ains_mni_s1_neutral_l_3", "ains_mni_s2_neutral_l_3")])
ICC(dfinal[, c("ains_mni_s1_neutral_l_4", "ains_mni_s2_neutral_l_4")])
ICC(dfinal[, c("ains_mni_s1_neutral_r_3", "ains_mni_s2_neutral_r_3")])
ICC(dfinal[, c("ains_mni_s1_neutral_r_4", "ains_mni_s2_neutral_r_4")])

ICC(dfinal[, c("ains_kg_s1_neutral_b_3", "ains_kg_s2_neutral_b_3")])
ICC(dfinal[, c("ains_kg_s1_neutral_b_4", "ains_kg_s2_neutral_b_4")])
ICC(dfinal[, c("ains_kg_s1_neutral_l_3", "ains_kg_s2_neutral_l_3")])
ICC(dfinal[, c("ains_kg_s1_neutral_l_4", "ains_kg_s2_neutral_l_4")])
ICC(dfinal[, c("ains_kg_s1_neutral_r_3", "ains_kg_s2_neutral_r_3")])
ICC(dfinal[, c("ains_kg_s1_neutral_r_4", "ains_kg_s2_neutral_r_4")])

# vta gain anticipation reliability
ICC(dfinal[, c("vta_mni_50_s1_gain_b_3", "vta_mni_50_s2_gain_b_3")])
ICC(dfinal[, c("vta_mni_50_s1_gain_b_4", "vta_mni_50_s2_gain_b_4")])
ICC(dfinal[, c("vta_mni_50_s1_gain_b_5", "vta_mni_50_s2_gain_b_5")])
ICC(dfinal[, c("vta_mni_50_s1_gain_b_6", "vta_mni_50_s2_gain_b_6")])
ICC(dfinal[, c("vta_mni_50_s1_gain_b_7", "vta_mni_50_s2_gain_b_7")])
ICC(dfinal[, c("vta_mni_50_s1_gain_l_3", "vta_mni_50_s2_gain_l_3")])
ICC(dfinal[, c("vta_mni_50_s1_gain_l_4", "vta_mni_50_s2_gain_l_4")])
ICC(dfinal[, c("vta_mni_50_s1_gain_l_5", "vta_mni_50_s2_gain_l_5")])
ICC(dfinal[, c("vta_mni_50_s1_gain_l_6", "vta_mni_50_s2_gain_l_6")])
ICC(dfinal[, c("vta_mni_50_s1_gain_l_7", "vta_mni_50_s2_gain_l_7")])
ICC(dfinal[, c("vta_mni_50_s1_gain_r_3", "vta_mni_50_s2_gain_r_3")])
ICC(dfinal[, c("vta_mni_50_s1_gain_r_4", "vta_mni_50_s2_gain_r_4")])
ICC(dfinal[, c("vta_mni_50_s1_gain_r_5", "vta_mni_50_s2_gain_r_5")])
ICC(dfinal[, c("vta_mni_50_s1_gain_r_6", "vta_mni_50_s2_gain_r_6")])
ICC(dfinal[, c("vta_mni_50_s1_gain_r_7", "vta_mni_50_s2_gain_r_7")])

ICC(dfinal[, c("vta_mni_10_s1_gain_b_3", "vta_mni_10_s2_gain_b_3")])
ICC(dfinal[, c("vta_mni_10_s1_gain_b_4", "vta_mni_10_s2_gain_b_4")])
ICC(dfinal[, c("vta_mni_10_s1_gain_b_5", "vta_mni_10_s2_gain_b_5")])
ICC(dfinal[, c("vta_mni_10_s1_gain_b_6", "vta_mni_10_s2_gain_b_6")])
ICC(dfinal[, c("vta_mni_10_s1_gain_b_7", "vta_mni_10_s2_gain_b_7")])
ICC(dfinal[, c("vta_mni_10_s1_gain_l_3", "vta_mni_10_s2_gain_l_3")])
ICC(dfinal[, c("vta_mni_10_s1_gain_l_4", "vta_mni_10_s2_gain_l_4")])
ICC(dfinal[, c("vta_mni_10_s1_gain_l_5", "vta_mni_10_s2_gain_l_5")])
ICC(dfinal[, c("vta_mni_10_s1_gain_l_6", "vta_mni_10_s2_gain_l_6")])
ICC(dfinal[, c("vta_mni_10_s1_gain_l_7", "vta_mni_10_s2_gain_l_7")])
ICC(dfinal[, c("vta_mni_10_s1_gain_r_3", "vta_mni_10_s2_gain_r_3")])
ICC(dfinal[, c("vta_mni_10_s1_gain_r_4", "vta_mni_10_s2_gain_r_4")])
ICC(dfinal[, c("vta_mni_10_s1_gain_r_5", "vta_mni_10_s2_gain_r_5")])
ICC(dfinal[, c("vta_mni_10_s1_gain_r_6", "vta_mni_10_s2_gain_r_6")])
ICC(dfinal[, c("vta_mni_10_s1_gain_r_7", "vta_mni_10_s2_gain_r_7")])



# vta loss anticipation reliability
ICC(dfinal[, c("vta_mni_50_s1_loss_b_3", "vta_mni_50_s2_loss_b_3")])
ICC(dfinal[, c("vta_mni_50_s1_loss_b_4", "vta_mni_50_s2_loss_b_4")])
ICC(dfinal[, c("vta_mni_50_s1_loss_b_5", "vta_mni_50_s2_loss_b_5")])
ICC(dfinal[, c("vta_mni_50_s1_loss_b_6", "vta_mni_50_s2_loss_b_6")])
ICC(dfinal[, c("vta_mni_50_s1_loss_b_7", "vta_mni_50_s2_loss_b_7")])
ICC(dfinal[, c("vta_mni_50_s1_loss_l_3", "vta_mni_50_s2_loss_l_3")])
ICC(dfinal[, c("vta_mni_50_s1_loss_l_4", "vta_mni_50_s2_loss_l_4")])
ICC(dfinal[, c("vta_mni_50_s1_loss_l_5", "vta_mni_50_s2_loss_l_5")])
ICC(dfinal[, c("vta_mni_50_s1_loss_l_6", "vta_mni_50_s2_loss_l_6")])
ICC(dfinal[, c("vta_mni_50_s1_loss_l_7", "vta_mni_50_s2_loss_l_7")])
ICC(dfinal[, c("vta_mni_50_s1_loss_r_3", "vta_mni_50_s2_loss_r_3")])
ICC(dfinal[, c("vta_mni_50_s1_loss_r_4", "vta_mni_50_s2_loss_r_4")])
ICC(dfinal[, c("vta_mni_50_s1_loss_r_5", "vta_mni_50_s2_loss_r_5")])
ICC(dfinal[, c("vta_mni_50_s1_loss_r_6", "vta_mni_50_s2_loss_r_6")])
ICC(dfinal[, c("vta_mni_50_s1_loss_r_7", "vta_mni_50_s2_loss_r_7")])

ICC(dfinal[, c("vta_mni_10_s1_loss_b_3", "vta_mni_10_s2_loss_b_3")])
ICC(dfinal[, c("vta_mni_10_s1_loss_b_4", "vta_mni_10_s2_loss_b_4")])
ICC(dfinal[, c("vta_mni_10_s1_loss_b_5", "vta_mni_10_s2_loss_b_5")])
ICC(dfinal[, c("vta_mni_10_s1_loss_b_6", "vta_mni_10_s2_loss_b_6")])
ICC(dfinal[, c("vta_mni_10_s1_loss_b_7", "vta_mni_10_s2_loss_b_7")])
ICC(dfinal[, c("vta_mni_10_s1_loss_l_3", "vta_mni_10_s2_loss_l_3")])
ICC(dfinal[, c("vta_mni_10_s1_loss_l_4", "vta_mni_10_s2_loss_l_4")])
ICC(dfinal[, c("vta_mni_10_s1_loss_l_5", "vta_mni_10_s2_loss_l_5")])
ICC(dfinal[, c("vta_mni_10_s1_loss_l_6", "vta_mni_10_s2_loss_l_6")])
ICC(dfinal[, c("vta_mni_10_s1_loss_l_7", "vta_mni_10_s2_loss_l_7")])
ICC(dfinal[, c("vta_mni_10_s1_loss_r_3", "vta_mni_10_s2_loss_r_3")])
ICC(dfinal[, c("vta_mni_10_s1_loss_r_4", "vta_mni_10_s2_loss_r_4")])
ICC(dfinal[, c("vta_mni_10_s1_loss_r_5", "vta_mni_10_s2_loss_r_5")])
ICC(dfinal[, c("vta_mni_10_s1_loss_r_6", "vta_mni_10_s2_loss_r_6")])
ICC(dfinal[, c("vta_mni_10_s1_loss_r_7", "vta_mni_10_s2_loss_r_7")])


# vta neutral anticipation reliability
ICC(dfinal[, c("vta_mni_50_s1_neutral_b_3", "vta_mni_50_s2_neutral_b_3")])
ICC(dfinal[, c("vta_mni_50_s1_neutral_b_4", "vta_mni_50_s2_neutral_b_4")])
ICC(dfinal[, c("vta_mni_50_s1_neutral_b_5", "vta_mni_50_s2_neutral_b_5")])
ICC(dfinal[, c("vta_mni_50_s1_neutral_b_6", "vta_mni_50_s2_neutral_b_6")])
ICC(dfinal[, c("vta_mni_50_s1_neutral_b_7", "vta_mni_50_s2_neutral_b_7")])
ICC(dfinal[, c("vta_mni_50_s1_neutral_l_3", "vta_mni_50_s2_neutral_l_3")])
ICC(dfinal[, c("vta_mni_50_s1_neutral_l_4", "vta_mni_50_s2_neutral_l_4")])
ICC(dfinal[, c("vta_mni_50_s1_neutral_l_5", "vta_mni_50_s2_neutral_l_5")])
ICC(dfinal[, c("vta_mni_50_s1_neutral_l_6", "vta_mni_50_s2_neutral_l_6")])
ICC(dfinal[, c("vta_mni_50_s1_neutral_l_7", "vta_mni_50_s2_neutral_l_7")])
ICC(dfinal[, c("vta_mni_50_s1_neutral_r_3", "vta_mni_50_s2_neutral_r_3")])
ICC(dfinal[, c("vta_mni_50_s1_neutral_r_4", "vta_mni_50_s2_neutral_r_4")])
ICC(dfinal[, c("vta_mni_50_s1_neutral_r_5", "vta_mni_50_s2_neutral_r_5")])
ICC(dfinal[, c("vta_mni_50_s1_neutral_r_6", "vta_mni_50_s2_neutral_r_6")])
ICC(dfinal[, c("vta_mni_50_s1_neutral_r_7", "vta_mni_50_s2_neutral_r_7")])

ICC(dfinal[, c("vta_mni_10_s1_neutral_b_3", "vta_mni_10_s2_neutral_b_3")])
ICC(dfinal[, c("vta_mni_10_s1_neutral_b_4", "vta_mni_10_s2_neutral_b_4")])
ICC(dfinal[, c("vta_mni_10_s1_neutral_b_5", "vta_mni_10_s2_neutral_b_5")])
ICC(dfinal[, c("vta_mni_10_s1_neutral_b_6", "vta_mni_10_s2_neutral_b_6")])
ICC(dfinal[, c("vta_mni_10_s1_neutral_b_7", "vta_mni_10_s2_neutral_b_7")])
ICC(dfinal[, c("vta_mni_10_s1_neutral_l_3", "vta_mni_10_s2_neutral_l_3")])
ICC(dfinal[, c("vta_mni_10_s1_neutral_l_4", "vta_mni_10_s2_neutral_l_4")])
ICC(dfinal[, c("vta_mni_10_s1_neutral_l_5", "vta_mni_10_s2_neutral_l_5")])
ICC(dfinal[, c("vta_mni_10_s1_neutral_l_6", "vta_mni_10_s2_neutral_l_6")])
ICC(dfinal[, c("vta_mni_10_s1_neutral_l_7", "vta_mni_10_s2_neutral_l_7")])
ICC(dfinal[, c("vta_mni_10_s1_neutral_r_3", "vta_mni_10_s2_neutral_r_3")])
ICC(dfinal[, c("vta_mni_10_s1_neutral_r_4", "vta_mni_10_s2_neutral_r_4")])
ICC(dfinal[, c("vta_mni_10_s1_neutral_r_5", "vta_mni_10_s2_neutral_r_5")])
ICC(dfinal[, c("vta_mni_10_s1_neutral_r_6", "vta_mni_10_s2_neutral_r_6")])
ICC(dfinal[, c("vta_mni_10_s1_neutral_r_7", "vta_mni_10_s2_neutral_r_7")])



# caudate? 
ICC(dfinal[, c("caudate_s1_gain_b_3", "caudate_s2_gain_b_3")])
ICC(dfinal[, c("caudate_s1_gain_b_4", "caudate_s2_gain_b_4")])
ICC(dfinal[, c("caudate_s1_gain_b_5", "caudate_s2_gain_b_5")])
ICC(dfinal[, c("caudate_s1_gain_b_6", "caudate_s2_gain_b_6")])
ICC(dfinal[, c("caudate_s1_gain_b_7", "caudate_s2_gain_b_7")])
ICC(dfinal[, c("caudate_s1_gain_l_3", "caudate_s2_gain_l_3")])
ICC(dfinal[, c("caudate_s1_gain_l_4", "caudate_s2_gain_l_4")])
ICC(dfinal[, c("caudate_s1_gain_l_5", "caudate_s2_gain_l_5")])
ICC(dfinal[, c("caudate_s1_gain_l_6", "caudate_s2_gain_l_6")])
ICC(dfinal[, c("caudate_s1_gain_l_7", "caudate_s2_gain_l_7")])
ICC(dfinal[, c("caudate_s1_gain_r_3", "caudate_s2_gain_r_3")])
ICC(dfinal[, c("caudate_s1_gain_r_4", "caudate_s2_gain_r_4")])
ICC(dfinal[, c("caudate_s1_gain_r_5", "caudate_s2_gain_r_5")])
ICC(dfinal[, c("caudate_s1_gain_r_6", "caudate_s2_gain_r_6")])
ICC(dfinal[, c("caudate_s1_gain_r_7", "caudate_s2_gain_r_7")])


write.csv(dfinal, '/Users/nathanielbouton/Documents/UArk/FAN Lab/Test-Retest/mid_reliability.csv', row.names = F)




