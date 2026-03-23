library(stringr)
library(ggplot2)
library(reshape2)
library(extrafont)
library(plyr)
library(dplyr)
library(irr)

# Load data for each subject, rbind into 1 data frame. separate sessions.
subvec = c('rrl01','rrl02','rrl05','rrl06','rrl07',
           'rrl10','rrl11','rrl12','rrl13','rrl14','rrl15',
           'rrl17','rrl18','rrl19','rrl22','rrl23')

# Load session 1 data
d1 = data.frame()
for (subj in 1:length(subvec)){
  loadfilename = str_c('/Users/nate/Desktop/subspace/nacc_timecourses/',subvec[subj],'/',subvec[subj],'_s1_nacc2_tc10tr.csv')
  d0 = read.csv(loadfilename, head=T)
  d0$subject = subvec[subj]
  d1 = rbind(d1,d0)
}

# Load session 2 data
d2 = data.frame()
for (subj in 1:length(subvec)){
  loadfilename = str_c('/Users/nate/Desktop/subspace/nacc_timecourses/',subvec[subj],'/',subvec[subj],'_s2_nacc2_tc10tr.csv')
  d0 = read.csv(loadfilename, head=T)
  d0$subject = subvec[subj]
  d2 = rbind(d2,d0)
}

# Add session column
d1$ses = rep('s1',nrow(d1))
d2$ses = rep('s2',nrow(d2))

# Clean up column names - remove .1D suffix from timecourse columns
# Find which columns are timecourse columns (should be the NAcc2 ones)
tc_cols = grep("nacc2", colnames(d1))
colnames(d1)[tc_cols] = str_sub(colnames(d1)[tc_cols], 1, -5)
colnames(d2)[tc_cols] = str_sub(colnames(d2)[tc_cols], 1, -5)

# Add condition column 
d1 <- d1 %>%
  mutate(condition = case_when(
    trialtype == 1 ~ "neutral",
    trialtype == 2 ~ "loss", 
    trialtype == 3 ~ "gain",
  )) %>%
  select(-trialtype)

d2 <- d2 %>%
  mutate(condition = case_when(
    trialtype == 1 ~ "neutral",
    trialtype == 2 ~ "loss",
    trialtype == 3 ~ "gain", 
  )) %>%
  select(-trialtype)

# Combine both sessions
df = rbind(d1,d2)

# Save combined data
write.csv(df, '/Users/nate/Desktop/subspace/nacc_timecourses/nacc2_ses1&2_tcs_n16.csv', row.names = F)

# Define brain regions (should be the NAcc2 columns)
brain_regions = grep("nacc2", colnames(df), value = TRUE)

# Melt the data
df.long = melt(df, measure.vars = brain_regions)

# Extract region and hemisphere info
df.long <- df.long %>%
  mutate(region = "nacc2",  # Since we only have NAcc2 data now
         hemi = str_extract(variable, 'b|l|r'))

# Calculate mean NAcc activity
result_alt1 <- aggregate(value ~ subject + region + ses + condition + hemi + tr, 
                         data = df.long, 
                         FUN = mean, 
                         na.rm = TRUE)

# Convert to wide form for ICC analysis
dfinal <- dcast(result_alt1, subject ~ region + ses + condition + hemi + tr, 
                fun.aggregate = mean,
                value.var = "value")


library(psych)
# NAcc2 gain anticipation reliability
cat("NAcc2 Gain Anticipation Reliability:\n")
cat("Bilateral:\n")
print(ICC(dfinal[, c("nacc2_s1_gain_b_3", "nacc2_s2_gain_b_3")]))
print(ICC(dfinal[, c("nacc2_s1_gain_b_4", "nacc2_s2_gain_b_4")]))
print(ICC(dfinal[, c("nacc2_s1_gain_b_5", "nacc2_s2_gain_b_5")]))
print(ICC(dfinal[, c("nacc2_s1_gain_b_6", "nacc2_s2_gain_b_6")]))
print(ICC(dfinal[, c("nacc2_s1_gain_b_7", "nacc2_s2_gain_b_7")]))

cat("Left hemisphere:\n")
print(ICC(dfinal[, c("nacc2_s1_gain_l_3", "nacc2_s2_gain_l_3")]))
print(ICC(dfinal[, c("nacc2_s1_gain_l_4", "nacc2_s2_gain_l_4")]))
print(ICC(dfinal[, c("nacc2_s1_gain_l_5", "nacc2_s2_gain_l_5")]))
print(ICC(dfinal[, c("nacc2_s1_gain_l_6", "nacc2_s2_gain_l_6")]))
print(ICC(dfinal[, c("nacc2_s1_gain_l_7", "nacc2_s2_gain_l_7")]))

cat("Right hemisphere:\n")
print(ICC(dfinal[, c("nacc2_s1_gain_r_3", "nacc2_s2_gain_r_3")]))
print(ICC(dfinal[, c("nacc2_s1_gain_r_4", "nacc2_s2_gain_r_4")]))
print(ICC(dfinal[, c("nacc2_s1_gain_r_5", "nacc2_s2_gain_r_5")]))
print(ICC(dfinal[, c("nacc2_s1_gain_r_6", "nacc2_s2_gain_r_6")]))
print(ICC(dfinal[, c("nacc2_s1_gain_r_7", "nacc2_s2_gain_r_7")]))

# NAcc2 loss anticipation reliability
cat("\nNAcc2 Loss Anticipation Reliability:\n")
cat("Bilateral:\n")
print(ICC(dfinal[, c("nacc2_s1_loss_b_3", "nacc2_s2_loss_b_3")]))
print(ICC(dfinal[, c("nacc2_s1_loss_b_4", "nacc2_s2_loss_b_4")]))

cat("Left hemisphere:\n")
print(ICC(dfinal[, c("nacc2_s1_loss_l_3", "nacc2_s2_loss_l_3")]))
print(ICC(dfinal[, c("nacc2_s1_loss_l_4", "nacc2_s2_loss_l_4")]))

cat("Right hemisphere:\n")
print(ICC(dfinal[, c("nacc2_s1_loss_r_3", "nacc2_s2_loss_r_3")]))
print(ICC(dfinal[, c("nacc2_s1_loss_r_4", "nacc2_s2_loss_r_4")]))

# NAcc2 neutral anticipation reliability  
cat("\nNAcc2 Neutral Anticipation Reliability:\n")
cat("Bilateral:\n")
print(ICC(dfinal[, c("nacc2_s1_neutral_b_3", "nacc2_s2_neutral_b_3")]))
print(ICC(dfinal[, c("nacc2_s1_neutral_b_4", "nacc2_s2_neutral_b_4")]))

cat("Left hemisphere:\n")
print(ICC(dfinal[, c("nacc2_s1_neutral_l_3", "nacc2_s2_neutral_l_3")]))
print(ICC(dfinal[, c("nacc2_s1_neutral_l_4", "nacc2_s2_neutral_l_4")]))

cat("Right hemisphere:\n")
print(ICC(dfinal[, c("nacc2_s1_neutral_r_3", "nacc2_s2_neutral_r_3")]))
print(ICC(dfinal[, c("nacc2_s1_neutral_r_4", "nacc2_s2_neutral_r_4")]))

# Save final reliability data
write.csv(dfinal, '/Users/nate/Desktop/subspace/nacc_timecourses/nacc2_reliability.csv', row.names = F)

cat("\nAnalysis complete! Results saved to nacc2_reliability.csv\n")