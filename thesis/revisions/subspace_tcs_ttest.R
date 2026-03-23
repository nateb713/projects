library(dplyr)
require(reshape2)
require(stringr)

# Read data
data <- read.csv('/Users/nate/Desktop/subspace/nacc_timecourses/nacc2_ses1&2_tcs_n16.csv')

# Convert to long form - using bilateral NAcc2 only
d.tc.melt = melt(data, id=c('tr','condition','subject','block'),
                 measure.vars = c('bmid_mbnf_nacc2'))

# Summarize over TR and condition for each subject
d.tc.subject = ddply(d.tc.melt, .variables = c('tr','condition','variable','subject','block'),
                     summarise, mean.subject=mean(value))

# Prepare condition labels
d.tc.subject$trialtype <- ""
d.tc.subject[d.tc.subject$condition == "neutral", 'trialtype'] = '$0'
d.tc.subject[d.tc.subject$condition == "gain", 'trialtype'] = '+$5'

# Filter for TR 3 (anticipation) and TR 7 (outcome) by session
# Session 1 - TR 3 (anticipation)
subject_gain_s1_tr3 <- subset(d.tc.subject, trialtype == "+$5" & tr == "3" & ses == "s1")
subject_neutral_s1_tr3 <- subset(d.tc.subject, trialtype == "$0" & tr == "3" & ses == "s1")

# Session 2 - TR 3 (anticipation) 
subject_gain_s2_tr3 <- subset(d.tc.subject, trialtype == "+$5" & tr == "3" & ses == "s2")
subject_neutral_s2_tr3 <- subset(d.tc.subject, trialtype == "$0" & tr == "3" & ses == "s2")

# Session 1 - TR 7 (outcome)
subject_gain_s1_tr7 <- subset(d.tc.subject, trialtype == "+$5" & tr == "7" & ses == "s1")
subject_neutral_s1_tr7 <- subset(d.tc.subject, trialtype == "$0" & tr == "7" & ses == "s1")

# Session 2 - TR 7 (outcome)
subject_gain_s2_tr7 <- subset(d.tc.subject, trialtype == "+$5" & tr == "7" & ses == "s2")
subject_neutral_s2_tr7 <- subset(d.tc.subject, trialtype == "$0" & tr == "7" & ses == "s2")

# Get one mean value per subject for each condition
subject_means_gain_s1_tr3 <- aggregate(mean.subject ~ subject, data = subject_gain_s1_tr3, FUN = mean)
subject_means_neutral_s1_tr3 <- aggregate(mean.subject ~ subject, data = subject_neutral_s1_tr3, FUN = mean)
subject_means_gain_s2_tr3 <- aggregate(mean.subject ~ subject, data = subject_gain_s2_tr3, FUN = mean)
subject_means_neutral_s2_tr3 <- aggregate(mean.subject ~ subject, data = subject_neutral_s2_tr3, FUN = mean)
subject_means_gain_s1_tr7 <- aggregate(mean.subject ~ subject, data = subject_gain_s1_tr7, FUN = mean)
subject_means_neutral_s1_tr7 <- aggregate(mean.subject ~ subject, data = subject_neutral_s1_tr7, FUN = mean)
subject_means_gain_s2_tr7 <- aggregate(mean.subject ~ subject, data = subject_gain_s2_tr7, FUN = mean)
subject_means_neutral_s2_tr7 <- aggregate(mean.subject ~ subject, data = subject_neutral_s2_tr7, FUN = mean)

# Ensure subjects match for paired t-tests
common_subjects_s1_tr3 <- intersect(subject_means_gain_s1_tr3$subject, subject_means_neutral_s1_tr3$subject)
common_subjects_s2_tr3 <- intersect(subject_means_gain_s2_tr3$subject, subject_means_neutral_s2_tr3$subject)
common_subjects_s1_tr7 <- intersect(subject_means_gain_s1_tr7$subject, subject_means_neutral_s1_tr7$subject)
common_subjects_s2_tr7 <- intersect(subject_means_gain_s2_tr7$subject, subject_means_neutral_s2_tr7$subject)

# Filter to common subjects and order
subject_means_gain_s1_tr3_common <- subject_means_gain_s1_tr3[subject_means_gain_s1_tr3$subject %in% common_subjects_s1_tr3, ]
subject_means_neutral_s1_tr3_common <- subject_means_neutral_s1_tr3[subject_means_neutral_s1_tr3$subject %in% common_subjects_s1_tr3, ]
subject_means_gain_s2_tr3_common <- subject_means_gain_s2_tr3[subject_means_gain_s2_tr3$subject %in% common_subjects_s2_tr3, ]
subject_means_neutral_s2_tr3_common <- subject_means_neutral_s2_tr3[subject_means_neutral_s2_tr3$subject %in% common_subjects_s2_tr3, ]
subject_means_gain_s1_tr7_common <- subject_means_gain_s1_tr7[subject_means_gain_s1_tr7$subject %in% common_subjects_s1_tr7, ]
subject_means_neutral_s1_tr7_common <- subject_means_neutral_s1_tr7[subject_means_neutral_s1_tr7$subject %in% common_subjects_s1_tr7, ]
subject_means_gain_s2_tr7_common <- subject_means_gain_s2_tr7[subject_means_gain_s2_tr7$subject %in% common_subjects_s2_tr7, ]
subject_means_neutral_s2_tr7_common <- subject_means_neutral_s2_tr7[subject_means_neutral_s2_tr7$subject %in% common_subjects_s2_tr7, ]

# Order subjects consistently
subject_means_gain_s1_tr3_common <- subject_means_gain_s1_tr3_common[order(subject_means_gain_s1_tr3_common$subject), ]
subject_means_neutral_s1_tr3_common <- subject_means_neutral_s1_tr3_common[order(subject_means_neutral_s1_tr3_common$subject), ]
subject_means_gain_s2_tr3_common <- subject_means_gain_s2_tr3_common[order(subject_means_gain_s2_tr3_common$subject), ]
subject_means_neutral_s2_tr3_common <- subject_means_neutral_s2_tr3_common[order(subject_means_neutral_s2_tr3_common$subject), ]
subject_means_gain_s1_tr7_common <- subject_means_gain_s1_tr7_common[order(subject_means_gain_s1_tr7_common$subject), ]
subject_means_neutral_s1_tr7_common <- subject_means_neutral_s1_tr7_common[order(subject_means_neutral_s1_tr7_common$subject), ]
subject_means_gain_s2_tr7_common <- subject_means_gain_s2_tr7_common[order(subject_means_gain_s2_tr7_common$subject), ]
subject_means_neutral_s2_tr7_common <- subject_means_neutral_s2_tr7_common[order(subject_means_neutral_s2_tr7_common$subject), ]

# Print descriptive statistics
cat("NAcc2 Bilateral Results by Session:\n\n")

cat("SESSION 1 - TR 3 (Anticipation Phase):\n")
cat("Gain (+$5): M =", round(mean(subject_means_gain_s1_tr3_common$mean.subject), 4), 
    ", SD =", round(sd(subject_means_gain_s1_tr3_common$mean.subject), 4), "\n")
cat("Neutral ($0): M =", round(mean(subject_means_neutral_s1_tr3_common$mean.subject), 4), 
    ", SD =", round(sd(subject_means_neutral_s1_tr3_common$mean.subject), 4), "\n")

cat("\nSESSION 2 - TR 3 (Anticipation Phase):\n")
cat("Gain (+$5): M =", round(mean(subject_means_gain_s2_tr3_common$mean.subject), 4), 
    ", SD =", round(sd(subject_means_gain_s2_tr3_common$mean.subject), 4), "\n")
cat("Neutral ($0): M =", round(mean(subject_means_neutral_s2_tr3_common$mean.subject), 4), 
    ", SD =", round(sd(subject_means_neutral_s2_tr3_common$mean.subject), 4), "\n")

cat("\nSESSION 1 - TR 7 (Outcome Phase):\n")
cat("Gain (+$5): M =", round(mean(subject_means_gain_s1_tr7_common$mean.subject), 4), 
    ", SD =", round(sd(subject_means_gain_s1_tr7_common$mean.subject), 4), "\n")
cat("Neutral ($0): M =", round(mean(subject_means_neutral_s1_tr7_common$mean.subject), 4), 
    ", SD =", round(sd(subject_means_neutral_s1_tr7_common$mean.subject), 4), "\n")

cat("\nSESSION 2 - TR 7 (Outcome Phase):\n")
cat("Gain (+$5): M =", round(mean(subject_means_gain_s2_tr7_common$mean.subject), 4), 
    ", SD =", round(sd(subject_means_gain_s2_tr7_common$mean.subject), 4), "\n")
cat("Neutral ($0): M =", round(mean(subject_means_neutral_s2_tr7_common$mean.subject), 4), 
    ", SD =", round(sd(subject_means_neutral_s2_tr7_common$mean.subject), 4), "\n")

# Run paired t-tests for each session and phase
t_test_s1_tr3 <- t.test(subject_means_gain_s1_tr3_common$mean.subject, 
                        subject_means_neutral_s1_tr3_common$mean.subject, 
                        paired = TRUE)

t_test_s2_tr3 <- t.test(subject_means_gain_s2_tr3_common$mean.subject, 
                        subject_means_neutral_s2_tr3_common$mean.subject, 
                        paired = TRUE)

t_test_s1_tr7 <- t.test(subject_means_gain_s1_tr7_common$mean.subject, 
                        subject_means_neutral_s1_tr7_common$mean.subject, 
                        paired = TRUE)

t_test_s2_tr7 <- t.test(subject_means_gain_s2_tr7_common$mean.subject, 
                        subject_means_neutral_s2_tr7_common$mean.subject, 
                        paired = TRUE)

# Print t-test results
cat("\nT-test for +$5 vs $0 - Session 1, TR 3 (Anticipation):\n")
print(t_test_s1_tr3)

cat("\nT-test for +$5 vs $0 - Session 2, TR 3 (Anticipation):\n")
print(t_test_s2_tr3)

cat("\nT-test for +$5 vs $0 - Session 1, TR 7 (Outcome):\n")
print(t_test_s1_tr7)

cat("\nT-test for +$5 vs $0 - Session 2, TR 7 (Outcome):\n")
print(t_test_s2_tr7)
