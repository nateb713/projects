data <- read.csv('/Users/nathanielbouton/Documents/UArk/FANlab/Test-Retest/MID/mid_tcs_activity.csv')

data$block <- factor(data$block)


# Subset for trialtype +$5
subset_condition1 <- subset(data, variable == "nacc8mm_mni" & 
                              (trialtype == "+$5"))

# Subset for trialtype $0
subset_condition2 <- subset(data, variable == "nacc8mm_mni" & trialtype == "$0")

# Calculate mean activation for each group
mean_activation_condition1 <- mean(subset_condition1$mean)
mean_activation_condition2 <- mean(subset_condition2$mean)

t.test(subset_condition1$mean, subset_condition2$mean)
