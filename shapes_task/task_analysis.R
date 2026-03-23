setwd("Documents/UArk/courses/cognitive/data/")
d <- do.call(rbind, lapply(dir(pattern="participant_[0-9]"), function(x){
  d1 <- read.csv(x)
  d1 <- d1[d1$response!=" none" & d1$isPractice==0,]
  hits <- (nrow(d1[d1$response==" s" & d1$condition==" old-old",])+.5)/(nrow(d1[d1$condition==" old-old",])+1)
  faOldNew <- (nrow(d1[d1$response==" s" & d1$condition==" old-new",])+.5)/(nrow(d1[d1$condition==" old-new",])+1)
  faNewOld <- (nrow(d1[d1$response==" s" & d1$condition==" new-old",])+.5)/(nrow(d1[d1$condition==" new-old",])+1)
  faNewNew <- (nrow(d1[d1$response==" s" & d1$condition==" new-new",])+.5)/(nrow(d1[d1$condition==" new-new",])+1)
  dprimeOldOldWithNewNew <- qnorm(hits)-qnorm(faNewNew)
  dprimeOldOldWithOldNew <- qnorm(hits)-qnorm(faOldNew)
  dprimeOldOldWithNewOld <- qnorm(hits)-qnorm(faNewOld)
  dprimeNewOldWithNewNew <- qnorm(faNewOld)-qnorm(faNewNew)
  dprimeOldNewWithNewNew <- qnorm(faOldNew)-qnorm(faNewNew)
  dprimeOldNewWithNewOld <- qnorm(faOldNew)-qnorm(faNewOld)
  #data.frame(id=d1$id[1], aggregate(correct ~ condition, data = d1[d1$response!=" none" & d1$isPractice==0,], FUN=mean))
  dat <- aggregate(as.numeric(rt) ~ condition, data = d1, FUN=mean)
  names(dat)[2] <- "rt"
  da <- data.frame(id=d1$id[1], dprimeOldNewWithNewOld, dprimeOldOldWithNewNew, dprimeOldOldWithNewOld, dprimeOldOldWithOldNew, dprimeOldNewWithNewNew, dprimeNewOldWithNewNew, rbind(unlist(dat$rt)))
  names(da)[which(names(da)=="X1"):which(names(da)=="X4")] <- paste0(dat$condition, "_rt")
  da
}))

with(d, t.test(dprimeOldOldWithNewNew))
with(d, t.test(dprimeOldOldWithOldNew))
with(d, t.test(dprimeOldOldWithNewOld)) # these three show that old-old items, people are able to remember better than chance, 
                                        # relative to old-new, relative to new-new, and old-old

#with(d, t.test(dprimeOldOldWithNewNew, dprimeOldNewWithNewNew, paired=T))
with(d, t.test(dprimeOldNewWithNewNew, dprimeNewOldWithNewNew, paired=T)) # shows that memory for shapes is better than memory for colors.
# Colors can be swapped, but shapes cannot. 


#d <- do.call(rbind, lapply(dir(pattern="participant_[0-9]"), function(x){
#  d1 <- read.csv(x)
#  d1 <- d1[d1$response!=" none" & d1$isPractice==0,]
#  data.frame(id=d1$id[1], aggregate(correct ~ condition, data = d1[d1$response!=" none" & d1$isPractice==0,], FUN=mean))
#}))
#install.packages("afex")
#install.packages("emmeans")

#library(afex)
#library(emmeans)


#(a1 <- aov_ez(id="id", dv="correct", within = "condition", data = d))
#(e1 <- emmeans(a1, ~ condition))
#pairs(e1, adjust="none")

# People are less likely to be correct when the shape that they are asked about was in the 
# study array, even if the color is switched. Because old.old deviates from 0.5 in the negative direction
# and old.new deviates from 0.5 in the positive direction

# People are making feature binding errors for color but not shape.

# Memory analysis shows people do have memory. There's just a strong response bias. 
# Memory for items when they are old shapes, they are much more likely to incorrectly say
# old when they have a new color, than when they are old colors with a different shape.
# People are indeed making these feature binding errors, espcially when it comes to the Old-New category.











# create visualizations

library(ggplot2)
library(dplyr)
library(tidyr)

# Create a long format dataset for visualization
# First, prepare the accuracy data by condition
accuracy_data <- do.call(rbind, lapply(dir(pattern="participant_[0-9]"), function(x){
  d1 <- read.csv(x)
  d1 <- d1[d1$response!=" none" & d1$isPractice==0,]
  # Create a data frame with participant ID, condition, and correct/incorrect
  data.frame(
    id = d1$id,
    condition = d1$condition,
    correct = d1$correct,
    rt = as.numeric(d1$rt)
  )
}))

# Clean condition names by removing leading space
accuracy_data$condition <- trimws(accuracy_data$condition)

# Create boxplot for accuracy by condition
accuracy_plot <- ggplot(accuracy_data, aes(x = condition, y = correct, fill = condition)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Accuracy by Condition",
    x = "Condition",
    y = "Accuracy (1 = Correct, 0 = Incorrect)",
    caption = "Black diamonds represent means"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Create boxplot for reaction times by condition
rt_plot <- ggplot(accuracy_data, aes(x = condition, y = rt, fill = condition)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Reaction Times by Condition",
    x = "Condition",
    y = "Reaction Time (ms)",
    caption = "Black diamonds represent means"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(accuracy_plot)
print(rt_plot)

# Visualize the d-prime scores across participants
# First, reshape the d-prime data into long format
dprime_long <- d %>%
  select(id, dprimeOldOldWithNewNew, dprimeOldOldWithOldNew, dprimeOldOldWithNewOld, 
         dprimeOldNewWithNewOld) %>%
  pivot_longer(
    cols = starts_with("dprime"),
    names_to = "comparison",
    values_to = "dprime"
  )

# Create better labels for the comparisons
dprime_long$comparison <- factor(dprime_long$comparison,
                                 levels = c("dprimeOldOldWithNewNew", "dprimeOldOldWithOldNew", 
                                            "dprimeOldOldWithNewOld", "dprimeOldNewWithNewOld"),
                                 labels = c("old-old vs new-new", "old-old vs old-new", 
                                            "old-old vs new-old", "old-new vs new-old")
)

# Create boxplot for d-prime scores
dprime_plot <- ggplot(dprime_long, aes(x = comparison, y = dprime, fill = comparison)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "D-prime Scores by Comparison",
    x = "Differences in d-prime",
    y = "d-prime",
    caption = "Black diamonds represent means"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(dprime_plot)

# Create a visualization for response bias
# Calculate response bias for "old" responses by condition
response_bias <- do.call(rbind, lapply(dir(pattern="participant_[0-9]"), function(x){
  d1 <- read.csv(x)
  d1 <- d1[d1$response!=" none" & d1$isPractice==0,]
  
  # Calculate proportion of "s" (old) responses for each condition
  old_old_s <- sum(d1$response == " s" & d1$condition == " old-old") / 
    sum(d1$condition == " old-old")
  old_new_s <- sum(d1$response == " s" & d1$condition == " old-new") / 
    sum(d1$condition == " old-new")
  new_old_s <- sum(d1$response == " s" & d1$condition == " new-old") / 
    sum(d1$condition == " new-old")
  new_new_s <- sum(d1$response == " s" & d1$condition == " new-new") / 
    sum(d1$condition == " new-new")
  
  data.frame(
    id = d1$id[1],
    old_old = old_old_s,
    old_new = old_new_s,
    new_old = new_old_s,
    new_new = new_new_s
  )
}))

# Reshape the response bias data to long format
response_bias_long <- response_bias %>%
  pivot_longer(
    cols = c(old_old, old_new, new_old, new_new),
    names_to = "condition",
    values_to = "prop_old_response"
  )

# Create a boxplot for proportion of "old" responses by condition
response_bias_plot <- ggplot(response_bias_long, 
                             aes(x = condition, y = prop_old_response, fill = condition)) +
  geom_boxplot() +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Proportion of 'Old' Responses by Condition",
    x = "Condition",
    y = "Proportion of 'Old' Responses",
    caption = "Black diamonds represent means, dashed line at chance level (0.5)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(response_bias_plot)

# Combine plots into a single figure (requires gridExtra)
if (!require(gridExtra)) install.packages("gridExtra")
library(gridExtra)

# Save all plots
#ggsave("accuracy_plot.png", accuracy_plot, width = 8, height = 6)
#ggsave("rt_plot.png", rt_plot, width = 8, height = 6)
#ggsave("dprime_plot.png", dprime_plot, width = 10, height = 6)
#ggsave("response_bias_plot.png", response_bias_plot, width = 8, height = 6)

# Display combined plot
combined_plot <- grid.arrange(
  accuracy_plot, rt_plot, 
  dprime_plot, response_bias_plot,
  ncol = 2,
  top = "Feature Binding Experiment Results"
)

# Save combined plot
#ggsave("combined_feature_binding_plots.png", combined_plot, width = 14, height = 10)

# Print summary statistics
cat("\nSummary Statistics for Accuracy by Condition:\n")
print(aggregate(correct ~ condition, data = accuracy_data, 
                FUN = function(x) c(mean = mean(x), sd = sd(x), n = length(x))))

cat("\nSummary Statistics for D-prime Measures:\n")
print(aggregate(dprime ~ comparison, data = dprime_long, 
                FUN = function(x) c(mean = mean(x), sd = sd(x))))

cat("\nSummary of t-tests for D-prime Measures:\n")
cat("Old-old vs New-new: ")
print(t.test(d$dprimeOldOldWithNewNew)$p.value)
cat("Old-old vs Old-new: ")
print(t.test(d$dprimeOldOldWithOldNew)$p.value)
cat("Old-old vs New-old: ")
print(t.test(d$dprimeOldOldWithNewOld)$p.value)
cat("Old-new vs New-old: ")
print(t.test(d$dprimeOldNewWithNewNew, d$dprimeNewOldWithNewNew, paired=TRUE)$p.value)
