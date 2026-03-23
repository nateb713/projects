require(ggplot2)
require(reshape2)
require(extrafont)
require(plyr)
require(grid)
require(stringr)
library(tidyverse)

# Read the combined data
d.tc_combined = read.csv('/Users/nate/Desktop/subspace/nacc_timecourses/nacc2_ses1&2_tcs_n16.csv', head=T)

# Check what NAcc2 columns actually exist
nacc2_cols = grep("nacc2", colnames(d.tc_combined), value = TRUE)
cat("Available NAcc2 columns:\n")
print(nacc2_cols)

# Split by session
d.tc1 = d.tc_combined[d.tc_combined$ses == 's1', ]
d.tc2 = d.tc_combined[d.tc_combined$ses == 's2', ]

process_session_data <- function(d.tc) {
  d.tc = na.omit(d.tc)
  
  # Find the actual NAcc2 column names
  actual_nacc2_cols = grep("nacc2", colnames(d.tc), value = TRUE)
  
  # Long form using the actual column names
  d.tc.melt = melt(d.tc, id=c('tr','condition','subject','block'),
                   measure.vars = actual_nacc2_cols)
  
  # Summarize over TR and condition for each subject
  d.tc.subject = ddply(d.tc.melt, .variables = c('tr','condition','variable','subject','block'),
                       summarise, mean.subject=mean(value), sd.subject=sd(value)/sqrt(length(value)))
  
  # Calculate means across subjects
  d.tc.mean <- ddply(d.tc.subject, .variables = c('tr','condition','variable','block'),
                     function(x) data.frame(mean = mean(x$mean.subject)))
  
  # Calculate standard errors
  d.tc.sem <- ddply(d.tc.subject, .variables = c('tr','condition','variable','block'),
                    function(x) data.frame(sem = sd(x$mean.subject)/sqrt(length(unique(x$subject)))))
  
  # Merge the results
  d.tc.sum <- merge(d.tc.mean, d.tc.sem, by=c('tr','condition','variable','block'))
  
  # Change condition names to match your original format
  d.tc.sum[d.tc.sum$condition == 'neutral', 'condition'] = '$0'
  d.tc.sum[d.tc.sum$condition == 'loss', 'condition'] = '-$5'
  d.tc.sum[d.tc.sum$condition == 'gain', 'condition'] = '+$5'
  d.tc.sum$condition = factor(d.tc.sum$condition, levels = c('+$5', '$0'))  # +$5 first, then $0
  
  # Add hemisphere information
  d.tc.sum$hemi = c()
  d.tc.sum[grep('bmid', d.tc.sum$variable), 'hemi'] = 'b'
  d.tc.sum[grep('lmid', d.tc.sum$variable), 'hemi'] = 'l'
  d.tc.sum[grep('rmid', d.tc.sum$variable), 'hemi'] = 'r'
  
  # Extract just the ROI name (nacc2)
  d.tc.sum$roi = 'nacc2'
  d.tc.sum$variable = as.factor(d.tc.sum$variable)
  d.tc.sum$block = as.factor(d.tc.sum$block)
  
  return(d.tc.sum)
}

# Process both sessions
d.tc.sum1 <- process_session_data(d.tc1)
d.tc.sum2 <- process_session_data(d.tc2)

# Define colors exactly like your original
cond.color = c('black','grey50','black')
cond.fill = c('black','white','white')
cond.line = c('solid','dashed','dashed')

# Map colors correctly for the new order: +$5, $0
# Based on your example: $0 (neutral) should be grey/dashed with white fill, +$5 (gain) should be black/solid with black fill
plot_colors = c('black', 'grey50')  # +$5 = black, $0 = grey
plot_fills = c('black', 'white')    # +$5 = black fill, $0 = white fill  
plot_lines = c('solid', 'dashed')   # +$5 = solid, $0 = dashed

### NAcc2 PLOTS ###
# Session 1 - bilateral only
d.sub1_nacc <- d.tc.sum1[d.tc.sum1$hemi == "b", ]
# Remove -$5 condition like your original
plot_data1_nacc <- aggregate(
  cbind(mean, sem) ~ tr + condition, 
  data = d.sub1_nacc[d.sub1_nacc$condition %in% c("$0", "+$5"), ], 
  FUN = mean
)

nacc2.tcplot1 = ggplot(data = plot_data1_nacc, aes(x = tr, y = mean, group = condition)) + 
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = 0.25, colour = '#939598') +
  geom_line(aes(linetype = condition, colour = condition), size = 1.5) +
  geom_point(aes(colour = condition, fill = condition), shape = 21, size = 5, show.legend = FALSE) +
  scale_linetype_manual('trialtype', values = plot_lines) +
  scale_color_manual('trialtype', values = plot_colors) +
  scale_fill_manual('trialtype', values = plot_fills) +
  scale_x_continuous(breaks = seq(0, 10, by = 2), labels = seq(0, 20, by = 4)) +
  scale_y_continuous(breaks = seq(-0.3, 0.3, by = 0.1), 
                     labels = function(x) ifelse(abs(x) < 1e-10, "0", x),
                     limits = c(-0.3, 0.3)) +
  theme(text = element_text(family = 'Helvetica', size = 20, colour = 'black'),
        axis.text = element_text(family = 'Helvetica', size = 20, colour = 'black'),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(keywidth = 1, keyheight = 1),
         linetype = guide_legend(keywidth = 2, keyheight = 1),
         colour = guide_legend(keywidth = 2, keyheight = 1)) +
  xlab('Seconds') +
  ylab('Percent Signal Change') +
  ggtitle('NAcc subject space timecourse - Ses 1')

# Session 2 - bilateral
d.sub2_nacc <- d.tc.sum2[d.tc.sum2$hemi == "b", ]
# Remove -$5 condition
plot_data2_nacc <- aggregate(
  cbind(mean, sem) ~ tr + condition, 
  data = d.sub2_nacc[d.sub2_nacc$condition %in% c("$0", "+$5"), ], 
  FUN = mean
)

nacc2.tcplot2 = ggplot(data = plot_data2_nacc, aes(x = tr, y = mean, group = condition)) + 
  geom_errorbar(aes(ymin = mean - sem, ymax = mean + sem), width = 0.25, colour = '#939598') +
  geom_line(aes(linetype = condition, colour = condition), size = 1.5) +
  geom_point(aes(colour = condition, fill = condition), shape = 21, size = 5, show.legend = FALSE) +
  scale_linetype_manual('trialtype', values = plot_lines) +
  scale_color_manual('trialtype', values = plot_colors) +
  scale_fill_manual('trialtype', values = plot_fills) +
  scale_x_continuous(breaks = seq(0, 10, by = 2), labels = seq(0, 20, by = 4)) +
  scale_y_continuous(breaks = seq(-0.3, 0.3, by = 0.1), 
                     labels = function(x) ifelse(abs(x) < 1e-10, "0", x),
                     limits = c(-0.3, 0.3)) +
  theme(text = element_text(family = 'Helvetica', size = 20, colour = 'black'),
        axis.text = element_text(family = 'Helvetica', size = 20, colour = 'black'),
        panel.grid.minor = element_blank()) +
  guides(fill = guide_legend(keywidth = 1, keyheight = 1),
         linetype = guide_legend(keywidth = 2, keyheight = 1),
         colour = guide_legend(keywidth = 2, keyheight = 1)) +
  xlab('Seconds') +
  ylab('Percent Signal Change') +
  ggtitle('NAcc subject space timecourse - Ses 2')

# Display plots
nacc2.tcplot1
nacc2.tcplot2

# Save all plots
ggsave("/Users/nate/Desktop/plots/nacc2_subspace_timecourse_ses1.png", plot = nacc2.tcplot1, width = 8, height = 6, dpi = 300)
ggsave("/Users/nate/Desktop/plots/nacc2_subspace_timecourse_ses2.png", plot = nacc2.tcplot2, width = 8, height = 6, dpi = 300)
