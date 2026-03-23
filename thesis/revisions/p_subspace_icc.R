require(stringr)
require(reshape2)
require(ggplot2)

# Read the ICC data created by the calculation script
dp = read.csv('/Users/nate/Desktop/subspace/nacc_timecourses/nacc2_icc3k.csv', head=T)

# Process the data exactly like your original script
dp[grep('_b_', dp$roi), 'hemi'] = 'Bilateral'
dp[grep('_l_', dp$roi), 'hemi'] = 'Left'
dp[grep('_r_', dp$roi), 'hemi'] = 'Right'

dp[grep('_gain_', dp$roi), 'cond'] = '+$5'
dp[grep('_loss_', dp$roi), 'cond'] = '-$5'
dp[grep('_neutral_', dp$roi), 'cond'] = '$0'

# Extract TR information exactly like your original
dp[grep('_1$', dp$roi), 'tr'] = 1
dp[grep('_2$', dp$roi), 'tr'] = 2
dp[grep('_3$', dp$roi), 'tr'] = 3
dp[grep('_4$', dp$roi), 'tr'] = 4
dp[grep('_5$', dp$roi), 'tr'] = 5
dp[grep('_6$', dp$roi), 'tr'] = 6
dp[grep('_7$', dp$roi), 'tr'] = 7

# Clean up ROI names
dp$roi = 'nacc2'

# Filter exactly like your original - remove bilateral and loss
dp = dp[dp$hemi != 'Bilateral',]
dp = dp[dp$cond != '-$5',]

# Convert to numeric
dp$icc3k = as.numeric(dp$icc3k)
dp$lower = as.numeric(dp$lower)
dp$upper = as.numeric(dp$upper)

# Define colors exactly like your original
cond.color = c('black','grey50')  
cond.fill = c('black','white')   
cond.line = c('solid','dashed') 

# Truncate lower bounds at 0 and upper bounds at 1 exactly like your original
dp$lower = pmax(0, dp$lower)
dp$upper = pmin(1, dp$upper)

# Create the NAcc2 ICC plot exactly like your original
p_nacc2 = ggplot(dp, aes(x = tr, y = icc3k, group = cond))+
  geom_line(aes(colour=cond, linetype=cond), linewidth=1)+
  geom_errorbar(aes(ymin=lower, ymax=upper, colour=cond), width=0.3)+
  geom_point(aes(colour=cond, fill=cond), shape=21, size=3)+
  scale_linetype_manual(values = cond.line) +
  scale_color_manual(values=cond.color)+
  scale_fill_manual(values=cond.fill)+
  scale_x_continuous(breaks=seq(1,7,by=1), labels=seq(1,7,by=1))+
  scale_y_continuous(breaks=seq(0,1,by=0.2), labels=seq(0,1,by=0.2), limits=c(0,1.05))+
  facet_grid(~hemi)+
  theme(text=element_text(family='Helvetica', size=12, colour = 'black'),
        axis.text=element_text(family='Helvetica', size=12, colour = 'black'),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background = element_rect(fill = "grey90"),
        axis.ticks=element_line(colour = 'black', size=0.5))+
  guides(fill = guide_legend(keywidth = 1, keyheight = 1),
         linetype=guide_legend(keywidth = 2, keyheight = 1),
         colour=guide_legend(keywidth = 2, keyheight = 1))+
  xlab('Task timing (TR = 2 sec)')+
  ylab('Reliability ICC(3,k)')+
  ggtitle('NAcc subject space ICC')

# Display the plot
p_nacc2

ggsave("/Users/nate/Desktop/plots/subspace_icc.png", plot = p_nacc2, width = 8, height = 6, dpi = 300)