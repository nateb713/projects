#plot time courses by TR and MID condition hit/miss 171215

require(ggplot2)
require(reshape2)
require(extrafont)
require(plyr)
require(grid)
require(stringr)
library(tidyverse)

d.tc = read.csv('/Users/nathanielbouton/Documents/UArk/FANlab/Test-Retest/MID/data/mid_ses1_tcs_n19.csv', head=T)

d.tc2 = read.csv('/Users/nathanielbouton/Documents/UArk/FANlab/Test-Retest/MID/data/mid_ses2_tcs_n19.csv', head=T)

# Modify column names within the specified range
colnames(d.tc)[19:63] = str_sub(colnames(d.tc)[19:68], 1, -5)
colnames(d.tc2)[19:63] = str_sub(colnames(d.tc2)[19:68], 1, -5)

df <- rbind(d.tc, d.tc2)

#colnames(d.tc)
#head(d.tc)
#str(d.tc)
#dim(d.tc)

d.tc = na.omit(df)

#long form
d.tc.melt = melt(d.tc, id=c('tr','trialtype','subject','block'),
                 measure.vars = c('lmid_mbnf_ains_mni_s','rmid_mbnf_ains_mni_s',
                                  'lmid_mbnf_nacc8mm_s','rmid_mbnf_nacc8mm_s',
                                  'lmid_mbnf_nacc8mm_mni_s','rmid_mbnf_nacc8mm_mni_s',
                                  'lmid_mbnf_mpfc_s','rmid_mbnf_mpfc_s',
                                  'lmid_mbnf_mpfc8mm_mni_s','rmid_mbnf_mpfc8mm_mni_s',
                                  'lmid_mbnf_vta_mni_10_s','rmid_mbnf_vta_mni_10_s'))

#summarize over TR and condition for each subject
d.tc.subject = ddply(d.tc.melt, .variables = c('tr','trialtype','variable','subject','block'),
                     summarise, mean.subject=mean(value), sd.subject=sd(value)/sqrt(length(value)))
#summarize across subjects
d.tc.sum = ddply(d.tc.subject, .variables = c('tr','trialtype','variable','block'),
                 summarise, mean = mean(mean.subject), sem = sd(mean.subject)/sqrt(length(unique(d.tc$subject))))

#change condition number to gamble type name
d.tc.sum$trialtype
d.tc.sum[d.tc.sum$trialtype == 1, 'trialtype'] = '$0'
d.tc.sum[d.tc.sum$trialtype == 2, 'trialtype'] = '-$5'
d.tc.sum[d.tc.sum$trialtype == 3, 'trialtype'] = '+$5'
d.tc.sum$trialtype=as.factor(d.tc.sum$trialtype)
d.tc.sum$trialtype

d.tc.sum$hemi = c()
d.tc.sum[grep('lmid', d.tc.sum$variable), 'hemi'] = 'l'
d.tc.sum[grep('rmid', d.tc.sum$variable), 'hemi'] = 'r'
d.tc.sum$variable = substr(d.tc.sum$variable, start = 11, stop = str_length(d.tc.sum$variable)-2)
d.tc.sum$variable = as.factor(d.tc.sum$variable)
levels(d.tc.sum$variable)
levels(d.tc.sum$trialtype)
d.tc.sum$block = as.factor(d.tc.sum$block)

write.csv(d.tc.sum,  '/Users/nathanielbouton/Documents/UArk/FANlab/Test-Retest/MID/mid_tcs_activity.csv', row.names = F)



# Load new data to average across blocks and hemispheres, and plot

#dp = read.csv('/Users/nathanielbouton/Documents/UArk/FANlab/Test-Retest/MID/mid_tcs_activity.csv', head=T)

d.sub <- subset(d.tc.sum, variable == 'nacc8mm_mni')

#d.agg <- aggregate(mean, sem ~ tr + variable + trialtype, data = d.sub, FUN = mean)

result <- d.sub %>%
  group_by(tr, variable, trialtype) %>%
  summarise(mean = mean(mean),
            sem = mean(sem))

#colors for plotting
cond.color = c('#f96300','navy','grey')
cond.fill = c('#f96300','navy','white')
cond.line = c('solid','solid','dashed')
line.color = c()

#nacc.tc.sum = subset(d.tc.sum, variable=='nacc8mm_mni')
nacc.tcplot = ggplot(data=result, aes(x = tr, y = mean, group=trialtype))+ 
  geom_errorbar(aes(y=mean, ymin=mean-sem, ymax=mean+sem), width=0.25, colour = '#939598')+
  geom_line(aes(linetype=trialtype, colour=trialtype),size=1.5)+
  geom_point(aes(colour=trialtype, fill=trialtype), shape = 21, size=5, show.legend=FALSE)+
  scale_linetype_manual('trialtype',values=cond.line)+
  scale_color_manual(values=cond.color)+
  scale_fill_manual(values=cond.fill)+
  scale_x_continuous(breaks=seq(0,10,by=2),labels=seq(0,20,by=4))+
  scale_y_continuous(breaks=seq(-0.2,0.2,by=0.1),labels=seq(-0.2,0.2,by=0.1), limits=c(-0.2,0.2))+
  #facet_grid(block~hemi)+
  theme(text=element_text(family='Helvetica', size=20, colour = 'black'),
        axis.text=element_text(family='Helvetica', size=20, colour = 'black'),
        #axis.ticks=element_line(colour = 'black', size=0.5),
        panel.grid.minor=element_blank())+
  guides(fill = guide_legend(keywidth = 1, keyheight = 1),
         linetype=guide_legend(keywidth = 2, keyheight = 1),
         colour=guide_legend(keywidth = 2, keyheight = 1))+
  xlab('Seconds')+
  ylab('Percent Signal Change')+
  ggtitle('NAcc timecourse')
nacc.tcplot
#ggsave('mid.ses1.nacc.n19.tcplot.pdf',nacc.tcplot, path='~/',width = 11,height = 7,units = 'in',useDingbats=FALSE)


vta.tc.sum = subset(d.tc.sum, variable=='vta_mni_10')
vta.tcplot = ggplot(data=vta.tc.sum, aes(x = tr, y = mean, group=trialtype))+ 
  geom_errorbar(aes(y=mean, ymin=mean-sem, ymax=mean+sem), width=0.25, colour = '#939598')+
  geom_line(aes(linetype=trialtype, colour=trialtype),size=1.5)+
  geom_point(aes(colour=trialtype, fill=trialtype), shape = 21, size=5, show.legend=FALSE)+
  scale_linetype_manual('trialtype',values=cond.line)+
  scale_color_manual(values=cond.color)+
  scale_fill_manual(values=cond.fill)+
  scale_x_continuous(breaks=seq(0,10,by=1),labels=seq(0,20,by=2))+
  #scale_y_continuous(breaks=seq(-0.3,0.3,by=0.1),labels=seq(-0.3,0.3,by=0.1), limits=c(-0.3,0.3))+
  facet_grid(block~hemi)+
  theme(text=element_text(family='Helvetica', size=20, colour = 'black'),
        axis.text=element_text(family='Helvetica', size=20, colour = 'black'),
        #axis.ticks=element_line(colour = 'black', size=0.5),
        panel.grid.minor=element_blank())+
  guides(fill = guide_legend(keywidth = 1, keyheight = 1),
         linetype=guide_legend(keywidth = 2, keyheight = 1),
         colour=guide_legend(keywidth = 2, keyheight = 1))+
  xlab('Seconds')+
  ylab('Percent Signal Change')+
  ggtitle('VTA timecourse')
vta.tcplot


mpfc.tc.sum = subset(d.tc.sum, variable=='mpfc')
mpfc.tcplot = ggplot(data=mpfc.tc.sum, aes(x = tr, y = mean, group=trialtype))+ 
  geom_errorbar(aes(y=mean, ymin=mean-sem, ymax=mean+sem), width=0.25, colour = '#939598')+
  geom_line(aes(linetype=trialtype, colour=trialtype),size=1.5)+
  geom_point(aes(colour=trialtype, fill=trialtype), shape = 21, size=5, show.legend=FALSE)+
  scale_linetype_manual('trialtype',values=cond.line)+
  scale_color_manual(values=cond.color)+
  scale_fill_manual(values=cond.fill)+
  scale_x_continuous(breaks=seq(0,10,by=1),labels=seq(0,20,by=2))+
  #scale_y_continuous(breaks=seq(-0.3,0.3,by=0.1),labels=seq(-0.3,0.3,by=0.1), limits=c(-0.3,0.3))+
  facet_grid(block~hemi)+
  theme(text=element_text(family='Helvetica', size=20, colour = 'black'),
        axis.text=element_text(family='Helvetica', size=20, colour = 'black'),
        #axis.ticks=element_line(colour = 'black', size=0.5),
        panel.grid.minor=element_blank())+
  guides(fill = guide_legend(keywidth = 1, keyheight = 1),
         linetype=guide_legend(keywidth = 2, keyheight = 1),
         colour=guide_legend(keywidth = 2, keyheight = 1))+
  xlab('Seconds')+
  ylab('Percent Signal Change')+
  ggtitle('MPFC timecourse')
mpfc.tcplot


ains.tc.sum = subset(d.tc.sum, variable=='ains_mni')
ains.tcplot = ggplot(data=ains.tc.sum, aes(x = tr, y = mean, group=trialtype))+ 
  geom_errorbar(aes(y=mean, ymin=mean-sem, ymax=mean+sem), width=0.25, colour = '#939598')+
  geom_line(aes(linetype=trialtype, colour=trialtype),size=1.5)+
  geom_point(aes(colour=trialtype, fill=trialtype), shape = 21, size=5, show.legend=FALSE)+
  scale_linetype_manual('trialtype',values=cond.line)+
  scale_color_manual(values=cond.color)+
  scale_fill_manual(values=cond.fill)+
  scale_x_continuous(breaks=seq(0,10,by=1),labels=seq(0,20,by=2))+
  #scale_y_continuous(breaks=seq(-0.3,0.3,by=0.1),labels=seq(-0.3,0.3,by=0.1), limits=c(-0.3,0.3))+
  facet_grid(block~hemi)+
  theme(text=element_text(family='Helvetica', size=20, colour = 'black'),
        axis.text=element_text(family='Helvetica', size=20, colour = 'black'),
        #axis.ticks=element_line(colour = 'black', size=0.5),
        panel.grid.minor=element_blank())+
  guides(fill = guide_legend(keywidth = 1, keyheight = 1),
         linetype=guide_legend(keywidth = 2, keyheight = 1),
         colour=guide_legend(keywidth = 2, keyheight = 1))+
  xlab('Seconds')+
  ylab('Percent Signal Change')+
  ggtitle('AIns timecourse')
ains.tcplot



