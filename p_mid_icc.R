
require(stringr)
require(reshape2)
require(ggplot2)

dp = read.csv('/Users/nathanielbouton/Documents/UArk/FANlab/Test-Retest/MID/data/icc3k.csv', head=T)
dp[grep('_b_',dp$roi),'hemi'] = 'Bilateral'
dp[grep('_l_',dp$roi),'hemi'] = 'Left'
dp[grep('_r_',dp$roi),'hemi'] = 'Right'

dp[grep('_gain_',dp$roi),'cond'] = '+$5'
dp[grep('_loss_',dp$roi),'cond'] = '-$5'
dp[grep('_neutral_',dp$roi),'cond'] = '$0'

dp$roi = str_replace_all(dp$roi,'_10','_ten')
dp$roi = str_replace_all(dp$roi,'_50','_fif')

dp[grep('_1',dp$roi),'tr'] = 1
dp[grep('_2',dp$roi),'tr'] = 2
dp[grep('_3',dp$roi),'tr'] = 3
dp[grep('_4',dp$roi),'tr'] = 4
dp[grep('_5',dp$roi),'tr'] = 5
dp[grep('_6',dp$roi),'tr'] = 6
dp[grep('_7',dp$roi),'tr'] = 7

dp$roi = str_replace_all(dp$roi,'ral','')
dp$roi = str_sub(dp$roi,1,-10)

#dp = dp[dp$hemi != 'Bilateral',]
dp = dp[dp$hemi != 'Left',]
dp = dp[dp$hemi != 'Right',]

#d.sub <- subset(dp, roi == 'nacc_mni')
#d.agg <- aggregate(icc3k ~ roi + lower + upper + cond + tr, data = d.sub, FUN = mean)

cond.color=c('#555555','#9D2235','cyan')

# plot each brain area

d_nacc = subset(dp, roi == 'nacc_mni')
p_nacc = ggplot(d_nacc, aes(x = tr, y = icc3k, group = cond))+
  geom_point(aes(colour=cond), shape = 19, size=3)+
  geom_errorbar(aes(ymin=lower,ymax=upper,colour=cond),width=0.3)+
  geom_line(aes(colour=cond),linewidth=1)+
  scale_color_manual(values=cond.color)+
  guides(colour=guide_legend(title="Reward"))+
  #scale_fill_manual(values=cond.fill)+
  scale_x_continuous(breaks=seq(0,7,by=1),labels=seq(0,7,by=1))+
  scale_y_continuous(breaks=seq(0,1,by=0.2),labels=seq(0,1,by=0.2), limits=c(0,1))+
  xlab('Task timing (TR = 2 sec)')+
  ylab('Reliability ICC(3,k)')+
  facet_grid(~hemi)+
  theme(text=element_text(family='Helvetica', size=12, colour = 'black'),
        axis.text=element_text(family='Helvetica', size=12, colour = 'black'),
        #panel.grid.major=element_blank(),
        #axis.ticks=element_line(colour = 'black', size=0.5),
        panel.grid.minor=element_blank())+
    guides(fill = guide_legend(keywidth = 1, keyheight = 1),
       linetype=guide_legend(keywidth = 2, keyheight = 1),
       colour=guide_legend(keywidth = 2, keyheight = 1))+
  xlab('Task timing (TR = 2 sec)')+
  ylab('Reliability ICC(3,k)')+
  ggtitle('NAcc ICC')
p_nacc

#insula
d_ains = subset(dp, roi == 'ains_kg')
p_ains = ggplot(d_ains, aes(x = tr, y = icc3k, group = cond))+
  geom_point(aes(colour=cond), shape = 19, size=4)+
  geom_errorbar(aes(ymin=lower,ymax=upper,colour=cond),width=0.3)+
  geom_line(aes(colour=cond),linewidth=1)+
  scale_color_manual(values=cond.color)+
  guides(colour=guide_legend(title="Reward"))+
  #scale_fill_manual(values=cond.fill)+
  scale_x_continuous(breaks=seq(0,7,by=1),labels=seq(0,7,by=1))+
  scale_y_continuous(breaks=seq(0,1,by=0.2),labels=seq(0,1,by=0.2), limits=c(0,1))+
  xlab('Task timing (TR = 2 sec)')+
  ylab('Reliability ICC(3,k)')+
  facet_grid(~hemi)+
  theme(text=element_text(family='Lato', size=18, colour = 'black'),
        axis.text=element_text(family='Helvetica', size=12, colour = 'black'),
        #panel.grid.major=element_blank(),
        #axis.ticks=element_line(colour = 'black', size=0.5),
        panel.grid.minor=element_blank())
p_ains


#vta
d_vta = subset(dp, roi == 'vta_mni_fif')
p_vta = ggplot(d_vta, aes(x = tr, y = icc3k, group = cond))+
  geom_point(aes(colour=cond), shape = 19, size=4)+
  geom_errorbar(aes(ymin=lower,ymax=upper,colour=cond),width=0.3)+
  geom_line(aes(colour=cond),linewidth=1)+
  scale_color_manual(values=cond.color)+
  guides(colour=guide_legend(title="Reward"))+
  #scale_fill_manual(values=cond.fill)+
  scale_x_continuous(breaks=seq(0,7,by=1),labels=seq(0,7,by=1))+
  scale_y_continuous(breaks=seq(0,1,by=0.2),labels=seq(0,1,by=0.2), limits=c(0,1))+
  xlab('Task timing (TR = 2 sec)')+
  ylab('Reliability ICC(3,k)')+
  facet_grid(~hemi)+
  theme(text=element_text(family='Lato', size=18, colour = 'black'),
        axis.text=element_text(family='Helvetica', size=12, colour = 'black'),
        #panel.grid.major=element_blank(),
        #axis.ticks=element_line(colour = 'black', size=0.5),
        panel.grid.minor=element_blank())
p_vta


#mpfc
d_mpfc = subset(dp, roi == 'mpfc_mni')
p_mpfc = ggplot(d_mpfc, aes(x = tr, y = icc3k, group = cond))+
  geom_point(aes(colour=cond), shape = 19, size=4)+
  geom_errorbar(aes(ymin=lower,ymax=upper,colour=cond),width=0.3)+
  geom_line(aes(colour=cond),linewidth=1)+
  scale_color_manual(values=cond.color)+
  guides(colour=guide_legend(title="Reward"))+
  #scale_fill_manual(values=cond.fill)+
  scale_x_continuous(breaks=seq(0,7,by=1),labels=seq(0,7,by=1))+
  scale_y_continuous(breaks=seq(0,1,by=0.2),labels=seq(0,1,by=0.2), limits=c(0,1))+
  xlab('Task timing (TR = 2 sec)')+
  ylab('Reliability ICC(3,k)')+
  facet_grid(~hemi)+
  theme(text=element_text(family='Lato', size=18, colour = 'black'),
        axis.text=element_text(family='Helvetica', size=12, colour = 'black'),
        #panel.grid.major=element_blank(),
        #axis.ticks=element_line(colour = 'black', size=0.5),
        panel.grid.minor=element_blank())
p_mpfc


#mpfc
d_mpfc = subset(dp, roi == 'mpfc')
p_mpfc = ggplot(d_mpfc, aes(x = tr, y = icc3k, group = cond))+
  geom_point(aes(colour=cond), shape = 19, size=4)+
  #geom_errorbar(aes(ymin=upper,ymax=upper,colour=cond),width=0.3)+
  geom_line(aes(colour=cond),linewidth=1)+
  scale_color_manual(values=cond.color)+
  guides(colour=guide_legend(title="Reward"))+
  #scale_fill_manual(values=cond.fill)+
  scale_x_continuous(breaks=seq(0,7,by=1),labels=seq(0,7,by=1))+
  scale_y_continuous(breaks=seq(0,1,by=0.2),labels=seq(0,1,by=0.2), limits=c(0,1))+
  xlab('Task timing (TR = 2 sec)')+
  ylab('Reliability ICC(3,k)')+
  facet_grid(~hemi)+
  theme(text=element_text(family='Lato', size=18, colour = 'black'),
        axis.text=element_text(family='Helvetica', size=12, colour = 'black'),
        #panel.grid.major=element_blank(),
        #axis.ticks=element_line(colour = 'black', size=0.5),
        panel.grid.minor=element_blank())
p_mpfc



#caudate
d_caud = subset(dp, roi == 'caudate')
p_caud = ggplot(d_caud, aes(x = tr, y = icc3k, group = cond))+
  geom_point(aes(colour=cond), shape = 19, size=4)+
  geom_errorbar(aes(ymin=lower,ymax=upper,colour=cond),width=0.3)+
  geom_line(aes(colour=cond),linewidth=1)+
  scale_color_manual(values=cond.color)+
  guides(colour=guide_legend(title="Reward"))+
  #scale_fill_manual(values=cond.fill)+
  scale_x_continuous(breaks=seq(0,7,by=1),labels=seq(0,7,by=1))+
  scale_y_continuous(breaks=seq(0,1,by=0.2),labels=seq(0,1,by=0.2), limits=c(0,1))+
  xlab('Task timing (TR = 2 sec)')+
  ylab('Reliability ICC(3,k)')+
  facet_grid(~hemi)+
  theme(text=element_text(family='Lato', size=18, colour = 'black'),
        axis.text=element_text(family='Helvetica', size=12, colour = 'black'),
        #panel.grid.major=element_blank(),
        #axis.ticks=element_line(colour = 'black', size=0.5),
        panel.grid.minor=element_blank())
p_caud



#dlpfc
d_dlpfc = subset(dp, roi == 'dlpfc')
p_dlpfc = ggplot(d_dlpfc, aes(x = tr, y = icc3k, group = cond))+
  geom_point(aes(colour=cond), shape = 19, size=4)+
  geom_errorbar(aes(ymin=lower,ymax=upper,colour=cond),width=0.3)+
  geom_line(aes(colour=cond),linewidth=1)+
  scale_color_manual(values=cond.color)+
  guides(colour=guide_legend(title="Reward"))+
  #scale_fill_manual(values=cond.fill)+
  scale_x_continuous(breaks=seq(0,7,by=1),labels=seq(0,7,by=1))+
  scale_y_continuous(breaks=seq(0,1,by=0.2),labels=seq(0,1,by=0.2), limits=c(0,1))+
  xlab('Task timing (TR = 2 sec)')+
  ylab('Reliability ICC(3,k)')+
  facet_grid(~hemi)+
  theme(text=element_text(family='Lato', size=18, colour = 'black'),
        axis.text=element_text(family='Helvetica', size=12, colour = 'black'),
        #panel.grid.major=element_blank(),
        #axis.ticks=element_line(colour = 'black', size=0.5),
        panel.grid.minor=element_blank())
p_dlpfc


#csf
d_csf = subset(dp, roi == 'csf')
p_csf = ggplot(d_csf, aes(x = tr, y = icc3k, group = cond))+
  geom_point(aes(colour=cond), shape = 19, size=4)+
  #geom_errorbar(aes(ymin=upper,ymax=upper,colour=cond),width=0.3)+
  geom_line(aes(colour=cond),linewidth=1)+
  scale_color_manual(values=cond.color)+
  guides(colour=guide_legend(title="Reward"))+
  #scale_fill_manual(values=cond.fill)+
  scale_x_continuous(breaks=seq(0,7,by=1),labels=seq(0,7,by=1))+
  scale_y_continuous(breaks=seq(0,1,by=0.2),labels=seq(0,1,by=0.2), limits=c(0,1))+
  xlab('Task timing (TR = 2 sec)')+
  ylab('Reliability ICC(3,k)')+
  facet_grid(~hemi)+
  theme(text=element_text(family='Lato', size=18, colour = 'black'),
        axis.text=element_text(family='Helvetica', size=12, colour = 'black'),
        #panel.grid.major=element_blank(),
        #axis.ticks=element_line(colour = 'black', size=0.5),
        panel.grid.minor=element_blank())
p_csf

#wm
d_wm = subset(dp, roi == 'wm')
p_wm = ggplot(d_wm, aes(x = tr, y = icc3k, group = cond))+
  geom_point(aes(colour=cond), shape = 19, size=4)+
  #geom_errorbar(aes(ymin=upper,ymax=upper,colour=cond),width=0.3)+
  geom_line(aes(colour=cond),linewidth=1)+
  scale_color_manual(values=cond.color)+
  guides(colour=guide_legend(title="Reward"))+
  #scale_fill_manual(values=cond.fill)+
  scale_x_continuous(breaks=seq(0,7,by=1),labels=seq(0,7,by=1))+
  scale_y_continuous(breaks=seq(0,1,by=0.2),labels=seq(0,1,by=0.2), limits=c(0,1))+
  xlab('Task timing (TR = 2 sec)')+
  ylab('Reliability ICC(3,k)')+
  facet_grid(~hemi)+
  theme(text=element_text(family='Lato', size=18, colour = 'black'),
        axis.text=element_text(family='Helvetica', size=12, colour = 'black'),
        #panel.grid.major=element_blank(),
        #axis.ticks=element_line(colour = 'black', size=0.5),
        panel.grid.minor=element_blank())
p_wm
