# mean VOI activity midi 161103

require(ggplot2)
require(reshape2)
require(extrafont)
require(plyr)

d.tc = read.csv('/Users/josiah/Box Sync/projects/midirlps/data/midi_tc8tr_n32addicts.csv')
colnames(d.tc)

#d.tc = d.tc[d.tc$subject %in% subvec25, ]
#d.tc$subject = droplevels(d.tc$subject)

#long form
d.tc.melt = melt(d.tc, id=c('TR','trialtype','subject'),
                 measure.vars = c('l_nacc8mm_raw.tc','r_nacc8mm_raw.tc',
                                  'l_ins_raw.tc','r_ins_raw.tc',
                                  'l_nacc_desai_mpm_raw.tc','r_nacc_desai_mpm_raw.tc',
                                  'l_antins_desai_mpm_raw.tc','r_antins_desai_mpm_raw.tc',
                                  'l_mpfc_raw.tc','r_mpfc_raw.tc',
                                  'l_dlpfc_raw.tc','r_dlpfc_raw.tc',
                                  'l_acing_raw.tc','r_acing_raw.tc',
                                  'l_vlpfc_raw.tc','r_vlpfc_raw.tc',
                                  'l_vlpfc_neurosynth_raw.tc','r_vlpfc_neurosynth_raw.tc',
                                  'l_vlpfc_midirlps_ant_raw.tc','r_vlpfc_midirlps_ant_raw.tc',
                                  'l_vlpfc_midirlps_post_raw.tc','r_vlpfc_midirlps_post_raw.tc'))

#summarize over TR and condition for each subject
d.tc.subject = ddply(d.tc.melt, .variables = c('TR','trialtype','variable','subject'),
                     summarise, mean.subject=mean(value))
                     #sd.subject=sd(value)/sqrt(length(value)))

d.tc.wide = dcast(d.tc.subject, subject ~ variable + trialtype + TR)

write.csv(d.tc.wide, '/Users/josiah/Box Sync/projects/midirlps/data/fmri/midi_meanvoibold_addicts.csv', row.names=F)
