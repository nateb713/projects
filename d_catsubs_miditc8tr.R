#cat all timecourses jl170925

require('stringr')
require('reshape2')

#combine data frame
subvec = c('ag151024','al170316','as170730','at160601','cg160715','cm160510','ds170728',
           'ja160416','jb161004','jc170501','jd170330','jf160703','jw170330','lm160914',
           'mr161024','mr170621','nc160905','rc161007','rs160730','rt170816','se161021',
           'si151120','tj160529','wh160130','zm160627',
           'ds170915','rl170603','rt160420','ts170927','hp170601','nb160221',
           'tg170423')

d1 = data.frame()
for (subj in 1:length(subvec)){
  loadfilename = str_c('/Users/josiah/Box Sync/projects/midirlps/data/fmri/midi_tc8tr/',subvec[subj],'_miditc8tr.csv')
  d0 = read.csv(loadfilename, head=T)
  d0$subject = subvec[subj]
  d1 = rbind(d1,d0)
}

#any NAs?
str(d1)
d1$trial = as.numeric(d1$trial)
d1[c(22560,28088:28314),'subject']

write.csv(d1, '/Users/josiah/Box Sync/projects/midirlps/data/midi_tc8tr_n32addicts.csv', row.names=F)
#write.csv(d1, '/Users/josiah/Box Sync/projects/mididti/data/behavior/midibehavior/midi_matrix_fincue89.csv', row.names=F)

d.long = read.csv('/Users/josiah/Box Sync/projects/midirlps/data/midi_tc8tr_n32addicts.csv')
#wide form
d.melt = melt(d.long, measure.vars = c("b_dlpfc_raw.tc","b_ins_raw.tc","b_antins_desai_mpm_raw.tc",
                                       "b_mpfc_raw.tc","b_nacc8mm_raw.tc","b_nacc_desai_mpm_raw.tc",
                                       "b_acing_raw.tc","b_caudate_raw.tc","b_vlpfc_raw.tc","b_vlpfc_neurosynth_raw.tc",
                                       "b_vlpfc_midirlps_ant_raw.tc","b_vlpfc_midirlps_post_raw.tc",
                                       "r_dlpfc_raw.tc","r_ins_raw.tc","r_antins_desai_mpm_raw.tc",
                                       "r_mpfc_raw.tc","r_nacc8mm_raw.tc","r_nacc_desai_mpm_raw.tc",
                                       "r_acing_raw.tc","r_caudate_raw.tc","r_vlpfc_raw.tc","r_vlpfc_neurosynth_raw.tc",
                                       "r_vlpfc_midirlps_ant_raw.tc","r_vlpfc_midirlps_post_raw.tc",
                                       "l_dlpfc_raw.tc","l_ins_raw.tc","l_nacc_desai_mpm_raw.tc",
                                       "l_mpfc_raw.tc","l_nacc8mm_raw.tc","l_antins_desai_mpm_raw.tc",
                                       "l_acing_raw.tc","l_caudate_raw.tc","l_vlpfc_raw.tc","l_vlpfc_neurosynth_raw.tc",
                                       "l_vlpfc_midirlps_ant_raw.tc","l_vlpfc_midirlps_post_raw.tc"))

d.w = dcast(d.melt, subject + trial + trialonset + trialtype + target_ms + rt + cue_value + hit.win + trial_gain +
              total + iti + drift + total_winpercent + binned_winpercent ~ variable + TR, value.var = 'value')


write.csv(d.w, '/Users/josiah/Box Sync/projects/midirlps/data/midi_tc8tr_n32addicts_wide.csv', row.names=F)



