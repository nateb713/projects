#append kidmid timecourses 171213

require(stringr)

# sub07 redo 3 task runs

subjects = c('01','02','03','04','05','06','07',
             '10','11','12','13','14','15','16',
             '17','18','19','20','22','23')

####create dataframe to store values####
d.f=data.frame(
  subject=character(),
  bmid_mbnf_acing_s1.1D = character(),
  bmid_mbnf_ains8mm_mni_s1.1D = character(),
  bmid_mbnf_ains8mmkg_mni_s1.1D = character(),
  bmid_mbnf_ains_mni_s1.1D = character(),
  bmid_mbnf_caudate_s1.1D = character(),
  bmid_mbnf_csf_s1.1D = character(),
  bmid_mbnf_dlpfc_s1.1D = character(),
  bmid_mbnf_ins_s1.1D = character(),
  bmid_mbnf_mpfc8mm_mni_s1.1D = character(),
  bmid_mbnf_mpfc_s1.1D = character(),
  bmid_mbnf_nacc8mm_mni_s1.1D = character(),
  bmid_mbnf_nacc8mm_s1.1D = character(),
  bmid_mbnf_vta_mni_10_s1.1D = character(),
  bmid_mbnf_vta_mni_50_s1.1D = character(),
  bmid_mbnf_wm_s1.1D = character(),
  lmid_mbnf_acing_s1.1D = character(),
  lmid_mbnf_ains8mm_mni_s1.1D = character(),
  lmid_mbnf_ains8mmkg_mni_s1.1D = character(),
  lmid_mbnf_ains_mni_s1.1D = character(),
  lmid_mbnf_caudate_s1.1D = character(),
  lmid_mbnf_csf_s1.1D = character(),
  lmid_mbnf_dlpfc_s1.1D = character(),
  lmid_mbnf_ins_s1.1D = character(),
  lmid_mbnf_mpfc8mm_mni_s1.1D = character(),
  lmid_mbnf_mpfc_s1.1D = character(),
  lmid_mbnf_nacc8mm_mni_s1.1D = character(),
  lmid_mbnf_nacc8mm_s1.1D = character(),
  lmid_mbnf_vta_mni_10_s1.1D = character(),
  lmid_mbnf_vta_mni_50_s1.1D = character(),
  lmid_mbnf_wm_s1.1D = character(),
  rmid_mbnf_acing_s1.1D = character(),
  rmid_mbnf_ains8mm_mni_s1.1D = character(),
  rmid_mbnf_ains8mmkg_mni_s1.1D = character(),
  rmid_mbnf_ains_mni_s1.1D = character(),
  rmid_mbnf_caudate_s1.1D = character(),
  rmid_mbnf_csf_s1.1D = character(),
  rmid_mbnf_dlpfc_s1.1D = character(),
  rmid_mbnf_ins_s1.1D = character(),
  rmid_mbnf_mpfc8mm_mni_s1.1D = character(),
  rmid_mbnf_mpfc_s1.1D = character(),
  rmid_mbnf_nacc8mm_mni_s1.1D = character(),
  rmid_mbnf_nacc8mm_s1.1D = character(),
  rmid_mbnf_vta_mni_10_s1.1D = character(),
  rmid_mbnf_vta_mni_50_s1.1D = character(),
  rmid_mbnf_wm_s1.1D = character(),
  stringsAsFactors=F)

for (sub in 1:length(subjects)){
  #create filenames
  behaviorname = str_c('/Users/jeb/Box Sync/projects/reliability/data/libr_mid_task/MID/data/s',subjects[sub],'/s',subjects[sub],'_ses1_MID.csv')
  writefilename = str_c('/Users/jeb/Box Sync/projects/reliability/tcs/rrl',subjects[sub],'_ses1_tc10tr.csv')
  #create timecourse filenames
  filenames = c()
  for (roi in 1:length(colnames(d.f))){
    filenames[roi] = paste('/Users/jeb/Box Sync/projects/reliability/tcs/mid/','rrl',subjects[sub],'/',colnames(d.f)[roi], sep='')
  }
  
  #load behavior
  d0 = read.csv(behaviorname)
  colnames(d0)
  d1 = d0[-282,]

  #load timecourses
  dataframenames = c()
  for (roi in 1:length(colnames(d.f))){
    dataframenames[roi] = paste('d.',colnames(d.f)[roi], sep='')
  }
  
  #append time courses into data frame
  timecourses = data.frame(matrix(nrow= nrow(d1),ncol=length(colnames(d.f))))
  timecourses[,1] = rep(subjects[sub], nrow(d1))
  for (tc in 2:length(colnames(d.f))){
    timecourses[,tc] = read.table(filenames[tc])
  }
  #get ROI colnames
  colnames(timecourses) = colnames(d.f)
  
  #combine behavior and timecourse
  df = cbind(d1, timecourses)
  
  #make each trial 7 TRs long, with proper trial number, condition, etc.
  tc10tr = c()
  trialonset = which(df$TR==1)
  for(start in trialonset){
    end=start+9
    tr=c(1,2,3,4,5,6,7,8,9,10)
    trial.tmp = data.frame(tr,
                           rep(df[start,1],10),
                           rep(df[start,2],10),
                           rep(df[start,3],10),
                           rep(df[start,4],10),
                           rep(df[start,5],10),
                           rep(df[start,6],10),
                           rep(df[start,7],10),
                           rep(df[start,8],10),
                           rep(df[start,9],10),
                           rep(df[start,10],10),
                           rep(df[start,11],10),
                           rep(df[start,12],10),
                           rep(df[start,13],10),
                           rep(df[start,14],10),
                           rep(df[start,15],10),
                           rep(df[start,16],10),
                           rep(df[start,17],10),
                           df[start:end,18:62])
    tc10tr = rbind(tc10tr,trial.tmp)
  }
  colnames(tc10tr)[1:18] = c('tr',colnames(df[1:17]))
  
  #remove bottom 4 TRs because no extended time course
  #df1 = as.data.frame(tc7tr[-c(497,501:504),])
  write.csv(tc10tr, writefilename, row.names=F)
}



