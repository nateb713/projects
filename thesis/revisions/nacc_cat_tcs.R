# NAcc2 timecourse combination script
# Modified for subject-specific NAcc2 masks with proper L/R indexing

require(stringr)

# Updated subject list (16 subjects)
subjects = c('01','02','05','06','07','10','11','12','13','14','15','17','18','19','22','23')

# Simplified dataframe structure for NAcc2 only
d.f=data.frame(
  subject=character(),
  bmid_mbnf_nacc2_s1.1D = character(),
  lmid_mbnf_nacc2_s1.1D = character(),
  rmid_mbnf_nacc2_s1.1D = character(),
  stringsAsFactors=F)

# Process both sessions
sessions = c('s1', 's2')

for (session in sessions) {
  cat("Processing session:", session, "\n")
  
  for (sub in 1:length(subjects)) {
    cat("  Processing subject:", subjects[sub], "\n")
    
    # Create filenames
    behaviorname = str_c('/Users/nate/Documents/UArk/FANlab/masters/Test-Retest/data/taskbhvr/mid/s',subjects[sub],'/s',subjects[sub],'_ses',substr(session,2,2),'_MID.csv')
    writefilename = str_c('/Users/nate/Desktop/subspace/nacc_timecourses/rrl',subjects[sub],'_',session,'_nacc2_tc10tr.csv')
    
    # Create timecourse filenames for NAcc2 only
    filenames = c()
    filenames[1] = paste('rrl', subjects[sub], sep='') # subject column
    filenames[2] = paste('/Users/nate/Desktop/subspace/nacc_timecourses/rrl',subjects[sub],'/',session,'/bmid_mbnf_nacc2_',session,'.1D', sep='')
    filenames[3] = paste('/Users/nate/Desktop/subspace/nacc_timecourses/rrl',subjects[sub],'/',session,'/lmid_mbnf_nacc2_',session,'.1D', sep='')
    filenames[4] = paste('/Users/nate/Desktop/subspace/nacc_timecourses/rrl',subjects[sub],'/',session,'/rmid_mbnf_nacc2_',session,'.1D', sep='')
    
    # Load behavior
    d0 = read.csv(behaviorname)
    d1 = d0[-282,]
    
    # Load timecourses
    timecourses = data.frame(matrix(nrow= nrow(d1), ncol=length(colnames(d.f))))
    timecourses[,1] = rep(subjects[sub], nrow(d1))
    for (tc in 2:4) {
      timecourses[,tc] = read.table(filenames[tc])
    }
    # Set column names
    colnames(timecourses) = colnames(d.f)
    
    # Combine behavior and timecourse
    df = cbind(d1, timecourses)
    
    # Make each trial 10 TRs long
    tc10tr = c()
    trialonset = which(df$TR==1)
    for(start in trialonset) {
      end = start + 9
      tr = c(1,2,3,4,5,6,7,8,9,10)
      
      # Get the number of behavioral columns
      n_behav_cols = ncol(d1)
      
      trial.tmp = data.frame(
        tr,
        # Replicate behavioral data for all 10 TRs
        do.call(cbind, lapply(1:n_behav_cols, function(i) rep(df[start,i], 10))),
        # Add timecourse data for these 10 TRs
        df[start:end, (n_behav_cols+1):ncol(df)]
      )
      tc10tr = rbind(tc10tr, trial.tmp)
    }
    colnames(tc10tr)[1:(n_behav_cols+1)] = c('tr', colnames(d1))
    
    # Add session identifier
    tc10tr$ses = session
    
    # Write output file
    write.csv(tc10tr, writefilename, row.names=F)
  }
}

cat("Processing complete for all subjects and sessions!\n")
