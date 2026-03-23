require(psych)
require(stringr)

# Read the NAcc2 reliability data (created by your reliability analysis script)
d0 = read.csv('/Users/nate/Desktop/subspace/nacc_timecourses/nacc2_reliability.csv', head=T)

# Define parameters for NAcc2 only
area = c('nacc2')  # Only NAcc2 now
cond = c('gain','loss','neutral')
hemi = c('b','l','r')  # bilateral, left, right

# Initialize dataframe to store results
df = c()

# Loop through all combinations
for (brain in 1:length(area)){
  for (reward in 1:length(cond)){
    for (sphere in 1:length(hemi)){
      for (tr in 1:7){  # Only TRs 1-7 to match your example plot
        
        # Create column names for ICC analysis
        icc_name = str_c(area[brain],'_',
                         cond[reward],'_',
                         hemi[sphere],'_',
                         tr,
                         sep = '')
        
        s1_data = str_c(area[brain],'_',
                        's1','_',
                        cond[reward],'_',
                        hemi[sphere],'_',
                        tr,
                        sep = '')
        
        s2_data = str_c(area[brain],'_',
                        's2','_',
                        cond[reward],'_',
                        hemi[sphere],'_',
                        tr,
                        sep = '')
        
        cat("Processing:", s1_data, "vs", s2_data, "\n")
        
        # Try to calculate ICC with error handling
        res_tmp = c()
        tryCatch(
          expr = {
            # Check if columns exist in the data
            if(s1_data %in% colnames(d0) & s2_data %in% colnames(d0)) {
              res_tmp = ICC(d0[,c(s1_data,s2_data)])
              cat('  Success\n')
              
              # Extract ICC(3,k) results (row 6 in ICC output)
              data_tmp = c(icc_name, 
                           res_tmp$results[6,2],  # ICC estimate
                           res_tmp$results[6,7],  # Lower bound
                           res_tmp$results[6,8])  # Upper bound
            } else {
              cat('  Columns not found\n')
              data_tmp = c(icc_name, NA, NA, NA)
            }
          }, 
          error = function(e){
            cat('  Error:', e$message, '\n')
            data_tmp = c(icc_name, NA, NA, NA)
          })
        
        # Add to results dataframe
        df = rbind(df, data_tmp)
      }
    }
  }
}

# Create final dataframe
dtmp = data.frame(df, stringsAsFactors = FALSE)
colnames(dtmp) = c('roi','icc3k','lower','upper')

# Convert ICC values to numeric
dtmp$icc3k = as.numeric(dtmp$icc3k)
dtmp$lower = as.numeric(dtmp$lower)
dtmp$upper = as.numeric(dtmp$upper)

# Save results
write.csv(dtmp, '/Users/nate/Desktop/subspace/nacc_timecourses/nacc2_icc3k.csv', row.names=F)

# Display summary
cat("\n=== ICC Calculation Summary ===\n")
cat("Total comparisons attempted:", nrow(dtmp), "\n")
cat("Successful calculations:", sum(!is.na(dtmp$icc3k)), "\n")
cat("Failed calculations:", sum(is.na(dtmp$icc3k)), "\n")

# Show a few example results
cat("\n=== Sample ICC Results ===\n")
valid_results = dtmp[!is.na(dtmp$icc3k), ]
if(nrow(valid_results) > 0) {
  print(head(valid_results, 10))
} else {
  cat("No valid ICC calculations found. Check column names in your data.\n")
  cat("Available columns in your data:\n")
  print(colnames(d0))
}

# Manual ICC calculations for verification (NAcc2 gain condition examples)
cat("\n=== Manual Verification Examples ===\n")

# Check what columns are actually available
nacc2_cols = grep("nacc2", colnames(d0), value = TRUE)
cat("Available NAcc2 columns:\n")
print(nacc2_cols)

# Try a few manual calculations if columns exist
manual_tests = c(
  c('nacc2_s1_gain_b_1','nacc2_s2_gain_b_1'),
  c('nacc2_s1_gain_l_3','nacc2_s2_gain_l_3'),
  c('nacc2_s1_gain_r_4','nacc2_s2_gain_r_4')
)

for(i in 1:length(manual_tests)/2) {
  col1 = manual_tests[i*2-1]
  col2 = manual_tests[i*2]
  
  if(col1 %in% colnames(d0) & col2 %in% colnames(d0)) {
    cat("\nTesting", col1, "vs", col2, ":\n")
    tryCatch({
      result = ICC(d0[,c(col1, col2)])
      cat("ICC(3,k) =", round(result$results[6,2], 3), 
          "95% CI: [", round(result$results[6,7], 3), ",", 
          round(result$results[6,8], 3), "]\n")
    }, error = function(e) {
      cat("Error:", e$message, "\n")
    })
  } else {
    cat("\nColumns", col1, "and", col2, "not found\n")
  }
}

cat("\n=== Next Steps ===\n")
cat("1. Check the saved CSV file: nacc2_icc3k.csv\n")
cat("2. Use this file as input for the ICC plotting script\n")
cat("3. If many calculations failed, verify column names match expected format\n")