require(stringr)

# load subject
d0 = read.csv('/Users/nathanielbouton/Documents/UArk/FANlab/Test-Retest/MIL/data/rrl06_ses1_tc10tr.csv', head=T)

# subset to 1 row per trial
d1 = subset(d0, TR == 1)

# subset by condition (1=neutral, 2=loss, 3=gain)
d.1 = subset(d1, trialtype == 1)
d.2 = subset(d1, trialtype == 2)
d.3 = subset(d1, trialtype == 3)

# find first trial that subject picked "better" fractal
# get the button they pressed
d.1_bp = d.1[which(d.1$hit == 1)[1],'bp']
# lowercase the button and append to _cue_value to create column name
# store the bmp filename
d.1_better = d.1[which(d.1$hit == 1)[1],str_c(str_to_lower(d.1_bp),'_cue_value')]

# rinse repeat for other conditions
d.2_bp = d.2[which(d.2$hit == 1)[1],'bp']
d.2_better = d.2[which(d.2$hit == 1)[1],str_c(str_to_lower(d.2_bp),'_cue_value')]

d.3_bp = d.3[which(d.3$hit == 1)[1],'bp']
d.3_better = d.3[which(d.3$hit == 1)[1],str_c(str_to_lower(d.3_bp),'_cue_value')]

# save into dataframe / table
# turn this into for-loop across subjects

# create 1 column that gives tr for better fractal. calculate average brain activity for better fractal, independent of order (first or second). gain1 will be better, gain2 will be worse.
# sometimes we want tr 3 activity, sometimes we want 4. Order in csv file tells us if it's tr3 or tr4.
# need master key that tells you which index to look for in the mean calculation.
# correct choice win, correct choice loss, incorrect choice, incorrect loss (across 3 conditions)
# activity for fractal they chose.

# columns for roi_hemi_gain1_bp_hit_trialgain (hit column is if they chose better fractal or not). Not doing this for all trs. Choice is trs 6 to 10. 

# different scripts for data munging and for saving csv files. will want different scripts for viewing period which is what josiah sent in slack, and for choice section which is two rows above.
