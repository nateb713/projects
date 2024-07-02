sum_tbi <- read.delim('/Users/nathanielbouton/Documents/FAN Lab/abcd txt files/abcd_tbi01.txt')
tbi_loc <- sum_tbi %>%
  filter(tbi_ss_ntbiloc > 0) # 135 kids


long_sum_tbi <- read.delim('/Users/nathanielbouton/Documents/FAN Lab/abcd txt files/abcd_lsstbi01.txt')
long_tbi_loc <- long_sum_tbi %>%
  filter(tbi_ss_ntbiloc_l > 0) # 85 kids

oh_tbi <- read.table('/Users/nathanielbouton/Documents/FAN Lab/abcd txt files/abcd_otbi01.txt')
treated_for_injury <- oh_tbi %>%
  filter(V11 == 1) # 1442 kids

long_oh_tbi <- read.table('/Users/nathanielbouton/Documents/FAN Lab/abcd txt files/abcd_lpohstbi01.txt')
long_treated_for_injury <- long_oh_tbi %>%
  filter(V11 == 1) # 502 kids, "since we last saw you has your child been treated for a head or neck injury", 1, 2, or 3 year follow up
