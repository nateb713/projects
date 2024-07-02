sports <- read.table('/Users/nathanielbouton/Documents/FAN Lab/abcd txt files/abcd_saiq02.txt')

library(tidyverse)

sports %>%
  filter(V16==1) -> fball # 1384 kids

sports %>%
  filter(V26==1) -> soccer # 4868 kids



sum_sports <- read.table('/Users/nathanielbouton/Documents/FAN Lab/abcd txt files/abcd_spacss01.txt') # this gives info about how many years they played the sports, months per year, how many days per week, etc.

long_sum_sports <- read.table('/Users/nathanielbouton/Documents/FAN Lab/abcd txt files/abcd_lsssa01.txt') #same questions as sum_sports, but "since we last saw you"

lp_sports <- read.delim('/Users/nathanielbouton/Documents/FAN Lab/abcd txt files/abcd_lpsaiq01.txt')

lp_sports %>%
  filter(sai_p_activities_l___5==1) -> fball_long # 2162 kids

lp_sports %>%
  filter(sai_p_activities_l___15==1) -> soccer_long # 5501 kids
