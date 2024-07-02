library(tidyverse)

sports <- read.table('/Users/nathanielbouton/Documents/FAN Lab/DD project/abcd txt files/abcd_saiq02.txt')

oh_tbi <- read.table('/Users/nathanielbouton/Documents/FAN Lab/DD project/abcd txt files/abcd_otbi01.txt')

tbi_sports <- merge(oh_tbi,sports, by = "V4", all = TRUE)

tfi_fball <- tbi_sports %>%                #tfi is "treated for injury", 216 kids
  filter(V11.x == 1, V16.y == 1 )

tfi_soccer <- tbi_sports %>%                #tfi is "treated for injury", 685 kids
  filter(V11.x == 1, V26.y == 1 )

tfi_fball_soccer <- merge(tfi_fball,tfi_soccer, by = "V4", all = TRUE)

results <- tfi_fball_soccer[, c("V4","V11.x.x", "V16.y.y", "V26.y.y")]


install.packages("knitr")
install.packages("kableExtra")

library(knitr)
library(kableExtra)

basic_table <- kable(results, "html")

basic_table %>%
  add_header_above(c("subjects", "injury", "fball",
                     "soccer"))
print(styled_table)

results_cleaned <- na.omit(results)

colnames(results_cleaned) <- c("ID", "V4", "Injury", "Football", "Soccer")
rownames(results_cleaned) <- c(V4)
basic_table_2 <- kable(results_cleaned)

basic_table_2 %>%
  add_header_above(c("ID", "subjects", "injury", 
                     "fball","soccer" ))

results_cleaned <- tibble::rownames_to_column(results_cleaned, var = "ID")
results_cleaned <- results_cleaned[, !(names(results_cleaned) %in% c("V4"))]
print(basic_table_2)

install.packages("openxlsx")
library(openxlsx)

write.xlsx(results_cleaned, "table_data.xlsx", rowNames = F)


wm1 <- read.table("/Users/nathanielbouton/Documents/FAN Lab/DD project/abcd txt files/abcd_dmdtifp101.txt")

wm2 <- read.table("/Users/nathanielbouton/Documents/FAN Lab/DD project/abcd txt files/abcd_dmdtifp201.txt")

rc <- merge(results_cleaned, wm1, by = "V4", all = TRUE)

rc_wm <- merge(rc, wm2, by = "V4", all = TRUE)

rc_wm_table <- rc_wm[, c("V4","ID", "Injury", "Football", "Soccer", "V18.x", "V19.x", "V20.x", "V21.x", "V24.x", "V25.x", "V60.x", "V61.x", "V62.x", "V63.x", "V66.x", "V67.x")] # this keeps only the columns you're interested in

rc_wm_final <- na.omit(rc_wm)



write.xlsx(rc_wm_table, "rc_wm.xlsx", rowNames = F) # export to Excel file/table
library(openxlsx)

rc_wm_table_na <- read.xlsx("/Users/nathanielbouton/Documents/FAN Lab/DD project/rc_wm.xlsx")
rc_wm_table_final <- na.omit(rc_wm_table_na) # removed NA values

library(dplyr)
rc_wm_complete <- distinct(rc_wm_table_final) # removed duplicate values
# rc = results cleaned, wm = white matter

colnames(rc_wm_complete) <- c("Subject", "ID", "Injury", "Football", "Soccer",
                              "FA_right_anterior_thalamic_radiations",
                              "FA_left_anterior_thalamic_radiations",
                              "FA_right_uncinate",
                              "FA_left_uncinate",
                              "FA_right_inferior_fronto_occipital_fasiculus",
                              "FA_left_inferior_fronto_occipital_fasiculus",
                              "MD_right_anterior_thalamic_radiations",
                              "MD_left_anterior_thalamic_radiations",
                              "MD_right_uncinate",
                              "MD_left_uncinate",
                              "MD_right_inferior_fronto_occipital_fasiculus",
                              "MD_left_inferior_fronto_occipital_fasiculus")

# rc_wm_complete is now a data frame with a full table of results

hist(rc_wm_complete$FA_right_uncinate)

?ggplot
