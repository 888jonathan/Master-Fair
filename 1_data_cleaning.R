################################################## !! Read First!! ###################################
## Before running the following code, if one only need to access the cleaned data,
## one may simply readRDS("./data/cleaned.rds") or readRDS("./data/cleaned_with_mths_since.rds")
## in 2_eda_graphing.R, 3_premodeling.R., 4_modeling.R
## The following code shows the complete cleaning process and the last line of code save the cleaned
## data to an RDS file. For eda, modeling purpose, one might not have to re-clean the data everytime.
## Simply readRDS can save some time and computing resources.
################################################## !! Read First!! ###################################

library("tidyverse")
library("lubridate")
library("reshape2")
library("psych")

# Custom functions that deal with sensitive attributes and the Reweighing algorithm
source("./src/clean_race.R")     # Custom function for creating sensitive tag(0/1)
# source("./src/clean_holc.R")   # Variation 1 for sensitive tag
# source("./src/clean_gini.R")   # Variation 2 for sensitive tag
source("./src/reweighing_dplyr.R") # The reweighing algorithm

# Custom functions for summarizing Macroeconomic ZIP-code level data
source("./src/clean_income.R")
source("./src/clean_beaGDP.R")

#設定資料夾路徑
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
# print(getwd())

###########################################################################
#                    1. Select Variables included                         #
# The list of columns to keep has been stored as an rds file.             #
# One may skip to step 2. directly and read "./data/columns_to_select.rds"#
###########################################################################

# # 1.1 Check missing values of each column
# full <- read_csv("./data/Loan_status_2007-2020Q3.gzip", col_names = TRUE)
# full$issue_d <- my(full$issue_d)
# full <- full %>%
#   filter(issue_d > "2009-12-01") %>%  # Sort out issue_d of 2007-2009,
#                                       # according to Jagtiani and Lemieux (2019)
#   arrange(issue_d)
# 
# # 1.2 Variables that have more than 20% of missing values
# miss_val <- full %>%
#   summarise_all(list(~round(sum(is.na(.))/length(.),4))) %>%
#   pivot_longer(everything()) %>%
#   filter(value > 0.2)
# 
# # View(miss_val)
# 
# # Variables with more than 50% of missing values will be dropped
# # Except variables beginning with name "mths_since_".
# # Reasons explained below in section 2.2.
# 
# # full1 <- full %>%
# #   select(-where(~mean(is.na(.)) > 0.5))
# #
# # miss_val1 <- full1 %>%
# #   summarise_all(list(~round(sum(is.na(.))/length(.),4))) %>%
# #   pivot_longer(everything()) %>%
# #   filter(value > 0.2)
# # open_acc_6m, open_act_il, open_il_12m, open_il_24m, total_bal_il, open_rv_12m, open_rv_24m, max_bal_bc, inq_fi,
# # total_cu_tl, inq_last_12m, il_util, all_util, mths_since_rcnt_il
# 
# # View(miss_val1)
# 
# # The leftover variables with more than 20% of missing data
# # are dropped as well. Higher number of samples is preferred
# # for a better model.
# # drop_list includes variables with more than 50% of missing values
# # miss_val <- full %>%
# #   summarise_all(list(~round(sum(is.na(.))/length(.),4))) %>%
# #   pivot_longer(everything()) %>%
# #   filter(value > 0.32)
# 
# drop_list <- miss_val[,1,drop=T]
# 
# # drop_list2 includes variables not created upon loan application or unrelated variables
# drop_list2 <- c("...1", "id", "funded_amnt", "funded_amnt_inv", "installment",
#                 "sub_grade", "emp_title", "verification_status", "pymnt_plan",
#                 "url", "title", "initial_list_status", "out_prncp",
#                 "out_prncp_inv", "total_pymnt", "total_pymnt_inv", "total_rec_prncp",
#                 "total_rec_int", "total_rec_late_fee", "recoveries", "collection_recovery_fee",
#                 "last_pymnt_d", "last_pymnt_amnt", "next_pymnt_d", "last_credit_pull_d",
#                 "last_fico_range_high", "last_fico_range_low", "collections_12_mths_ex_med",
#                 "policy_code", "num_tl_120dpd_2m", "num_tl_30dpd", "revol_bal_joint")
# 
# # drop_list3 includes variables related to secondary applicants, hardship.
# # columns 116-142 are related to second applicants & hardship,
# # Not every applicants have a secondary applicant, so related variables are not that informative.
# # Plus, there's already another variable "application_type" recording whether the application is
# # "Individual" or "Joint App".
# # Hardship related variables are created after loan approval,
# # obviously not to be included in the model.
# # Furthermore, most of the data in these columns are NAs, so they are excluded.
# drop_list3 <- names(full)[116:142]
# 
# # Combine the list (of course, as vector).
# drop_list <- unique(c(drop_list,drop_list2,drop_list3))
# 
# # Keep the variables for rest the jobs
# columns_to_select <- setdiff(names(full), drop_list)
# 
# ################## Add "mths_since" variables back. Explained in 2.2
# ################## Cleaned dataset with these variables will output the "cleaned_with_mths_since.rds"
# columns_to_select <- unique(c(columns_to_select,
#                               c("mths_since_last_delinq","mths_since_last_record",
#                                 "mths_since_last_major_derog", "mths_since_recent_bc_dlq",
#                                 "mths_since_recent_revol_delinq")))
# ##############
# saveRDS(columns_to_select, "./data/columns_to_select.rds")
# rm(full)
# #rm(full1) # Cleaning......
# gc()            # Release RAM
# # ================================================================================

##########################################################################
#                           2. Data Cleaning                             #
##########################################################################
# list.files("./data")
columns_to_select <- readRDS("./data/columns_to_select.rds") # The RDS file consists of variables chosen above
df <- read_csv("./data/Loan_status_2007-2020Q3.gzip", col_names = TRUE, # 2,925,493 samples
               col_select = all_of(columns_to_select))

## 2.1 Sort out issue_d of 2007-2009, according to Jagtiani and Lemieux (2019)
## Keeping income-verified loan applications 
df$issue_d <- my(df$issue_d) # chr to Date
df <- df %>% 
  filter(issue_d > "2009-12-01") %>% 
  arrange(issue_d)

## 2.2 Impute "mths_since" variables with number 999 or max() + 1
## This is because some applicants may never have public records, delinquencies, etc.
## Therefore, 999(months) is imputed to represents the case of the situation never presented.
# df <- df %>%
#   mutate(across(which(str_starts(names(df),"mths_since")), ~replace_na(.x, 999)))

## Alternative strategy, impute with max()+1
# Treat "mths_since" variables NAs by filling with max()+1
df <- df %>%
  mutate(across(which(str_starts(names(df),"mths_since")), ~replace_na(.x, max(.x, na.rm = TRUE)+1)))

## 2.3 Drop the empty(NA) samples
df <- drop_na(df)

## 2.4 Create variables that shows the months difference 
## between loan issuing date and earliest credit line of applicants.
df$earliest_cr_line <- my(df$earliest_cr_line) # chr to Date
# Loan issue date minus the applicant's earliest credit line
df$days_issue_minus_earliest <- interval(df$earliest_cr_line, df$issue_d) %/% months(1) 
# ggplot(df, aes(x=grade, y=days_issue_minus_earliest)) +
#   geom_boxplot()
# This created variable may not be decisive in terms of grading, according to the plot.

## 2.5 revol_util, int_rate to numeric (was encoded as "XX.X%" format)
df$revol_util <- parse_number(df$revol_util) 
df$int_rate <- parse_number(df$int_rate) # This variable will be excluded from modeling

## 2.6 Combine fico low/high to fico mean
# The range between low & high are all 4
# Therefore, take the mean of the two variable
df <- df %>% 
  mutate(fico_mean = (fico_range_high+fico_range_low)/2) %>% 
  select(-c(fico_range_high,fico_range_low))   

## 2.7 Rest of the chr to factor
df <- df %>% 
  mutate_if(is.character, as.factor)

df <- df %>%
  mutate(addr_state=as.character(addr_state)) # addr_state back to chr

## 2.8 Grade, emp_lemgth as ordered factor. This is for plotting purposes.
df$grade <- factor(df$grade, ordered = TRUE, levels = c("A","B","C","D","E","F","G"))
df$emp_length <- factor(df$emp_length, ordered = TRUE, levels = c("< 1 year","1 year",
                                                                  "2 years","3 years",
                                                                  "4 years","5 years",
                                                                  "6 years","7 years",
                                                                  "8 years","9 years","10+ years"))
# ===================================================================================================
##########################################################################
#           3. Further Cleaning Based on Descriptive Statistics          #
##########################################################################
# # 3.1 Initial descriptive statistics
# write_csv(describe(df, fast = FALSE), "./output/desc_before_clean.csv")
# write_csv(data.frame(names = names(df)), "./output/names.csv")

## 3.2 Cleaning strange extreme data
# according to the summary infos of variable "dti" and the density plot below...
# ggplot(df, aes(x=dti)) +
#   geom_density()
# As seen in the plot rendered, the plot is heavily skewed to the right
# In the real world, a debt-to-income (dti) ratio over 100 or less than 0 is rather unreasonable
# Therefore...
# nrow(filter(df, dti>100)) # Only 2746(0.1%) with more than 100%, dropped.
# nrow(filter(df, dti<0))   # Only 2(<0.0001%) as -1, dropped.

df <- df %>% 
  filter(dti<=100, dti>=0) 

# According to summary statistics,
# the maximum value 9999999/999 seems to be a special
# case but unexplained, remove these samples. There aren't much of maxed out samples anyway.
# tail(summary.factor(df$total_rev_hi_lim)) 
# tail(summary.factor(df$mo_sin_old_il_acct)) # This is different from "mths_since" variables. 
                                              # Never having an installment account is kind of unusual in this data
# tail(summary.factor(df$tot_hi_cred_lim)) 
# Plus, outliers of annual_inc(annual income) are removed
df <- df %>% 
  filter(total_rev_hi_lim < 9999999,
         mo_sin_old_il_acct < 999,
         tot_hi_cred_lim < 9999999)

df <- df %>% 
  filter(! annual_inc %in% boxplot.stats(df$annual_inc)$out)

###################################################################################
#           4. Create Sensitive Attribute tag(Privileged / Unprivileged)          #
###################################################################################

## 4.1 keep first 3-digits of zip_code (last 2-digits of zip_code are "xx" and not required)
df$zip_code <- str_sub(df$zip_code, 1, 3)

## 4.2 clean race(the sensitive attribute of the research) data,
##     create protected class variable tag.

# For the cleaning function, please refer to "clean_race.R"

# create the privileged / unprivileged dummy table
sensitive_df <- clean_race_3(path = "./data/race_combined/",
                             type = "matrix")


# # Create a year variable in df for matching with the race_df data
df$issue_year <- as.character(year(df$issue_d)-1)

melted_sensitive <- pivot_longer(sensitive_df,
                            cols = c(everything(), -ncol(sensitive_df)),
                            names_to = "issue_year",
                            values_to = "sensitive_attribute")

## 4.3 Add the sensitive attribute to the main df(LendingClub data)
# The zip codes from LendingClub data will be matched with the US census data
# then tags from different years will be assigned correspondingly.
df <- df %>%
  left_join(melted_sensitive, by = c("zip_code", "issue_year")) 

df <- df %>%
  drop_na(sensitive_attribute) 
# Some of the observations do not have a matching ZIP. 

df$sensitive_attribute <- as.logical(df$sensitive_attribute)


###################################################################################
#     5. Add Macroeconomic Variables (Based on ZIP codes of applications)         #
###################################################################################

## In this part, macroeconomic variables(MVs) will be merged to the LendingClub dataset.
## However, taxable income data was recorded by 5-digit ZIP code,
## GDP data was recorded by COUNTY code.
## Therefore, all of the MVs will be aggregated as average or sum of the 3-digit ZIP area.

## 5.1 Create the matching year of loan application with macroeconomic variables

## Every loan application should be assigned with macroeconomic variables one year before
## the application was made (because the MVs are generated by institutions by the end of
## the year). e.g. 2010 loan application should be assigned with 2009 MVs.

# df$macro_year <- as.character(as.numeric(df$issue_year) - 1)
df$macro_year <- df$issue_year # This is a workaround for the above mistake of issue_year

## 5.2 Merge Macroeconomic variables to the LendingClub data

# ## Taxable Income data from IRS(Internal Revenue Service)
# # These data are calculated as average of the ZIP area.
# irs <- taxable_income(path = "./data/irs_taxable_income/") 
# 
# melted_irs <- pivot_longer(irs,
#                            cols = c(everything(), -zip_code),
#                            names_to = "macro_year",
#                            values_to = "irs_taxable_income")
# 
# ## Household median income from United States Census Bureau (USCB)
# medianIncome <- census_median_income()
# 
# melted_median <- pivot_longer(medianIncome,
#                            cols = c(everything(), -zip_code),
#                            names_to = "macro_year",
#                            values_to = "median_income")

## Unemployment rate from USCB
unemployment_df <- census_unemployment()

melted_unemployment <- pivot_longer(unemployment_df,
                                    cols = c(everything(), -zip_code),
                                    names_to = "macro_year",
                                    values_to = "unemployment")

# unemployment_change <- unemployment[,c(1, ncol(unemployment), 2:(ncol(unemployment)-1))]
# unemployment_change <- changeGDP(unemployment_change)
# melted_unemployment_change <- pivot_longer(unemployment_change,
#                                     cols = c(everything(), -zip_code),
#                                     names_to = "macro_year",
#                                     values_to = "unemployment_change") 

## Uninsured rate from USCB
uninsured_df <- census_uninsured()

melted_uninsured <- pivot_longer(uninsured_df,
                                    cols = c(everything(), -zip_code),
                                    names_to = "macro_year",
                                    values_to = "uninsured")


# ## Generate County GDP to ZIP GDP data
# # It is okay to see the warnings here. Rows of NA data are removed after the line of code
# # generating the warnings() about NAs.
# allGDP <- summaryGDP(countyGDP_data = "./data/bea_gdp/CAGDP1__ALL_AREAS_2001_2020.csv",
#                      zcta_relationship = "./data/bea_gdp/tab20_zcta520_county20_natl.txt")
# 
# ## Real GDP data from BEA(Bureau of Economic Analysis)
# realGDP_df <- realGDP(allGDP)
# 
# melted_real <- pivot_longer(realGDP_df,
#                             cols = c(everything(), -zip_code),
#                             names_to = "macro_year",
#                             values_to = "real_gdp")
# 
# 
# ## Current GDP data from BEA(Bureau of Economic Analysis)
# currentGDP_df <- currentGDP(allGDP)
# 
# melted_current <- pivot_longer(currentGDP_df,
#                             cols = c(everything(), -zip_code),
#                             names_to = "macro_year",
#                             values_to = "current_gdp")
# 
# 
# ## Real GDP growth data from BEA(Bureau of Economic Analysis)
# growthGDP_df <- changeGDP(realGDP_df)
# 
# melted_growth <- pivot_longer(growthGDP_df,
#                                cols = c(everything(), -zip_code),
#                                names_to = "macro_year",
#                                values_to = "gdp_growth")
# 
# ## Real GDP Per Capita
# perCapita <- gdp_per_capita_zip(realGDP_df)
# 
# melted_perCapita <- pivot_longer(perCapita,
#                               cols = c(everything(), -zip_code),
#                               names_to = "macro_year",
#                               values_to = "real_gdp_per_capita")
# 
# ## Growth in Real GDP Per Capita
# growth_perCapita <- changeGDP(perCapita)
# 
# melted_perCapitaGrowth <- pivot_longer(growth_perCapita,
#                                  cols = c(everything(), -zip_code),
#                                  names_to = "macro_year",
#                                  values_to = "change_in_gdp_per_capita")
## Left join all of the above macroeconomic variables
# df <- df %>%
#   left_join(melted_irs, by = c("zip_code", "macro_year")) %>% 
#   left_join(melted_real, by = c("zip_code", "macro_year")) %>% 
#   left_join(melted_current, by = c("zip_code", "macro_year")) %>% 
#   left_join(melted_growth, by = c("zip_code", "macro_year")) %>% 
#   left_join(melted_median, by = c("zip_code", "macro_year")) %>% 
#   left_join(melted_unemployment, by = c("zip_code", "macro_year")) %>% 
#   left_join(melted_perCapita, by = c("zip_code", "macro_year")) %>% 
#   left_join(melted_perCapitaGrowth, by = c("zip_code", "macro_year")) %>% 
#   left_join(melted_uninsured, by = c("zip_code", "macro_year")) %>% 
#   left_join(melted_unemployment_change, by = c("zip_code", "macro_year"))

## 5.3 Merge Macroeconomic data to the LendingClub dateset 
df <- df %>%
  left_join(melted_uninsured, by = c("zip_code", "macro_year")) %>% 
  left_join(melted_unemployment, by = c("zip_code", "macro_year"))

## 5.4 Drop unmatched samples (due to the missing observations of macroeconomic variables)
df <- df %>%
  drop_na()


##############################################################################
#           6. Apply the reweighing algorithm (add sample weights)           #
##############################################################################

# For the algorithm function reweighing(), please refer to "reweighing_dplyr.R".
# df$good_bad <- ifelse(df$grade == "A"|df$grade == "B", "GOOD", "BAD")         # For the alternative Reweighing strategy, please ignore.

df <- df %>% 
  left_join(reweighing(df), by = c("grade", "sensitive_attribute"))

##############################################################################
#           7. Save the main df as RDS file for future applications          #
##############################################################################
## If proceeded lines of codes in 1.~6., 
## one may continue to 2_eda_graphing.R or 3_modeling.R with the 
## existing "df" created above without saving the RDS.

# write_csv(describe(df, fast = FALSE), "./output/desc_after_clean.csv")
# write_csv(data.frame(names = names(df)), "./output/names_after.csv")

# saveRDS(df, "./data/cleaned.rds")
# saveRDS(df, "./data/cleaned_with_mths_since.rds")
