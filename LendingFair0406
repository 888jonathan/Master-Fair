library("tidyverse")
library("lubridate")
library("mlr3")
library("haven")

#設定資料夾路徑
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
# print(getwd())

#list.files("./data")
df <- read_csv("./data/Loan_status_2007-2020Q3.gzip", col_names = TRUE,
               col_select = c("issue_d", "grade", "loan_status", "installment", "int_rate"), n_max = 10000)

df$grade <- factor(df$grade, ordered = TRUE, levels = c("A","B","C","D","E","F","G"))
df$issue_d <- my(df$issue_d)
df$int_rate <- parse_number(df$int_rate)

asdf <- filter(df, issue_d < "2018-01-01", issue_d > "2015-12-01",
               loan_status %in% c("Charged Off", "Fully Paid")) %>% 
  group_by(grade) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/ sum(n))

num <-asdf <- filter(df, issue_d < "2017-10-01", issue_d > "2015-12-01") %>% 
  group_by(issue_d) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/ sum(n))

## Calculate the overall data structute
# Average interest rate by grades

# a1 <- filter(df, issue_d < "2018-01-01", issue_d > "2009-12-01") %>%
#   group_by(grade) %>% 
#   summarise(n = n(), avg_int=mean(int_rate)) %>% 
#   mutate(grade_freq = n/ sum(n)) %>% 
#   ungroup()
# 
# a2 <- filter(df, issue_d < "2018-01-01", issue_d > "2009-12-01", loan_status %in% ("Charged Off")) %>%
#   group_by(grade) %>% 
#   count(loan_status) %>% 
#   ungroup() %>% 
#   mutate(prob_default =n / a1["n"])
# 
# b1 <- filter(df, issue_d < "2017-10-01", issue_d > "2015-12-01") %>%
#   group_by(grade) %>% 
#   summarise(n = n(), avg_int=mean(int_rate)) %>% 
#   mutate(grade_freq = n/ sum(n)) %>% 
#   ungroup()
# 
# b2 <- filter(df, issue_d < "2017-10-01", issue_d > "2015-12-01", loan_status %in% ("Charged Off")) %>%
#   group_by(grade) %>% 
#   count(loan_status) %>% 
#   ungroup() %>% 
#   mutate(prob_default =n / a1["n"])

# Cost-Sensitive with thresholding
# https://mlr3book.mlr-org.com/special-tasks.html?q=cost#cost-sens
## 1. Create Cost Matrix
costs <- matrix(c(0,0.0089,0.0166,0.0241,0.0303,0.0365,0.0443,
                 0.0333,0,0.0073,0.0144,0.0203,0.0262,0.0336,
                 0.0530,0.0211,0,0.0070,0.0128,0.0184,0.0256,
                 0.0684,0.0376,0.0172,0,0.0056,0.0112,0.0182,
                 0.0789,0.0489,0.0291,0.0123,0,0.0055,0.0124,
                 0.0851,0.0568,0.0380,0.0222,0.0106,0,0.0070,
                 0.0856,0.0608,0.0443,0.0304,0.0202,0.0109,0),
               nrow = 7)
dimnames(costs) <- list(Actual = c("A","B","C","D","E","F","G"), Prediction = c("A","B","C","D","E","F","G"))
# print(costs)

task_lc = as_task_classif(df, target = "grade")
