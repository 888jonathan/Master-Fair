library("tidyverse")
library("lubridate")

#設定資料夾路徑
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
# print(getwd())

#list.files("./data")
df <- read_csv("./data/Loan_status_2007-2020Q3.gzip", col_names = TRUE,
               col_select = c("issue_d", "grade", "loan_status", "installment", "int_rate"))

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

# Average interest rate by grades

a1 <- filter(df, issue_d < "2018-01-01", issue_d > "2009-12-01") %>%
  group_by(grade) %>% 
  summarise(n = n(), avg_int=mean(int_rate)) %>% 
  mutate(grade_freq = n/ sum(n)) %>% 
  ungroup()

a2 <- filter(df, issue_d < "2018-01-01", issue_d > "2009-12-01", loan_status %in% ("Charged Off")) %>%
  group_by(grade) %>% 
  count(loan_status) %>% 
  ungroup() %>% 
  mutate(prob_default =n / a1["n"])

b1 <- filter(df, issue_d < "2017-10-01", issue_d > "2015-12-01") %>%
  group_by(grade) %>% 
  summarise(n = n(), avg_int=mean(int_rate)) %>% 
  mutate(grade_freq = n/ sum(n)) %>% 
  ungroup()

b2 <- filter(df, issue_d < "2017-10-01", issue_d > "2015-12-01", loan_status %in% ("Charged Off")) %>%
  group_by(grade) %>% 
  count(loan_status) %>% 
  ungroup() %>% 
  mutate(prob_default =n / a1["n"])

