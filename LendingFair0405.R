library("tidyverse")
library("lubridate")

#設定資料夾路徑
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
# print(getwd())

#list.files("./data")
df <- read_csv("./data/Loan_status_2007-2020Q3.gzip", col_names = TRUE,
               col_select = c("id", "issue_d", "grade", "loan_status"))

df$issue_d <- my(df$issue_d)

asdf <- filter(df, issue_d < "2018-01-01", issue_d > "2016-12-01",
               loan_status %in% c("Charged Off", "Default", "Fully Paid")) %>% 
  group_by(grade) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n/ sum(n))

#haha
# sthsth