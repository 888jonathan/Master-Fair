library("tidyverse")
library("lubridate")
library("mlr3")
library("haven")

#設定資料夾路徑
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
# print(getwd())

# Check total observations
df.a <- read_csv("./data/Loan_status_2007-2020Q3.gzip", col_names = TRUE, col_select = c("id"))
nrow(df.a)

# Check the name of variables
df.b <- read_csv("./data/Loan_status_2007-2020Q3.gzip", col_names = TRUE, n_max = 1)
names(df.b)

#list.files("./data")
df <- read_csv("./data/Loan_status_2007-2020Q3.gzip", col_names = TRUE,
               col_select = c("issue_d", "grade", "loan_status", "installment", "int_rate"), n_max = 100000)

df <- drop_na(df, issue_d)

df$grade <- factor(df$grade, ordered = TRUE, levels = c("A","B","C","D","E","F","G"))
df$issue_d <- my(df$issue_d)
df$int_rate <- parse_number(df$int_rate)/100

df <- df %>% 
  filter(issue_d < "2018-01-01", issue_d > "2015-12-01")

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
costs <- matrix(c(0,0.0089,0.0166,0.0241,0.0303,0.0365,0.0443, # This fits the paper
                 0.0333,0,0.0073,0.0144,0.0203,0.0262,0.0336,
                 0.0530,0.0211,0,0.0070,0.0128,0.0184,0.0256,
                 0.0684,0.0376,0.0172,0,0.0056,0.0112,0.0182,
                 0.0789,0.0489,0.0291,0.0123,0,0.0055,0.0124,
                 0.0851,0.0568,0.0380,0.0222,0.0106,0,0.0070,
                 0.0856,0.0608,0.0443,0.0304,0.0202,0.0109,0),
               nrow = 7)

dimnames(costs) <- list(Actual = c("A","B","C","D","E","F","G"), Prediction = c("A","B","C","D","E","F","G"))
costs_t <- t(costs) # This is for mlr3
# print(costs)

df.test <- read_csv("./data/Loan_status_2007-2020Q3.gzip", col_names = TRUE,
               col_select = c("grade","int_rate"), n_max = 100000)

df.test$grade <- factor(df.test$grade, ordered = TRUE, levels = c("A","B","C","D","E","F","G"))
df.test$int_rate <- parse_number(df.test$int_rate)/100
df.test <- drop_na(df.test, grade)

train_set = sample(task$nrow, 0.8 * task$nrow)
test_set = setdiff(seq_len(task$nrow), train_set)

task_lc = as_task_classif(df.test, target = "grade")
library("mlr3learners")
learner = lrn("classif.svm")
# learner$param_set # For hyperparameter tuning
# learner$param_set$values # For setting values
# additional argument can directly goes into lrn() as well
rr = resample(task_lc, learner, rsmp("cv"))

confusion = rr$prediction()$confusion
accuracy = sum(diag(confusion)) / sum(confusion)
avg_costs = sum(confusion * costs_t) / nrow(df.test)
print(avg_costs)

cost_measure = msr("classif.costs", costs = costs_t)
measures = msrs(c("classif.acc", "classif.costs", "classif.tnr"))
# https://mlr3.mlr-org.com/reference/mlr_measures.html
print(cost_measure)

rr$aggregate(cost_measure)
