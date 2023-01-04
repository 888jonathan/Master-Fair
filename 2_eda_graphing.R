library("tidyverse")
library("ggthemes")
library("ggrepel")
library("ggpubr")
library("reshape2")
library("gt")
library("gtExtras")
library("gtsummary")
library("rstatix")
# source("./src/clean_race.R")
source("./src/clean_plot_data.R")
source("./src/reweighing_dplyr.R") # The reweighing algorithm. Loaded here for plotting


######### Fast forward the cleaning process :) ########
#######################################################

#設定資料夾路徑
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
# print(getwd())

# Load the cleaned dataset directly
# df <- readRDS("./data/cleaned.rds")
df <- readRDS("./data/cleaned_with_mths_since.rds") # This cleaned data has more observations
                                                    # because variables with "mths_since" are kept and imputed with (max value+1)
#######################################################

### Descriptive Statistics =============================
# write_csv(describe(df, fast = FALSE), "desc_after_clean.csv")

### EDA ================================================

###########################################################################
#          1.  Table of Grade Structures of LendingClub Dataset           #
###########################################################################

# Structure of credit grade of full cleaned dataset
grade_full <- df %>% 
  select(grade) %>%
  group_by(grade) %>% 
  summarise(n=n()) %>% 
  mutate(loan_proportion = paste0(round(n/sum(n)*100,2),"%"))

avg_int_full <- df %>% 
  select(grade, int_rate) %>% 
  group_by(grade) %>% 
  summarise(avg_int_rate=paste0(round(mean(int_rate),2), "%"))

grade_full %>%
  left_join(avg_int_full, by = "grade") %>% 
  gt() %>%
  gtsave("dist_full_grade_table.png", path = "./output/")

# Structure of credit grades with final outcome available
grade_counts <- df %>% 
  select(grade, loan_status) %>%
  filter(loan_status %in% c("Charged Off", "Fully Paid")) %>%
  group_by(grade) %>% 
  count()

percentage_of_default <- df %>% 
  select(grade, loan_status) %>% 
  filter(loan_status %in% c("Charged Off", "Fully Paid")) %>%
  group_by(grade) %>% 
  count(loan_status) %>% 
  filter(loan_status == "Charged Off") %>% 
  ungroup() %>% 
  mutate(prct = paste0(round((n/grade_counts$n)*100,2),"%"))

int_rate_bygrade <- df %>% 
  select(grade,loan_status, int_rate) %>% 
  filter(loan_status %in% c("Charged Off", "Fully Paid")) %>%
  group_by(grade) %>% 
  summarise(rate=paste0(round(mean(int_rate),2), "%"))

structure_table <- df %>% 
  select(grade, loan_status, int_rate) %>% 
  filter(loan_status %in% c("Charged Off", "Fully Paid")) %>% 
  group_by(grade) %>% 
  summarise(n=n()) %>% 
  mutate(loan_proportion=paste0(round(n/sum(n)*100,2),"%"),
         Charged_off=percentage_of_default$prct,
         avg_int_rate=int_rate_bygrade$rate) %>% 
  select(-c("n"))

structure_table %>%
  gt() %>%
  gtsave("dist_with_outcome.png", path = "./output/")
# ==================================================================


###################################
#          2.  Plotting           #
###################################

## 1. Where do applicants live?
app_state <- df %>% 
  select(addr_state) %>%
  group_by(addr_state) %>% 
  count()

app_state %>% 
ggplot(aes(x = reorder(addr_state, -n), y = n))+
  geom_bar(stat = "identity")+
  labs(x="State", y="Count")+
  theme(axis.text.x = element_text(angle=45,hjust=1,vjust=1.0, size = 8))

ggsave("./output/emp_grade.png", width = 8, height = 5)


## 2. For each grade, how much do applicants lend?
ggplot(df, aes(x = grade, y = loan_amnt, fill = grade)) +
  geom_boxplot()+
  scale_fill_grey(start = 0.5, end = 0.9)+
  theme_pubr()+
  theme(legend.position="none")
ggsave("./output/grade_loanamt.png", width = 8, height = 5)

# By Sensitive

# ggplot(df,aes(x = sensitive_attribute, y = loan_amnt, fill = sensitive_attribute)) +
#   geom_boxplot()+
#   stat_summary(fun=mean, colour="darkred", geom="point", 
#                shape=18, size=3, show.legend=FALSE) + 
#   scale_fill_brewer(palette="Pastel1")
df %>% 
  mutate(sensitive_attribute = ifelse(sensitive_attribute == FALSE, "Unprivileged", "Privileged")) %>%
  ggplot(aes(x = sensitive_attribute, y = loan_amnt, fill = sensitive_attribute)) +
  geom_boxplot()+
  scale_fill_grey(start = 0.5, end = 0.9)+
  theme_pubr()+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5, size = 30))+
  ggtitle("Loan Amount")

ggsave("./output/sensi_loanamt.png", width = 8, height = 6)

## 3. Grade VS annual_inc
ggplot(df, aes(x = grade, y = annual_inc, fill = grade)) +
  geom_boxplot()+
  scale_fill_grey(start = 0.5, end = 0.9)+
  theme_pubr()+
  theme(legend.position="none")
ggsave("./output/grade_inc.png", width = 8, height = 5)

# By Sensitive
df %>% 
  mutate(sensitive_attribute = ifelse(sensitive_attribute == FALSE, "Unprivileged", "Privileged")) %>%
  ggplot(aes(x = sensitive_attribute, y = annual_inc, fill = sensitive_attribute)) +
  geom_boxplot()+
  scale_fill_grey(start = 1, end = .6)+
  theme_pubr()
ggsave("./output/sensi_income.png", width = 8, height = 5)

## 4. Grade VS dti (debt-to-income ratio)
ggplot(df, aes(x = grade, y = dti, fill = grade)) +
  geom_boxplot()+
  scale_fill_grey(start = 0.5, end = 0.9)+
  theme_pubr()+
  theme(legend.position="none")

ggsave("./output/grade_dti.png", width = 8, height = 5)

# By Sensitive
df %>% 
  mutate(sensitive_attribute = ifelse(sensitive_attribute == FALSE, "Unprivileged", "Privileged")) %>%
  ggplot(aes(x = sensitive_attribute, y = dti, fill = sensitive_attribute)) +
  geom_boxplot()+
  scale_fill_tableau(palette = "Tableau 20", type = "regular")+
  theme_pubr()
ggsave("./output/sensi_dti.png", width = 8, height = 5)

## 5. Grade VS FICO
ggplot(df, aes(x = grade, y = fico_mean)) +
  geom_violin(aes(fill=grade, color=grade), alpha = 0.5)+
  geom_boxplot(width = 0.2)

ggsave("./output/grade_FICO.png")

ggplot(df, aes(x = grade, y = fico_mean, fill = grade)) +
  geom_boxplot()+
  scale_fill_grey(start = 0.5, end = 0.9)+
  theme_pubr()+
  theme(legend.position="none")

ggsave("./output/grade_FICO.png", width = 8, height = 5)

# By Sensitive
df %>% 
  mutate(sensitive_attribute = ifelse(sensitive_attribute == FALSE, "Unprivileged", "Privileged")) %>%
  ggplot(aes(x = sensitive_attribute, y = fico_mean, fill = sensitive_attribute)) +
  geom_boxplot()+
  scale_fill_brewer(palette = "Pastel2")+
  theme_pubr()
ggsave("./output/sensi_fico.png", width = 8, height = 5)
#==========================================================

## 6. How long have the applicants work?
# By grade

emp_grade <- df %>% 
  select(emp_length, grade, sensitive_attribute) %>% 
  group_by(grade, emp_length) %>% 
  summarise(n=n()) %>% 
  mutate(prop = n/sum(n))

ggplot(emp_grade, aes(x=emp_length, y = prop,  fill = emp_length))+
  geom_col()+
  facet_wrap(~grade)+
  labs(x = "Length of Employment", y = "Percentage", fill = "Length")+
  theme(axis.text.x = element_text(angle=45,hjust=1,vjust=1.0, size = 8),
        legend.text = element_text(size = 13),
        strip.text.x = element_text(size = 12),
        axis.title = element_text(size = 13),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey", linetype = "solid"),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = NA))+
  scale_fill_grey(start=0.8, end=0.2)+
  scale_y_continuous(labels = scales::percent_format())
 
ggsave("./output/grade_emp.png", width = 8, height = 4.5)

# By sensitive attribute
emp_sen <- df %>% 
  select(emp_length, grade, sensitive_attribute) %>% 
  group_by(sensitive_attribute, emp_length) %>% 
  summarise(n=n()) %>% 
  mutate(prop = n/sum(n))

ggplot(emp_sen, aes(x=emp_length, y = prop,  fill = emp_length))+
  geom_col()+
  facet_wrap(~sensitive_attribute,
             labeller = as_labeller(c("FALSE"="Unprivileged",
                                      "TRUE"="Privileged")))+
  labs(x = "Length of Employment", y = "Percentage", fill = "Length")+
  theme(axis.text.x = element_text(angle=45,hjust=1,vjust=1, size = 12),
        legend.text = element_text(size = 13),
        strip.text.x = element_text(size = 12),
        axis.title = element_text(size = 13),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey", linetype = "solid"),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = NA))+
  scale_fill_grey(start=0.8, end=0.2)+
  scale_y_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, .02)))
ggsave("./output/sensi_emp.png", width = 8, height = 4.5)


## 7. For each grade, which term do applicants choose?
term_grade <- df %>% 
  select(term, grade) %>% 
  group_by(grade, term) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n/sum(n))

ggplot(term_grade, aes(x=term, y = prop,  fill = term))+
  geom_col()+
  facet_wrap(~grade)+
  labs(x = "Term", y = "Percentage", fill = "Term")+
  theme(axis.text.x = element_text(angle=45,hjust=1,vjust=1.0, size = 8),
        legend.text = element_text(size = 13),
        strip.text.x = element_text(size = 12),
        axis.title = element_text(size = 13),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey", linetype = "solid"),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = NA))+
  scale_fill_grey(start=0.8, end=0.2)+
  scale_y_continuous(labels = scales::percent_format())
ggsave("./output/grade_term.png", width = 8, height = 4.5)

# By sensitive attribute
df %>% 
  select(term, sensitive_attribute) %>% 
  group_by(sensitive_attribute, term) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n/sum(n)) %>% 
ggplot(aes(x=term, y = prop,  fill = term))+
  geom_col()+
  facet_wrap(~sensitive_attribute,
             labeller = as_labeller(c("FALSE"="Unprivileged",
                                      "TRUE"="Privileged")))+
  labs(x = "Term", y = "Percentage", fill = "Term")+
  theme(axis.text.x = element_text(angle=45,hjust=1,vjust=1, size = 12),
        legend.text = element_text(size = 13),
        strip.text.x = element_text(size = 12),
        axis.title = element_text(size = 13),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey", linetype = "solid"),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = NA))+
  scale_fill_grey(start=0.8, end=0.2)+
  scale_y_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, .02)))

ggsave("./output/sensi_term.png", width = 8, height = 4.5)


## 8. Grade VS Home Ownership
home_grade <- df %>% 
  select(home_ownership, grade) %>% 
  group_by(grade, home_ownership) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n/sum(n))

ggplot(home_grade, aes(x=home_ownership, y = prop,  fill = home_ownership))+
  geom_col()+
  facet_wrap(~grade)+
  labs(x = "Home Ownership", y = "Percentage", fill = "Home Ownership")+
  theme(axis.text.x = element_text(angle=45,hjust=1,vjust=1.0, size = 8),
        legend.text = element_text(size = 13),
        strip.text.x = element_text(size = 12),
        axis.title = element_text(size = 13),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey", linetype = "solid"),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = NA),
        legend.position="right")+
  scale_fill_grey()+
  scale_y_continuous(labels = scales::percent_format())
ggsave("./output/grade_home.png", width = 8, height = 5)

# By sensitive
df %>% 
  select(home_ownership, sensitive_attribute) %>% 
  group_by(sensitive_attribute, home_ownership) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n/sum(n)) %>% 
  
  ggplot(aes(x=home_ownership, y = prop,  fill = home_ownership))+
  geom_col()+
  facet_wrap(~sensitive_attribute,
             labeller = as_labeller(c("FALSE"="Unprivileged",
                                      "TRUE"="Privileged")))+
  labs(x = "Home Ownership", y = "Percentage", fill = "Home Ownership")+
  theme(axis.text.x = element_text(angle=45,hjust=1,vjust=1, size = 12),
        legend.text = element_text(size = 13),
        strip.text.x = element_text(size = 12),
        axis.title = element_text(size = 13),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey", linetype = "solid"),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = NA))+
  scale_fill_grey(start=0.8, end=0.2)+
  scale_y_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, .02)))
ggsave("./output/sensi_home.png", width = 8, height = 5)

## 9. Grade VS Purpose
purpose_grade <- df %>% 
  select(purpose, grade) %>% 
  group_by(grade, purpose) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n/sum(n))

ggplot(purpose_grade, aes(x=purpose, y = prop,  fill = purpose))+
  geom_col()+
  facet_wrap(~grade)+
  labs(x = "Purpose of Loan", y = "Percentage", fill = "Purpose")+
  theme(axis.text.x = element_text(angle=45,hjust=1,vjust=1.0, size = 8),
        legend.text = element_text(size = 13),
        strip.text.x = element_text(size = 12),
        axis.title = element_text(size = 13),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey", linetype = "solid"),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = NA))+
  scale_fill_grey()+
  scale_y_continuous(labels = scales::percent_format())
ggsave("./output/grade_purpose.png", width = 8, height = 5)

# By sensitive
df %>% 
  select(sensitive_attribute, purpose) %>% 
  group_by(sensitive_attribute, purpose) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n/sum(n)) %>% 
  
  ggplot(aes(x=purpose, y = prop,  fill = purpose))+
  geom_col()+
  facet_wrap(~sensitive_attribute,
             labeller = as_labeller(c("FALSE"="Unprivileged",
                                      "TRUE"="Privileged")))+
  labs(x = "Purpose", y = "Percentage", fill = "Purpose")+
  theme(axis.text.x = element_text(angle=45,hjust=1,vjust=1, size = 12),
        legend.text = element_text(size = 13),
        strip.text.x = element_text(size = 12),
        axis.title = element_text(size = 13),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey", linetype = "solid"),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = NA))+
  scale_fill_grey(start=0.8, end=0.2)+
  scale_y_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, .02)))
ggsave("./output/sensi_purpose.png", width = 8, height = 5)

## 10. Grade VS Application Type
apptype_grade <- df %>% 
  select(application_type, grade) %>% 
  group_by(grade, application_type) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n/sum(n))

ggplot(apptype_grade, aes(x=application_type, y = prop,  fill = application_type))+
  geom_col()+
  facet_wrap(~grade)+
  labs(x = "Application Type", y = "Percentage", fill = "Type")+
  theme(axis.text.x = element_text(angle=45,hjust=1,vjust=1.0, size = 8),
        legend.text = element_text(size = 13),
        strip.text.x = element_text(size = 12),
        axis.title = element_text(size = 13),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey", linetype = "solid"),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = NA))+
  scale_fill_grey()+
  scale_y_continuous(labels = scales::percent_format())
ggsave("./output/grade_app.png", width = 8, height = 5)

# By sensitive
df %>% 
  select(sensitive_attribute, application_type) %>% 
  group_by(sensitive_attribute, application_type) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n/sum(n)) %>% 
  
  ggplot(aes(x=application_type, y = prop,  fill = application_type))+
  geom_col()+
  facet_wrap(~sensitive_attribute,
             labeller = as_labeller(c("FALSE"="Unprivileged",
                                      "TRUE"="Privileged")))+
  labs(x = "Application Type", y = "Percentage", fill = "Application Type")+
  theme(axis.text.x = element_text(angle=45,hjust=1,vjust=1, size = 12),
        legend.text = element_text(size = 13),
        strip.text.x = element_text(size = 12),
        axis.title = element_text(size = 13),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey", linetype = "solid"),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = NA))+
  scale_fill_grey(start=0.8, end=0.2)+
  scale_y_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, .02)))
ggsave("./output/sensi_app.png", width = 8, height = 5)
# ==============================================================================

## 11. Grade VS Uninsured
ggplot(df, aes(x = uninsured, color = grade, linetype=grade))+
  geom_density(adjust = 2)+
  scale_color_tableau()+
  theme_pubr()
ggsave("./output/grade_uninsured.png", width = 8, height = 4.5)  

# By sensitive
df %>% 
  mutate(sensitive_attribute = ifelse(sensitive_attribute == FALSE, "Unprivileged", "Privileged")) %>%
  ggplot(aes(x = uninsured, color = sensitive_attribute, linetype = sensitive_attribute))+
  geom_density(adjust = 2)+
  scale_color_tableau()+
  theme_pubr()
ggsave("./output/sensi_uninsured.png", width = 8, height = 5)  

## 12. Grade VS Unemployment
ggplot(df, aes(x = unemployment, color = grade, linetype=grade))+
  geom_density(adjust = 2)+
  scale_color_tableau()+
  theme_pubr()
ggsave("./output/grade_unemployment.png", width = 8, height = 4.5)  

# By sensitive
df %>% 
  mutate(sensitive_attribute = ifelse(sensitive_attribute == FALSE, "Unprivileged", "Privileged")) %>%
  ggplot(aes(x = unemployment, linetype = sensitive_attribute))+
  geom_density(aes(color = sensitive_attribute),adjust = 2)+
  scale_color_tableau()+
  theme_pubr()
ggsave("./output/sensi_unemployment.png", width = 8, height = 5) 

# ===================================================================
## For rest of the variables
# Heated Corrmap (Based on Spearman Correlation) 

get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

melted_cormat <- df %>% 
  select(where(is.numeric), emp_length, grade) %>% # Note: The higher the grade, the smaller the number.
  mutate(grade = as.integer(grade), emp_length = as.integer(emp_length)) %>% 
  cor(method = "spearman") %>% # Spearman Correlation
  get_lower_tri() %>% 
  round(2) %>% 
  melt(na.rm = TRUE) 


ggplot(melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_raster() +
  scale_fill_gradient2(low = "blue", mid='white', high = "red", midpoint = 0,
                       limit = c(-1, 1), name = "Spearman\nCorrelation")+
  labs(x='', y='')+
  theme(axis.text.x = element_text(angle=-45,hjust=0.05,vjust=0.2, size = 7),
        axis.text.y = element_text(size = 8))+
  coord_fixed()+
  scale_x_discrete(limits = rev(levels(melted_cormat$Var2)))

ggsave("./output/corr_heatmap.png", width = 12, height = 12, units = "in", dpi = 320)

# melted_cormat %>% filter(Var1 == "grade", abs(value) >=.10)
# 
# df %>% 
#   select(revol_util, total_rev_hi_lim, bc_open_to_buy, bc_util, percent_bc_gt_75,
#          total_bc_limit, fico_mean, grade) %>% 
#   mutate(grade = as.integer(grade)) %>%
#   cor()
# ======================================================================

#######################################
#          3.  Map Plotting           #
#######################################

## US map plots by state
# LC data state acromnym to full name (lower)
# state_name <- read_csv("./data/state_name.csv")
# state_name[28,2] <- "NE"
# state_name$full <- tolower(state_name$full)
# df <- df %>% 
#   left_join(state_name, by=c("addr_state"="short"))
# 
# state_data <- df %>%
#   select(annual_inc, int_rate, fico_mean, sensitive_attribute, full) %>% 
#   group_by(full) %>% 
#   summarise(avg_income=mean(annual_inc), avg_int=mean(int_rate),
#             avg_fico=mean(fico_mean), avg_sensitive=mean(sensitive_attribute))
# 
# usa <- map_data("state") %>%
#   left_join(state_data, by=c("region"="full"))
# 
# mid_points <- usa %>% 
#   select(long, lat, group, region) %>% 
#   left_join(state_name, by=c("region"="full")) %>% 
#   group_by(region, short) %>% 
#   summarise(long=median(range(long)), lat=median(range(lat)))
# 
# ggplot()+
#   geom_polygon(data=usa, aes(x=long, y=lat, group=group, fill = avg_sensitive),
#                color="white")+
#   geom_text(data=mid_points, aes(label = short, x = long, y = lat)) + #add labels at centroids
#   scale_fill_gradient(low="white", high="grey39")

# 1. Prepare state border lines for all county map
state_border <- map_data("state")

state_name <- read_csv("./data/long_lat.csv")

state_name <- state_name %>% 
  select(5:7) %>% 
  drop_na() %>% 
  filter(!usa_state_code %in% c("AK", "HI", "PR"))


# 2.1 Prepare data for county race proportion map
race_percentage <- race_county()
county <- map_data("county") %>%
  mutate(subregion=ifelse(subregion=="yellowstone national", "yellowstone", subregion)) %>% 
  left_join(race_percentage, by = c("region","subregion"))

# 2.2 Plot African American
ggplot()+
  geom_polygon(data=county, aes(x=long, y=lat, group=group, fill = Black), size = 0.1, color="white")+
  geom_polygon(data=state_border, aes(x=long, y=lat, group=group), size = 0.1, fill = NA, color = "black")+
  geom_text(data=state_name, aes(label = usa_state_code,
                                 x = usa_state_longitude,
                                 y = usa_state_latitude)) + #add labels at centroids
  scale_fill_gradient(low="white", high="black", limits = c(0,100))+
  labs(x="", y="", fill = "Percent\nAfrican\nAmarican(%)")+
  theme_map()
ggsave("./output/map_black.png", width = 8, height = 4.5)

# 2.3 Plot Indigenous Americans
ggplot()+
  geom_polygon(data=county, aes(x=long, y=lat, group=group, fill = Indian), size = 0.1, color="white")+
  geom_polygon(data=state_border, aes(x=long, y=lat, group=group), size = 0.1, fill = NA, color = "black")+
  geom_text(data=state_name, aes(label = usa_state_code,
                                 x = usa_state_longitude,
                                 y = usa_state_latitude)) + #add labels at centroids
  scale_fill_gradient(low="white", high="black", limits = c(0,100))+
  labs(x="", y="", fill = "Percent\nIndigenous\nAmerican(%)")+
  theme_map()
ggsave("./output/map_Indian.png", width = 8, height = 4.5)

# 2.4 Plot Latino
ggplot()+
  geom_polygon(data=county, aes(x=long, y=lat, group=group, fill = Latino), size = 0.1, color="white")+
  geom_polygon(data=state_border, aes(x=long, y=lat, group=group), size = 0.1, fill = NA, color = "black")+
  geom_text(data=state_name, aes(label = usa_state_code,
                                 x = usa_state_longitude,
                                 y = usa_state_latitude)) + #add labels at centroids
  scale_fill_gradient(low="white", high="black", limits = c(0,100))+
  labs(x="", y="", fill = "Percent\nLatino(%)")+
  theme_map()

ggsave("./output/map_latino.png", width = 8, height = 4.5)
# =============================================================================

# 3. Plot poverty rate map

poverty_percentage <- poverty_county()

county_poverty <- map_data("county") %>%
  mutate(subregion=ifelse(subregion=="yellowstone national", "yellowstone", subregion)) %>% 
  left_join(poverty_percentage, by = c("region","subregion"))

ggplot()+
  geom_polygon(data=county_poverty, aes(x=long, y=lat, group=group, fill = povertyRate), size = 0.1, color="white")+
  geom_polygon(data=state_border, aes(x=long, y=lat, group=group), size = 0.1, fill = NA, color = "Black")+
  # geom_text(data=state_name, aes(label = usa_state_code,
  #                                x = usa_state_longitude,
  #                                y = usa_state_latitude)) + #add labels at centroids
  scale_fill_binned(low = "white", high = "black",
                    breaks = c(10,15,20,25,30),
                    limits = c(5,
                               35),
                    oob = scales::squish)+
  labs(x="", y="", fill = "Percent in\nPoverty(%)")+
  theme_map()
  # theme(
  #   #panel.background = element_rect(fill='transparent'),
  #   plot.background = element_rect(fill='transparent', color=NA),
  #   panel.grid.major = element_blank(),
  #   panel.grid.minor = element_blank(),
  #   axis.title.x=element_blank(),
  #   axis.text.x=element_blank(),
  #   axis.ticks.x=element_blank(),
  #   axis.title.y=element_blank(),
  #   axis.text.y=element_blank(),
  #   axis.ticks.y=element_blank()
  # )
ggsave("./output/map_poverty.png", width = 8, height = 4.5)


# 4. Plot the median household income by counties
median_income <- income_county()

county_income <- map_data("county") %>%
  mutate(subregion=ifelse(subregion=="yellowstone national", "yellowstone", subregion)) %>% 
  left_join(median_income, by = c("region","subregion"))

ggplot()+
  geom_polygon(data=county_income, aes(x=long, y=lat, group=group, fill = medianIncome), size = 0.1, color="white")+
  geom_polygon(data=state_border, aes(x=long, y=lat, group=group), size = 0.1, fill = NA, color = "black")+
  scale_fill_steps(low = "white", high = "black",
                    breaks = c(40000, 45000, 50000, 55000, 60000),
                    limits = c(35000,
                               quantile(median_income$medianIncome, 0.75)+1.5*IQR(median_income$medianIncome)),
                    oob = scales::squish)+
  labs(x="", y="", fill = "Median\nHousehold\nIncome")+
  theme_map()

ggsave("./output/map_median_income.png", width = 8, height = 4.5)

# 5. Plot the uninsured by counties
uninsured_plot <- uninsured_county()

county_uninsured <- map_data("county") %>%
  mutate(subregion=ifelse(subregion=="yellowstone national", "yellowstone", subregion)) %>% 
  left_join(uninsured_plot, by = c("region","subregion"))

ggplot()+
  geom_polygon(data=county_uninsured, aes(x=long, y=lat, group=group, fill = unInsured), size = 0.1, color="white")+
  geom_polygon(data=state_border, aes(x=long, y=lat, group=group), size = 0.1, fill = NA, color = "black")+
  scale_fill_steps(low = "white", high = "black",
                   breaks = c(6, 8, 10, 12),
                   limits = c(4,
                              median(uninsured_plot$unInsured)+1.5*IQR(uninsured_plot$unInsured)),
                   oob = scales::squish)+
  labs(x="", y="", fill = "Percent\nUninsured(%)")+
  theme_map()

ggsave("./output/map_uninsured.png", width = 8, height = 4.5)

# 6. Plot the unemployment by counties
unemployment_plot <- unemployment_county()

county_unemployment <- map_data("county") %>%
  mutate(subregion=ifelse(subregion=="yellowstone national", "yellowstone", subregion)) %>% 
  left_join(unemployment_plot, by = c("region","subregion"))

ggplot()+
  geom_polygon(data=county_unemployment, aes(x=long, y=lat, group=group, fill = unEmployment), size = 0.1, color="white")+
  geom_polygon(data=state_border, aes(x=long, y=lat, group=group), size = 0.1, fill = NA, color = "black")+
  scale_fill_steps(low = "white", high = "black",
                   breaks = c(4, 5, 6),
                   limits = c(3,
                              median(unemployment_plot$unEmployment)+1.5*IQR(unemployment_plot$unEmployment)),
                   oob = scales::squish)+
  labs(x="", y="", fill = "Percent\nUnemployed(%)")+
  theme_map()

ggsave("./output/map_unemployed.png", width = 8, height = 4.5)
# =========================================================================

###################################################
#          4.  Unfairness Visualization           #
###################################################
## Based on the Priv/ Unpriv groups...

# 1. Calculate the grade proportion of Priv/Unpriv
unfair_plot <- df %>% 
  select(grade, sensitive_attribute) %>% 
  group_by(sensitive_attribute, grade) %>% 
  summarise(n=n()) %>% 
  mutate(freq=n/sum(n)) %>% 
  mutate(sensitive_attribute = ifelse(sensitive_attribute == FALSE, "Unprivileged", "Privileged"))

# 2. Simple Unfairness Calculation
sum(abs(unfair_plot[1:7,4]-unfair_plot[8:14,4]))
sum(abs(unfair_plot[1:2,4]-unfair_plot[8:9,4]))
# max(abs(unfair_plot[1:7,4]-unfair_plot[8:14,4]))

# 3. Plot the unfairness
unfair_plot %>%
  gt() %>% 
  fmt_number(
    columns = freq,
    decimals = 3,
    use_seps = FALSE
  ) %>% 
  gtsave("dist_Target_variable.png", path = "./output/")

ggplot(unfair_plot, aes(x=grade,y=freq, fill=grade))+
  geom_bar(position="dodge", stat="identity")+
  geom_hline(yintercept = as.numeric(unfair_plot[8,4]), linetype = "dashed")+
  geom_hline(yintercept = as.numeric(unfair_plot[9,4]), linetype = "dashed")+
  facet_wrap(~fct_rev(sensitive_attribute))+
  labs(x="Grades", y="Percentage")+
  scale_fill_grey()+
  scale_y_continuous(labels = scales::percent)+
  theme(strip.text.x = element_text(size = 16))

ggsave("./output/dist_unfair.png", width = 8, height = 6)
# ======================================================================

######################################################
#          5.  Reweighing Results Plotting           #
######################################################

reweighing(df) %>%
  gt(groupname_col = "sensitive_attribute") %>% 
  gtsave("reweighing_instance_weights.png", path = "./output/")

reweighing(df) %>% 
  mutate(sensitive_attribute = ifelse(sensitive_attribute == FALSE, "Unprivileged", "Privileged")) %>% 
  ggplot(aes(x = grade, y = weights, group = sensitive_attribute, label = round(weights,3))) +
  geom_point(aes(shape = sensitive_attribute), size = 4) +
  geom_line()+
  geom_label_repel()+
  geom_hline(yintercept = 1, linetype = "dashed")

ggsave("./output/reweighing_weights_plot.png", width = 8, height = 5)
#===========================================================

##############################################
#             6.  Hypothesis Testing         #
##############################################

## Create summary table with hypothesis tests to test the significance between Privileged / Unprivileged samples
## Continuous data (numericals): Wilcoxon Rank Sum p-Value and Rosenthal correlation(Z/sqrt(n)) for Effect Size 
## t-test was ignored because the non-normal distribution of variables
## Discrete data (factors): Chi-squared test for p-Value and Cramer's V for effect size
## Ordered data (ordered): Kruskal Wallis is performed, along with Eta-squared for effect size

# Define functions for gtsummary that calculates the Effect Size  
wil_ES <- function(data, variable, by, ...) {
    
    rstatix::wilcox_effsize(data, as.formula(glue::glue("{variable} ~ {by}")))$effsize
}
# wil_ES(df, "loan_amnt", "sensitive_attribute")

wil_ES_ordered <- function(data, variable, by, ...) { # This is a workaround of wilcoxon effect size for ordered variables
  tmp <- data[,c(variable, by)]
  tmp[variable] <- as.numeric(tmp[[variable]])
  rstatix::wilcox_effsize(tmp, as.formula(glue::glue("{variable} ~ {by}")))$effsize
}

# wil_ES_ordered(df, "emp_length", "sensitive_attribute")

chi_ES <- function(data, variable, by, ...){
  
  rstatix::cramer_v(data[[variable]], data[[by]], correct = FALSE)
}

kru_ES <- function(data, variable, by, ...) {
  rstatix::kruskal_effsize(data, as.formula(glue::glue("{variable} ~ {by}")))$effsize
}
# kru_ES(df, "grade", "sensitive_attribute")

# Create a table with p-value and effect size
tbl <- df %>%
  select(-c(issue_d, loan_status, zip_code, addr_state, earliest_cr_line,
            issue_year, macro_year, grade, int_rate, weights)) %>%
  mutate(sensitive_attribute = ifelse(sensitive_attribute == FALSE, "Unprivileged", "Privileged")) %>% 
  tbl_summary(
    by = sensitive_attribute, 
    type = list(acc_now_delinq ~ "continuous", inq_last_6mths ~ "continuous")
  ) %>%
  add_n() %>% 
  add_overall() %>% 
  add_p(list(where(is.numeric) ~ "wilcox.test",
             where(is.factor) ~ "chisq.test.no.correct",
             where(is.ordered) ~ "wilcox.test")) %>% 
  add_stat(fns = list(where(is.numeric) ~ wil_ES,
                      where(is.factor) ~ chi_ES,
                      where(is.ordered) ~ wil_ES_ordered)) %>%
  modify_header(label ~ "**Variable**") %>%
  modify_header(add_stat_1 ~ "**Effect Size**") %>% 
  modify_footnote(add_stat_1 ~"Rosenthal Correlation for Wilcoxon, Cramer's V for Chi-squared")

tbl

tbl2 <- df %>%
  select(emp_length, sensitive_attribute) %>%
  mutate(sensitive_attribute = ifelse(sensitive_attribute == FALSE, "Unprivileged", "Privileged")) %>%
  tbl_summary(
    by = sensitive_attribute,

  ) %>%
  add_n() %>%
  add_overall() %>%
  add_p(list(where(is.numeric) ~ "wilcox.test",
             where(is.factor) ~ "chisq.test.no.correct",
             where(is.ordered) ~ "wilcox.test")) %>%
  add_stat(fns = list(where(is.numeric) ~ wil_ES,
                      where(is.factor) ~ chi_ES,
                      where(is.ordered) ~ wil_ES_ordered)) %>%
  modify_header(label ~ "**Variable**") %>%
  modify_header(add_stat_1 ~ "**Effect Size**") %>%
  modify_footnote(add_stat_1 ~"Rosenthal Correlation for Wilcoxon, Cramer's V for Chi-squared, Eta-squared for Kruskal Wallis")

tbl2

# Optional, save the table as docx
# tbl %>% 
#   as_flex_table() %>% 
#   flextable::save_as_docx(path = "./output/file.docx")

# ===================
###################################################################
#             Additional.  Plot Sensitive VSs in one plot         #
###################################################################
library(patchwork)
df %>% 
  mutate(sensitive_attribute = ifelse(sensitive_attribute == FALSE, "Unprivileged", "Privileged")) %>%
  ggplot(aes(x = sensitive_attribute, y = loan_amnt, fill = sensitive_attribute)) +
  geom_boxplot()+
  scale_fill_grey(start = 1, end = .6)+
  theme_pubr()+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5, size = 25),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
  ggtitle("Loan Amount")+
  
  df %>% 
  mutate(sensitive_attribute = ifelse(sensitive_attribute == FALSE, "Unprivileged", "Privileged")) %>%
  ggplot(aes(x = sensitive_attribute, y = annual_inc, fill = sensitive_attribute)) +
  geom_boxplot()+
  scale_fill_grey(start = 1, end = .6)+
  theme_pubr()+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5, size = 25),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("Annual Income")+
  
  df %>% 
  mutate(sensitive_attribute = ifelse(sensitive_attribute == FALSE, "Unprivileged", "Privileged")) %>%
  ggplot(aes(x = sensitive_attribute, y = dti, fill = sensitive_attribute)) +
  geom_boxplot()+
  scale_fill_grey(start = 1, end = .6)+
  theme_pubr()+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5, size = 25))+
  ggtitle("DTI Ratio")+
  
  df %>% 
  mutate(sensitive_attribute = ifelse(sensitive_attribute == FALSE, "Unprivileged", "Privileged")) %>%
  ggplot(aes(x = sensitive_attribute, y = fico_mean, fill = sensitive_attribute)) +
  geom_boxplot()+
  scale_fill_grey(start = 1, end = .6)+
  theme_pubr()+
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5, size = 25))+
  ggtitle("FICO Score")

ggsave("./output/sensi_combined.png", width = 8, height = 7)
