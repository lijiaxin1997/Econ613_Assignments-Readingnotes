rm(list = ls())
library(tidyverse)
library(ggplot2)
library(scales)
library(sampleSelection)
library(AER)
library(plm)
library(panelr)
library(xtable)
setwd("~/Desktop/22Spring/Econ613/Assignments/A4/Data")
#==============================================================
## Exercise 1 Preparing the Data
#==============================================================
# 1.1 & 1.2 
adjusted_data <- 
  read_csv("dat_A4.csv", col_types = cols(.default = "d")) %>% 
  rename(gender = KEY_SEX_1997,
         marital_status = CV_MARSTAT_COLLAPSED_2019,
         children_number = CV_BIO_CHILD_HH_U18_2019,
         income = YINC_1700_2019,
         race = KEY_RACE_ETHNICITY_1997) %>% 
  mutate(gender = factor(gender, labels = c("male", "female")),
         marital_status = factor(marital_status, labels = c("never", "married", "separated", "divorced", "widowed")),
         race = factor(race, labels = c("black", "hispanic", "mixed", "non")))
data <- 
  adjusted_data %>% 
  mutate(across(starts_with("CV_HGC_BIO"), function(x) ifelse(x == 95, NA, x)),
         age = 2019 - KEY_BDATE_Y_1997,
         work_exp = rowSums(across(starts_with("CV_WKSWK_JOB"), function(x) x/52), na.rm = TRUE),
         self_edu_year = case_when(YSCH.3113_2019 == 1 ~ 0,
                                   YSCH.3113_2019 == 2 ~ 12,
                                   YSCH.3113_2019 == 3 ~ 12,
                                   YSCH.3113_2019 == 4 ~ 14,
                                   YSCH.3113_2019 == 5 ~ 16,
                                   YSCH.3113_2019 == 6 ~ 18,
                                   YSCH.3113_2019 == 7 ~ 23,
                                   YSCH.3113_2019 == 8 ~ 19,
                                   YSCH.3113_2019 <= 0 ~ NA_real_),
         parent_edu_year = rowSums(across(starts_with("CV_HGC"))),
         all_edu_year = parent_edu_year + self_edu_year)
# 1.3
# Plot the income data (where income is positive) by i) age groups, ii) gender groups and iii) number of children
data_positive_income <- data %>%
  filter(income > 0) 
data_positive_income %>%
  ggplot(aes(x = age, y = income, group = age)) +
  geom_boxplot() +
  scale_y_continuous(labels = label_comma()) 
data_positive_income %>%
  ggplot(aes(x = gender, y = income, group = gender)) +
  geom_boxplot() +
  scale_y_continuous(labels = label_comma()) 
data_positive_income %>%
  filter(! is.na(children_number)) %>% 
  mutate(children_number = as.factor(children_number)) %>% 
  ggplot(aes(x = children_number, y = income, group = children_number)) +
  geom_boxplot() +
  scale_y_continuous(labels = label_comma())+
  scale_x_discrete()
# Table the share of ”0” in the income data by i) age groups, ii) gender groups, iii) number of children and marital status
data_income <- data %>%
  mutate(income_zero = income == 0 & !is.na(income),
         income_na = is.na(income),
         income_positive = !income_zero & !income_na)
data_income %>% 
  group_by(age) %>% 
  summarise(across(c(income_zero, income_na), .fns =  mean, na.rm = TRUE))
data_income %>%
  group_by(gender) %>% 
  summarise(across(c(income_zero, income_na), .fns =  mean, na.rm = TRUE))
#show <- data_share %>%
#  group_by(children_number, marital_status) %>% 
#  summarise(across(c(income_zero, income_na), .fns =  mean, na.rm = TRUE))

#==============================================================
## Exercise 2 Heckman Selection Model
#==============================================================
# 2.1
data_lm <- data_positive_income %>% 
  mutate(married = ifelse(marital_status == "married", 1, 0),
         separated = ifelse(marital_status == "separated", 1, 0),
         divorced = ifelse(marital_status == "divorced", 1, 0),
         widowed = ifelse(marital_status == "widowed", 1, 0),
         black = ifelse(race == "black", 1, 0),
         hispanic = ifelse(race == "hispanic", 1, 0),
         mixed = ifelse(race == "mixed", 1, 0)) %>% 
  select(income, age, gender, children_number, self_edu_year, parent_edu_year, 
         work_exp, married, separated, divorced, widowed, black, hispanic, mixed)%>% 
  drop_na()
lm <- lm(income ~ age + gender + children_number + self_edu_year + parent_edu_year + 
         work_exp + married + separated + divorced + widowed + black + hispanic + mixed, data_lm)
summary(lm)
# using variables created before
lm_created <- lm(income ~ age + gender + children_number + self_edu_year + parent_edu_year + 
                  work_exp + married, data_lm)
summary(lm_created)
# interpretation: 
# 2.2

# 2.3
# stage 1
data_selection <- data_income %>% 
  mutate(income = ifelse(is.na(income), 0, income)) %>% 
  mutate(married = ifelse(marital_status == "married", 1, 0)) %>% 
  select(income, income_positive, age, gender, children_number, self_edu_year, parent_edu_year, 
         work_exp, married) %>% 
  drop_na()
selection_glm <- glm(income_positive ~ age + gender + children_number + self_edu_year + parent_edu_year +
                     work_exp + married, binomial(link = "probit"), data_selection)
summary(selection_glm)
# stage 2
sel_probit_pred <- data_selection %>%
  mutate(probit_pred = predict.glm(selection_glm, data_selection),
         ratio = dnorm(probit_pred) / pnorm(probit_pred))
sel_probit_pred_miss <- sel_probit_pred %>% 
  mutate(income = ifelse(income == 0, NA_real_, income))
lm2 <- lm(income ~ age + gender + children_number + self_edu_year + parent_edu_year +
          work_exp + married + ratio, 
          sel_probit_pred_miss)
summary(lm2)
# calculate st.d
dat <- data.matrix(sel_probit_pred %>% mutate("(Intercept)" = 1) %>% relocate("(Intercept)"))
positive <- dat[,"income"] > 0
dat[, "gender"] = dat[, "gender"] - 1
errors <- lm2$residuals
probit_pred <- dat[, "probit_pred"]
ratio <- dat[, "ratio"]
ratio_coef <- lm2$coefficients["ratio"]
delta_i <- ratio * (ratio + probit_pred)
delta_i = delta_i[positive]
variance_hat <- ((errors %*% errors) / length(errors) +  (ratio_coef^2) * sum(delta_i, na.rm = TRUE) / length(errors))[1,1]
s1 <- dat[positive, c("(Intercept)", "age", "gender", "children_number", "self_edu_year", 
                              "parent_edu_year", "work_exp", "married", "ratio")]
s2 <- dat[positive, c("(Intercept)", "age", "gender", "children_number", "self_edu_year", 
                                "parent_edu_year", "work_exp", "married")]
q <- (ratio_coef / sqrt(variance_hat))
delta_matrix <- delta_i * diag(length(delta_i))
V <- vcov(selection_glm)
V <- V
Q_matrix <- q^2 * (t(s1) %*% delta_matrix %*% s2) %*% V %*% (t(s2) %*% delta_matrix %*% s1)
adjusted_var <-  variance_hat * solve(t(s1) %*% s1) %*% 
  (t(s1) %*% (diag(length(delta_i)) - q^2 * delta_matrix) %*% s1 + Q_matrix) %*% 
  solve(t(s1) %*% s1)
adjusted_var <- sqrt(diag(adjusted_var))

heckman_heckit = heckit(income_positive ~ age + gender + children_number + self_edu_year + parent_edu_year + work_exp + married,
                        income ~ age + gender + children_number + self_edu_year + parent_edu_year + work_exp + married,
                        data = sel_probit_pred_miss)

first_stage <- cbind(summary(heckman_heckit)$estimate[1:length(selection_glm$coefficients), 1:2], summary(selection_glm)$coefficients[, 1:2])
colnames(first_stage) = c("heckit : est", "heckit :se", "without package : est", "without package :se")
second_stage <- cbind(summary(heckman_heckit)$estimate[length(selection_glm$coefficients)+1:length(lm2$coefficients), 1:2], summary(lm2)$coefficients[, 1], adjusted_var)
colnames(second_stage) = c("heckit : est", "heckit :se", "without package : est", "without package :se")
first_stage
second_stage
#==============================================================
## Exercise 3 Censoring
#==============================================================
# 3.1
data %>% 
  ggplot(aes(x=income)) + 
  geom_histogram(binwidth = 5000) +
  scale_x_continuous(labels = label_comma())
#censored value is 100,000

# 3.2
data_positive <- data_income %>% 
  mutate(married = ifelse(marital_status == "married", 1, 0)) %>% 
  filter(income_positive == 1)
tobit <- tobit(income ~ age + gender + children_number + self_edu_year + parent_edu_year + work_exp + married,
              right = 100000, data = data_positive, robust=TRUE)
summary(tobit)
# 3.3

############### excercise 4 ###############
raw_panel = read.csv("dat_A4_panel.csv") 
panel <- long_panel(raw_panel, prefix = "_", begin = 1997, end = 2019, label_location = "end")
panel <- panel %>%
  mutate(edu=0)
panel$edu<- as.factor(ifelse(panel$wave==1998,panel$CV_HIGHEST_DEGREE_9899,
                              ifelse(panel$wave==1999,panel$CV_HIGHEST_DEGREE_9900,
                                     ifelse(panel$wave==2000,panel$CV_HIGHEST_DEGREE_0001,
                                            ifelse(panel$wave==2001,panel$CV_HIGHEST_DEGREE_0102,
                                                   ifelse(panel$wave==2002,panel$CV_HIGHEST_DEGREE_0203,
                                                          ifelse(panel$wave==2003,panel$CV_HIGHEST_DEGREE_0304,
                                                                 ifelse(panel$wave==2004,panel$CV_HIGHEST_DEGREE_0405,
                                                                        ifelse(panel$wave==2005,panel$CV_HIGHEST_DEGREE_0506,
                                                                               ifelse(panel$wave==2006,panel$CV_HIGHEST_DEGREE_0607,
                                                                                      ifelse(panel$wave==2007,panel$CV_HIGHEST_DEGREE_0708,
                                                                                             ifelse(panel$wave==2008,panel$CV_HIGHEST_DEGREE_0809,
                                                                                                    ifelse(panel$wave==2009,panel$CV_HIGHEST_DEGREE_0910,
                                                                                                           ifelse(panel$wave==2010,panel$CV_HIGHEST_DEGREE_1011,
                                                                                                                  ifelse(panel$wave==2011,panel$CV_HIGHEST_DEGREE_1112,
                                                                                                                         ifelse(panel$wave==2013,panel$CV_HIGHEST_DEGREE_1314,panel$CV_HIGHEST_DEGREE_EVER_EDT))))))))))))))))

panel <- panel %>%
  mutate(eduyear=case_when(edu == 1~12,
                           edu == 2~12,
                           edu == 3~14,
                           edu == 4~16,
                           edu == 5~18,
                           edu == 6~23,
                           edu == 7~18,
                           edu == 0~NA_real_))
panel <- panel %>%
  mutate(age = wave - KEY_BDATE_Y,
         work_exp = rowSums(across(starts_with("CV_WKSWK_JOB"), function(x) x/52), na.rm = TRUE))
panel <- panel %>% 
  rename(gender = KEY_SEX, marstat = CV_MARSTAT_COLLAPSED)
panel <- panel %>%
  mutate(gender=case_when(gender == 1~'male',
                          gender==2~"famale"))
panel <- panel %>% mutate(married = case_when(marstat == 1~'married',
                                              marstat == 0|2|3|4~"others"))
data_panel <- panel %>%
  select(YINC.1700, gender, age, married, eduyear, work_exp) %>%
  rename(edu=eduyear, year=wave, income=YINC.1700) %>%
  drop_na()

# 4.2
within <- plm(income ~ age + gender + married + work_exp + edu, data_panel, model = "within")
summary(within)
between <- plm(income ~ age + gender + married + work_exp + edu, data_panel, model = "between")
summary(between)
first_dif <- plm(income ~ age + gender + married + work_exp + edu, data_panel, model = "fd")
summary(first_dif)