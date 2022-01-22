#==============================================================
## Exercise 1 Basic Statistics
#==============================================================
rm(list=ls())
library(tidyverse)

years <- 2004:2019
dathhnames = list.files(path = "~/Desktop/22Spring/Econ613/Assignments/A1/Data/dathh", pattern = "*.csv$")
datindnames = list.files(path = "~/Desktop/22Spring/Econ613/Assignments/A1/Data/datind", pattern = "*.csv$")
setwd("~/Desktop/22Spring/Econ613/Assignments/A1")
obj_dathh <- sub(".csv", "", dathhnames)
obj_datind <- sub(".csv", "", datindnames)
for (i in 1:length(dathhnames)){
  assign(obj_dathh[i],read.csv(paste("Data/dathh/dathh", years[i], ".csv", sep = "", collapse = "")))
  i=i+1
}
for (i in 1:length(datindnames)){
  assign(obj_datind[i],read.csv(paste("Data/datind/datind", years[i], ".csv", sep = "", collapse = "")))
  i=i+1
}

# 1.1
nrow(dathh2007) # Ans: 10498
# 1.2
dathh2005_f <- filter(dathh2005, mstatus == "Couple, with Kids")
nrow(dathh2005_f) # Ans: 3374
# 1.3
nrow(datind2008) # Ans: 25510
# 1.4
datind2016_f <- filter(datind2016, age>=25 & age<=35)
nrow(datind2016_f) # Ans: 2765
# 1.5
datind2009 %>% group_by(profession, gender) %>% summarise(n=n())
# 1.6
datind2005 %>% filter(! is.na(wage)) %>% ggplot(aes(x = wage)) + geom_density()
datind2019 %>% filter(! is.na(wage)) %>% ggplot(aes(x = wage)) + geom_density()

datind2005 %>% summarise(MEAN = mean(wage, na.rm = T), 
                         STD = sd(wage, na.rm = T), 
                         IDR = quantile(wage, 9/10, na.rm = T) - quantile(wage, 1/10, na.rm = T)) 
datind2019 %>% summarise(MEAN = mean(wage, na.rm = T), 
                         STD = sd(wage, na.rm = T), 
                         IDR = quantile(wage, 9/10, na.rm = T) - quantile(wage, 1/10, na.rm = T))
# Cannot calculate D9/D1, because D1 is 0

gini <- function(data){
  wage <- data$wage[!is.na(data$wage)]#clear NA
  wage <- sort(wage)
  n <- length(wage)
  a <- 2*sum((wage-sum(wage)/n) * 1:n)
  b <- n^2*mean(wage)
  gini <- a/b
  return(gini)}
gini(datind2005) # Ans: 0.6671654
gini(datind2019) # Ans: 0.6655301
# 1.7
datind2010 %>% filter(! is.na(age)) %>% ggplot(aes(x = age)) + geom_density()
datind2010 %>% filter(! is.na(age)) %>% ggplot(aes(x = age)) + geom_histogram() 

datind2010 %>% filter(! is.na(age)) %>% ggplot(aes(x = age, fill = gender, group = gender)) + 
  geom_density(alpha =0.7) + 
  scale_x_continuous(limits = c(NA, 100)) + 
  scale_y_continuous() 
# In 2010, the number of man who is older than 70 is greater than woman. 
# Meanwhile, the number of man who is younger than 30 is less than woman.
# 1.8
hhinParis <- dathh2011 %>%  filter(location == "Paris") %>% pull(idmen)
indinParis <- datind2011 %>% filter(idmen %in% hhinParis) 
indinParis %>% nrow() # Ans: 3514

#==============================================================
## Exercise 2 Merge Datasets
#==============================================================
# 2.1.1 & 2.1.2 
# Files reading is completed at the start of programming
list_hh <- list(dathh2004, dathh2005, dathh2006, dathh2007, dathh2008, dathh2009, dathh2010,
               dathh2011, dathh2012, dathh2013, dathh2014, dathh2015, dathh2016, dathh2017,
               dathh2018, dathh2019)
list_ind <- list(datind2004, datind2005, datind2006, datind2007, datind2008, datind2009, datind2010,
               datind2011, datind2012, datind2013, datind2014, datind2015, datind2016, datind2017,
               datind2018, datind2019)
data_hh <- dathh2004
for (i in list_hh){
  data_hh <- rbind(data_hh,i)
}
data_ind <- datind2004
for (j in list_ind){
  data_ind <- rbind(data_ind,j)
}
# 2.1.3
tbl_vars(data_ind)[tbl_vars(data_ind) %in% tbl_vars(data_hh)]
# 2.1.4
all_data <- left_join(data_ind, data_hh, by=c("idmen","year"))
all_data[all_data == ""] <- NA
# 2.2.1
famnum <- all_data %>% count(idmen, year)
fournum <- famnum %>% filter(n>4)
fournum %>% nrow()
fournum %>% count(year)
# 2.2.2
unemp <- all_data %>% filter(empstat == "Unemployed") %>% count(idmen,year) %>% filter(n>=1)
unemp %>% nrow()
unemp %>% count(year)
# 2.2.3
sameprof <- all_data %>% filter(!is.na(profession)) %>% count(idmen, year, profession) %>% filter(n>=2)
sameprof %>% nrow()
sameprof %>% count(year)
# 2.2.4
kids <- all_data %>% filter(mstatus=="Couple, with Kids") %>% distinct(idind, year)
kids %>% nrow()
kids %>% count(year)
# 2.2.5
indParis <- all_data %>% filter(location == "Paris") %>% distinct(idind, year)
indParis %>% nrow()
indParis %>% count(year)
# 2.2.6
top <- sort(famnum$n,decreasing = T)[1]
mostfam <- famnum %>% filter(n == top) %>% pull(idmen)
mostfam %>% as.character()
# 2.2.7
hh_2010_11 <- all_data %>% filter(year == 2010 | year == 2011) %>% distinct(idmen, year) 
hh_2010_11 %>% nrow()
hh_2010_11 %>% count(year)

#==============================================================
## Exercise 3 Migration
#==============================================================

# 3.1
hh_grouped <- data_hh %>% group_by(idmen)
hh_exit <- hh_grouped %>%
  mutate(rn = rank(desc(year), ties.method = "first")) %>%
  filter(rn == 1) %>%
  mutate(year_exit = year) %>%
  select(idmen, year_exit) 
hh_enter <- hh_grouped %>%
  mutate(rn = rank(desc(year), ties.method = "first")) %>%
  filter(rn == n()) %>%
  mutate(year_enter = year) %>%
  select(idmen, year_enter) 
hh_enter_exit <- left_join(hh_enter, hh_exit)
hh_time <- hh_enter_exit %>% 
  mutate(time_spent = year_exit - year_enter + 1) %>% 
  distinct(idmen, year_enter, year_exit, time_spent)

hh_time %>%  ggplot(aes(x = time_spent)) +  geom_histogram(binwidth = 0.5) 

#3.2
hh_datent <- all_data %>% mutate(year_is_datent = (datent == year))
hh_datent %>% head(10)
share <- hh_datent %>% group_by(year) %>% summarise(datent_portion = mean(year_is_datent, na.rm=T)) 
share %>% ggplot(aes(x = year, y = datent_portion)) + geom_line() 

#3.3
hh_mig <- all_data %>% mutate(year_movein = ((myear == year) & !is.na(myear)) |((move == 2) & !is.na(move)))
hh_mig %>% head(10)
move_share <- hh_mig %>% group_by(year) %>% summarise(movein_portion = mean(year_movein, na.rm=T)) 
move_share %>% ggplot(aes(x = year, y = movein_portion)) + geom_line() 

#3.4
datent_mig <- left_join(share,move_share)
datent_mig %>% ggplot(aes(x = year)) + geom_line(aes(y = datent_portion), colour = "datent_portion") +
  geom_line(aes(y = movein_portion), color = "movein_portion") +
  labs(x = "Year", y = "(%)", color = "Legend") + 
  scale_color_manual("", breaks = c("datent_portion","movein_portion"), values = c("red","green"))
datent_mig %>% ggplot( aes(x = year)) +
  geom_line(aes(y = datent_portion, colour = "datent_portion")) +
  geom_line(aes(y = movein_portion, colour = "movein_portion")) +
  scale_colour_manual("", 
                      breaks = c("datent_portion", "movein_portion"),
                      values = c("red", "green")) +
  xlab(" ") +
  scale_y_continuous("(%)") + 
  labs(title="Datent and Movein Portion")

#3.5
hh_change_work <- hh_mig %>% 
  filter(year_movein == 1) %>%
  group_by(idind) %>%
  mutate(change_pro = n_distinct(profession, na.rm = TRUE) > 1) %>%
  mutate(change_em = n_distinct(empstat,na.rm = TRUE)>1) %>%
  mutate(change_work = change_em|change_pro)
hh_change_work %>% ungroup() %>% filter(change_work == 1) %>% nrow()


#==============================================================
## Exercise 4 Attrition
#==============================================================

ind_grouped <- data_ind %>% group_by(idind) %>% arrange(year)

ind_enter_exit <- left_join(hh_time, ind_grouped, by = "idmen")
ind_enter_exit <- ind_enter_exit %>% mutate(exit = (year_exit == year))

attrition_rate <- ind_enter_exit %>% group_by(year) 
attrition_rate %>% summarise(attrition_rate = mean(exit, na.rm=T)) 


