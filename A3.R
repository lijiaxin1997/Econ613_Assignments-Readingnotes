rm(list=ls())
library(tidyverse)
library(xtable)
library(ggplot2)
library(mlogit)

#==============================================================
## Exercise 1 Basic Statistics
#==============================================================
setwd("~/Desktop/22Spring/Econ613/Assignments/A3/Data")
datstu <- read.csv("datstu_v2.csv", header = TRUE)
datjss <- read.csv("datjss.csv", header = TRUE)
datsss <- read.csv("datsss.csv", header = TRUE)
# 1.1
nrow(datstu) #340823
datsss %>% distinct(schoolcode) %>% nrow() #898
datstu[,11:16] %>% unlist() %>% unique() %>% length() #33
# 1.2
choices <- 
  datstu %>% 
  pivot_longer(starts_with("schoolcode"), names_to = "school_rank", values_to = "schoolcode") %>% 
  pivot_longer(starts_with("choicepgm"), names_to = "program_rank", values_to = "choicepgm") %>% 
  mutate(school_rank = substr(school_rank, nchar(school_rank), nchar(school_rank)), program_rank = substr(program_rank, nchar(program_rank), nchar(program_rank)))%>% 
  filter(school_rank == program_rank) %>% 
  distinct(schoolcode, choicepgm) %>% 
  na.omit 
nrow(choices) #3080
# 1.3
datstu <- datstu %>% rename(ind = V1)
applications <- 
  datstu %>% 
  pivot_longer(starts_with("schoolcode"), names_to = "school_rank", values_to = "schoolcode") %>% 
  mutate(school_rank = substr(school_rank, nchar(school_rank), nchar(school_rank))) %>% 
  filter(!is.na(schoolcode))
same_dis <- 
  applications %>% 
  left_join(datsss, by = "schoolcode") %>% 
  left_join(datjss, by = "jssdistrict") %>% 
  rename(jsslong = point_x, jsslat = point_y) %>% 
  mutate(near = jsslong == ssslong & jsslat == ssslat) %>% 
  filter(near == TRUE) %>% 
  distinct(ind, .keep_all = TRUE) 
nrow(same_dis) #262603
# 1.4
stu_ad <- applications %>% mutate(enter_school = school_rank == rankplace)
ad_size <- stu_ad %>% group_by(schoolcode) %>% summarise(ad_size = sum(enter_school, na.rm = TRUE)) 
ad_size
# 1.5
cutoff <-
  stu_ad %>% filter(enter_school == TRUE) %>% group_by(schoolcode) %>% arrange(score, .by_group = TRUE) %>% select(schoolcode, score) %>% 
  rename(school_cutoff = score) %>% 
  slice_head()
school_list <- applications %>% select(schoolcode) %>% distinct
school_cutoff <- school_list %>% left_join(cutoff)
school_cutoff
#1.6
averagescore <-
  stu_ad %>% filter(enter_school == TRUE) %>% group_by(schoolcode) %>% summarise(school_quality = mean(score))
school_quality <- school_list %>% left_join(averagescore)
school_quality

#==============================================================
## Exercise 2 Data
#==============================================================
school_program <- datstu %>% 
  pivot_longer(starts_with("schoolcode"), names_to = "school_rank", values_to = "schoolcode") %>% 
  pivot_longer(starts_with("choicepgm"), names_to = "program_rank", values_to = "choicepgm") %>% 
  mutate(school_rank = substr(school_rank, nchar(school_rank), nchar(school_rank)),
         program_rank = substr(program_rank, nchar(program_rank), nchar(program_rank)))

ad_school_program <- 
  school_program %>% 
  mutate(enter =  program_rank == school_rank & program_rank == rankplace)
program_size <- 
  ad_school_program %>% 
  group_by(schoolcode, choicepgm) %>% 
  summarise(program_size = sum(enter, na.rm = TRUE)) %>% 
  na.omit
program_cutoff <-
  ad_school_program %>% 
  filter(enter == TRUE) %>% 
  group_by(schoolcode, choicepgm) %>% 
  select(schoolcode, choicepgm, score) %>% 
  na.omit %>% 
  arrange(score, .by_group = TRUE) %>% 
  rename(program_cutoff = score) %>% 
  slice_head()
program_quality <- 
  ad_school_program %>%
  filter(enter == TRUE) %>% 
  group_by(schoolcode, choicepgm) %>% 
  summarise(program_quality = mean(score)) %>% 
  na.omit()
school_program_info <- 
  choices %>% 
  left_join(ad_size, by = "schoolcode")%>% 
  left_join(school_quality, by = "schoolcode") %>% 
  left_join(school_cutoff, by = "schoolcode") %>% 
  left_join(program_size, by = c("schoolcode", "choicepgm")) %>% 
  left_join(program_quality, by = c("schoolcode", "choicepgm")) %>% 
  left_join(program_cutoff, by = c("schoolcode", "choicepgm"))
school_loc <- datsss %>% group_by(schoolcode) %>% filter(!is.na(schoolname)) %>% arrange(nchar(sssdistrict)) %>% slice_tail() %>% select(!V1)
school_program_info < -school_program_info %>% left_join(school_loc, by = "schoolcode")

#==============================================================
## Exercise 3 Distance
#==============================================================
stu_school_loc <- applications %>% 
  left_join(school_loc, by = "schoolcode") %>% 
  left_join(datjss, by = "jssdistrict") %>% 
  rename(jsslong = point_x, jsslat = point_y) %>% 
  mutate(near = jsslong == ssslong & jsslat == ssslat)
distance <- stu_school_loc %>%  mutate(distance = sqrt( (69.172 * (ssslong-jsslong) * cos(jsslat / 57.3) )^2 + (69.172 * (ssslat-jsslat) )^2  )   )

#==============================================================
## Exercise 4 Dimensionality Reduction
#==============================================================
stu_1<-arrange(datstu,-score) %>% head(20000)
school_program_1 <- stu_1 %>% 
  pivot_longer(starts_with("schoolcode"), names_to = "school_rank", values_to = "schoolcode") %>% 
  pivot_longer(starts_with("choicepgm"), names_to = "program_rank", values_to = "choicepgm") %>% 
  mutate(school_rank = substr(school_rank, nchar(school_rank), nchar(school_rank)),
         program_rank = substr(program_rank, nchar(program_rank), nchar(program_rank)))
ad_school_program_1 <- 
  school_program_1 %>% 
  mutate(enter =  program_rank == school_rank & program_rank == rankplace)
student_recode <- 
  ad_school_program_1 %>% 
  mutate(scode_rev = substr(schoolcode, 1, 3),
         pgm_rev = ifelse(choicepgm == "General Arts" | choicepgm == "Visual Arts", "arts",
                          ifelse(choicepgm == "Business" | choicepgm == "Home Economics", "economics",
                                 ifelse(choicepgm == "General Science", "science", "others"))),
         choice_rev = paste(scode_rev, pgm_rev, sep = "_"))
stu_school_1<-stu_1 %>% 
  pivot_longer(starts_with("schoolcode"), names_to = "school_rank", values_to = "schoolcode") %>% 
  mutate(school_rank = substr(school_rank, nchar(school_rank), nchar(school_rank))) %>% 
  filter(!is.na(schoolcode))
stu_ad_1 <-   stu_school_1 %>%   mutate(enter_school = school_rank == rankplace)
ad_size_1 <-   stu_ad_1 %>%   group_by(schoolcode) %>%   summarise(ad_size = sum(enter_school, na.rm = TRUE))
cutoff_1 <-
  stu_ad_1%>%   filter(enter_school == TRUE) %>%   group_by(schoolcode) %>%   arrange(score, .by_group = TRUE) %>%   select(schoolcode, score) %>% 
  rename(school_cutoff = score) %>% 
  slice_head()
school_cutoff_1 <- school_list %>% left_join(cutoff_1)
quality_1 <-
  stu_ad_1 %>% filter(enter_school == TRUE) %>% group_by(schoolcode) %>% summarise(school_quality = mean(score))
school_quality_1 <- school_list %>% left_join(quality_1)
program_size_1 <- 
  ad_school_program_1 %>% 
  group_by(schoolcode, choicepgm) %>% 
  summarise(program_size = sum(enter, na.rm = TRUE)) %>% 
  na.omit
program_cutoff_1 <-
  ad_school_program_1 %>% 
  filter(enter == TRUE) %>% 
  group_by(schoolcode, choicepgm) %>% 
  select(schoolcode, choicepgm, score) %>% 
  na.omit %>% 
  arrange(score, .by_group = TRUE) %>% 
  rename(program_cutoff = score) %>% 
  slice_head()
program_quality_1 <- 
  ad_school_program_1 %>%
  filter(enter == TRUE) %>% 
  group_by(schoolcode, choicepgm) %>% 
  summarise(program_quality = mean(score)) %>% 
  na.omit()
choices_1 <- 
  stu_1 %>% 
  pivot_longer(starts_with("schoolcode"), names_to = "school_rank", values_to = "schoolcode") %>% 
  pivot_longer(starts_with("choicepgm"), names_to = "program_rank", values_to = "choicepgm") %>% 
  mutate(school_rank = substr(school_rank, nchar(school_rank), nchar(school_rank)), program_rank = substr(program_rank, nchar(program_rank), nchar(program_rank)))%>% 
  filter(school_rank == program_rank) %>% 
  distinct(schoolcode, choicepgm) %>% 
  na.omit 
school_program_info_1 <- choices_1 %>% 
  left_join(ad_size_1, by = "schoolcode")%>% 
  left_join(school_quality_1, by = "schoolcode") %>% 
  left_join(school_cutoff_1, by = "schoolcode") %>% 
  left_join(program_size_1, by = c("schoolcode", "choicepgm")) %>% 
  left_join(program_quality_1, by = c("schoolcode", "choicepgm")) %>% 
  left_join(program_cutoff_1, by = c("schoolcode", "choicepgm"))

school_program_info_1<-school_program_info%>%left_join(school_loc, by = "schoolcode")

student_recode <- student_recode %>% group_by(ind) %>% slice_head()

#==============================================================
## Exercise 5 First Model
#==============================================================
student_recode.sample <- student_recode %>%   slice(sample(nrow(student_recode), size =nrow(student_recode)/100, replace = FALSE))
mlogit_1 <- student_recode.sample %>% select(score, choice_rev) %>%  mlogit.data(choice = "choice_rev", shape = "wide") 
mlogit_result_1 <- mlogit(choice_rev ~ 1 | score, data = mlogit_1)
mlogit_result_1 <- summary(mlogit_result_1)$coefficients

#==============================================================
## Exercise 6 Second Model
#==============================================================
stu_sch<-student_recode%>%left_join(school_program_info_1)
stu_sch.sample <- stu_sch %>% slice(sample(nrow(stu_sch), size =nrow(stu_sch)/100, replace = FALSE))
mlogit_2 <- stu_sch.sample %>% select(school_quality, choice_rev) %>%  mlogit.data(choice = "choice_rev", shape = "wide") 
mlogit_result_2 <- mlogit(choice_rev ~ 0 | school_quality, data = mlogit_2)
mlogit_result_2 <- summary(mlogit_result_2)$coefficients


