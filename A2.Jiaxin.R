rm(list=ls())
library(tidyverse)
library(xtable)
library(ggplot2)
library(AER)
library(ggpubr)
#==============================================================
## Exercise 1 OLS estimate
#==============================================================
setwd("~/Desktop/22Spring/Econ613/Assignments/A1/Data")
data <- read.csv("datind/datind2009.csv")
wage <- data$wage[!is.na(data$wage) & !is.na(data$age)]
age <- data$age[!is.na(data$wage) & !is.na(data$age)]
intercept <- rep(1, length(wage))
Y <- as.matrix(wage)
X <- as.matrix(cbind(intercept, age))
# 1.1
cor(Y, X)
# 1.2
beta <- solve(t(X) %*% X) %*% (t(X) %*% Y)
beta
# 1.3
# standard formulas
residuals <- Y - X %*% beta
p <- ncol(X) - 1 
df <- nrow(X) - p - 1 
res_var <- sum(residuals^2) / df 
beta_cov <- res_var * solve(t(X) %*% X)
beta_se <- sqrt(diag(beta_cov))
beta_se
dat<-data.frame(cbind(X,Y))
# bootstrap
colnames(dat) = c("intercept","age","wage")
#R=49
R=499
nind = nrow(dat);            # number of individuals
reg = lm(wage~age,data = dat)
nvar = length(reg$coefficients)  # number of variables
  outs = mat.or.vec(R,nvar)
  set.seed(123)
  for (i in 1:R)
  {
    samp     = sample(1:nind,nind,rep=TRUE)
    dat_samp = dat[samp,]
    reg1     = lm(wage ~ age,data = dat_samp)
    outs[i,] = reg1$coefficients
  }
  
  mean_est = apply(outs,2,mean)
  sd_est   = apply(outs,2,sd)
  
  est = cbind(mean_est,
              sd_est)
  colnames(est) = c("BT: est","BT: sd")
est
#==============================================================
## Exercise 2 Detrend Data
#==============================================================
years <- 2005:2018
temp = list.files(path = "datind", pattern = "*.csv$")
temp2 = temp[-1]
datindnames = temp2[-15]
obj_datind <- sub(".csv", "", datindnames)
for (i in 1:length(datindnames)){
  assign(obj_datind[i],read.csv(paste("datind/datind", years[i], ".csv", sep = "", collapse = "")))
  i=i+1
}
indlist = list(datind2006,datind2007,datind2008,datind2009,datind2010,datind2011,datind2012,datind2013,datind2014,datind2015,datind2016,datind2017,datind2018)
ind = datind2005
for (j in indlist){
  ind <- rbind(ind,j)
}
ind1 <- ind[complete.cases(ind$wage),]
# 2.1
ind1$ag <- as.factor(ifelse(ind1$age<18, '0-18',ifelse(ind1$age<26, '18-25',
                                                     ifelse(ind1$age<31, '26-30',
                                                            ifelse(ind1$age<35, '31-35', 
                                                                   ifelse(ind1$age<41, '36-40',
                                                                          ifelse(ind1$age<46, '41-45',
                                                                                 ifelse(ind1$age<51, '46-50',
                                                                                        ifelse(ind1$age<56, '51-55',
                                                                                               ifelse(ind1$age<61, '56-60','60+'))))))))))
# 2.2
agegroup1 <- subset(ind1, ag == "0-18")
agegroup2 <- subset(ind1, ag == "18-25")
agegroup3 <- subset(ind1, ag == "26-30")
agegroup4 <- subset(ind1, ag == "31-35")
agegroup5 <- subset(ind1, ag == "36-40")
agegroup6 <- subset(ind1, ag == "41-45")
agegroup7 <- subset(ind1, ag == "46-50")
agegroup8 <- subset(ind1, ag == "51-55")
agegroup9 <- subset(ind1, ag == "56-60")
agegroup10 <- subset(ind1, ag == "60+")

p1 <- agegroup1%>%ggplot(aes(x = year, y = wage)) +
  geom_point() 
p2 <- agegroup2%>%ggplot(aes(x = year, y = wage)) +
  geom_point() 
p3 <- agegroup3%>%ggplot(aes(x = year, y = wage)) +
  geom_point() 
p4 <- agegroup4%>%ggplot(aes(x = year, y = wage)) +
  geom_point() 
p5 <- agegroup5%>%ggplot(aes(x = year, y = wage)) +
  geom_point() 
p6 <- agegroup6%>%ggplot(aes(x = year, y = wage)) +
  geom_point() 
p7 <- agegroup7%>%ggplot(aes(x = year, y = wage)) +
  geom_point() 
p8 <- agegroup8%>%ggplot(aes(x = year, y = wage)) +
  geom_point() 
p9 <- agegroup9%>%ggplot(aes(x = year, y = wage)) +
  geom_point() 
p10 <- agegroup10%>%ggplot(aes(x = year, y = wage)) +
  geom_point() 
p1
# 2.3
ind$year2005 <- ifelse(ind$year == 2005, 1, 0)
ind$year2006 <- ifelse(ind$year == 2006, 1, 0)
ind$year2007 <- ifelse(ind$year == 2007, 1, 0)
ind$year2008 <- ifelse(ind$year == 2008, 1, 0)
ind$year2009 <- ifelse(ind$year == 2009, 1, 0)
ind$year2010 <- ifelse(ind$year == 2010, 1, 0)
ind$year2011 <- ifelse(ind$year == 2011, 1, 0)
ind$year2012 <- ifelse(ind$year == 2012, 1, 0)
ind$year2013 <- ifelse(ind$year == 2013, 1, 0)
ind$year2014 <- ifelse(ind$year == 2014, 1, 0)
ind$year2015 <- ifelse(ind$year == 2015, 1, 0)
ind$year2016 <- ifelse(ind$year == 2016, 1, 0)
ind$year2017 <- ifelse(ind$year == 2017, 1, 0)
ind$year2018 <- ifelse(ind$year == 2018, 1, 0)
y1 <- as.matrix(ind1[, "wage"])
x1 <- as.matrix(ind1[, "age"])
Y1<-as.matrix(y1)
intercept1<-rep(1, nrow(ind1))
X1<-as.matrix(cbind(intercept1,x1,ind1$year2006,ind1$year2007,ind1$year2008,ind1$year2009,ind1$year2010,ind1$year2011,ind1$year2012,ind1$year2013,ind1$year2014,ind1$year2015,ind1$year2016,ind1$year2017,ind1$year2018))
colnames(Y1)[1] <- 'wage'
colnames(X1)[2] <- 'age'
# beta_1 = lm(Y1~X1)
beta1 <- solve(t(X1) %*% X1) %*% (t(X1) %*% Y1)

#==============================================================
## Exercise 3 Numerical Optimization
#==============================================================
data_1<-read.csv("datind/datind2007.csv", header = TRUE)
# 3.1
em<-subset(data_1,empstat != "Inactive")
# 3.3
em$emst <- as.factor(ifelse(em$empstat=="Employed", '1',"0"))
em.probit <- glm(emst~age, 
                 data = em,
                 family = binomial(link = "probit"))
summary(em.probit)
# 3.4

#==============================================================
## Exercise 4 Discrete choice
#==============================================================
indlist2 <- list(datind2006,datind2007,datind2008,datind2009,datind2010,datind2011,datind2012,datind2013,datind2014,datind2015)
ind2 <- datind2005
for (j in indlist2){
  ind2 <- rbind(ind2,j)
}
ind2$year2005 <- ifelse(ind2$year == 2005, 1, 0)
ind2$year2006 <- ifelse(ind2$year == 2006, 1, 0)
ind2$year2007 <- ifelse(ind2$year == 2007, 1, 0)
ind2$year2008 <- ifelse(ind2$year == 2008, 1, 0)
ind2$year2009 <- ifelse(ind2$year == 2009, 1, 0)
ind2$year2010 <- ifelse(ind2$year == 2010, 1, 0)
ind2$year2011 <- ifelse(ind2$year == 2011, 1, 0)
ind2$year2012 <- ifelse(ind2$year == 2012, 1, 0)
ind2$year2013 <- ifelse(ind2$year == 2013, 1, 0)
ind2$year2014 <- ifelse(ind2$year == 2014, 1, 0)
ind2$year2015 <- ifelse(ind2$year == 2015, 1, 0)
em2<-subset(ind2,empstat != "Inactive")
em2$emst <- ifelse(em2$empstat=="Employed", 1,0)

em2.probit <- glm(emst~age+year2006+year2007+year2008+year2009+year2010+year2011+year2012+year2013+year2014+year2015, 
                  data = em2,
                  family = binomial(link = "probit"))
summary(em2.probit)
em2.logit <- glm(emst~age+year2006+year2007+year2008+year2009+year2010+year2011+year2012+year2013+year2014+year2015, 
                 data = em2,
                 family = binomial(link = "logit"))
summary(em2.logit)
em2.linear<- lm(emst ~ age+year2006+year2007+year2008+year2009+year2010+year2011+year2012+year2013+year2014+year2015,data=em2)
summary(em2.linear)

#==============================================================
## Exercise 5 Marginal Effects
#==============================================================
margin_data0 = data.frame(age = mean(em2$age),year2006 = mean(em2$year2006),year2007 = mean(em2$year2007),year2008 = mean(em2$year2008),year2009 = mean(em2$year2009),year2010 = mean(em2$year2010),year2011 = mean(em2$year2011),year2012 = mean(em2$year2012),year2013 = mean(em2$year2013),year2014 = mean(em2$year2014),year2015 = mean(em2$year2015))

probit = predict(em2.probit, margin_data0, type="response", se=TRUE)
probit_fit = data.frame(Margin = probit$fit[1], se=probit$se.fit[1])
probit_fit

margin_data1 = data.frame(age = mean(em2$age),year2006 = mean(em2$year2006),year2007 = mean(em2$year2007),year2008 = mean(em2$year2008),year2009 = mean(em2$year2009),year2010 = mean(em2$year2010),year2011 = mean(em2$year2011),year2012 = mean(em2$year2012),year2013 = mean(em2$year2013),year2014 = mean(em2$year2014),year2015 = mean(em2$year2015))

logit = predict(em2.logit, margin_data1, type="response", se=TRUE)
logit_fit = data.frame(Margin = logit$fit[1], se=logit$se.fit[1])
logit_fit
