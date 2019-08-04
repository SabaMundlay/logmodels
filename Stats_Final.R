setwd("") #your working directory here 
install.packages("xtable")
install.packages("knitr")
library(knitr)
library(xtable)
library(MASS)
library(foreign)
library(aod)
library(ggplot2)
library(Rcpp)
library(plyr)
library(doBy)
library(nlme)

#Load data
Indo <- read.csv("indo.csv", header = T) 
head(Indo)
table(Indo$credit)

#create past-credit variable (whether or not the family has ever borrowed)
Indo$past_use_credit <- 0
table(Indo$past_use_credit)
Indo$past_use_credit[Indo$e111 == "Yes" | Indo$e112 == "Yes" | Indo$e113 == "Yes" | Indo$e114 == "Yes" | Indo$e115 == "Yes" | Indo$e116 == "Yes" | Indo$e117 == "Yes" | Indo$e118 == "Yes"] <- 1
table(Indo$past_use_credit)

#create the other variables 
community_savings <- Indo$d31
summary(community_savings)
current_credit <- Indo$credit

Indo$spending_habits <- NA
Indo$spending_habits[Indo$g12b == "Yes"] <- 1
Indo$spending_habits[Indo$g12b == "No"] <- 0
table(Indo$spending_habits)
spending_habits <- Indo$spending_habits

Indo$too_much_debt <- NA
Indo$too_much_debt[Indo$g12c == "Yes"] <- 1
Indo$too_much_debt[Indo$g12c == "No"] <- 0
table(Indo$too_much_debt)
too_much_debt <- Indo$too_much_debt


belief_in_savings <- revalue(Indo$g11, c("No" = "0", "Yes" = "1"))
table(belief_in_savings)

Indo$HH_credit <- NA
Indo$HH_credit[Indo$c110 == "No"] <- 0
Indo$HH_credit[Indo$c110 == "Yes"] <- 1
table(Indo$HH_credit)
HH_credit <- Indo$HH_credit

#create cross-tab tables
group1 <- xtabs(~community_savings + past_use_credit + belief_in_savings, data = Indo)
ftable(group1)
#every cell has a frequency above 5. (MODEL 1)

group3 <- xtabs(~community_savings + past_use_credit + too_much_debt, data = Indo)
ftable(group3)
#1 cell has a frequency less than 5 (MODEL 2)

group4 <- xtabs(~community_savings + spending_habits + HH_credit, data = Indo)
ftable(group4)
#1 cell has frequency less than 5 (MODEL 3)

group2 <- xtabs(~community_savings + past_use_credit + spending_habits, data = Indo)
ftable(group2)
group2
#Every cell has a frequency above 5 


#LOG LINEAR TESTING for group 1
#no effects model
test1 <- loglm(~community_savings + past_use_credit + belief_in_savings, data = group1)
summary(test1)

#all interactions model
test2 <- loglm(~community_savings * past_use_credit * belief_in_savings, data = group1)
summary(test2) #p-value = 1 (which is normal)

#take away 3-way interaction
test3 <- loglm(~community_savings * past_use_credit * belief_in_savings - community_savings:past_use_credit:belief_in_savings, data = group1)
summary(test3) #p-value = 0.2983711 (USE THIS MODEL)

#test all 2-way interactions
test4 <- loglm(~community_savings * belief_in_savings * past_use_credit - community_savings:past_use_credit:belief_in_savings - belief_in_savings:past_use_credit, data = group1)
summary(test4)

#lOG LINEAR TESTING for group 2
#no effects model
test5 <- loglm(~community_savings + past_use_credit + spending_habits, data = group2)
summary(test5)

#all interactions model
test6 <- loglm(~ community_savings * past_use_credit * spending_habits, data = group2)
summary(test6) #p-value = 1

test7 <- loglm(~community_savings * past_use_credit * spending_habits - community_savings:past_use_credit:spending_habits, data = group2)
summary(test7)

#LOG LINEAR TESTING for group 3
test8 <- loglm(~community_savings * past_use_credit * too_much_debt - community_savings:past_use_credit:too_much_debt, data = group3)
summary(test8) #p-vaue: 0.5337274

#LOG LINEAR TESTING for group 4
#no effect
test10 <-loglm(~current_credit + spending_habits + HH_credit, data = group4)
summary(test10)

#saturated
test11 <- loglm(~current_credit * spending_habits * HH_credit, data = group4)
summary(test11)

#taking away interactions.. 
test12 <- loglm(~community_savings * spending_habits * HH_credit - community_savings:spending_habits:HH_credit, data = group4)
summary(test12)

#model of conditional independence
test13 <- loglm(~community_savings:spending_habits + community_savings:HH_credit, data = group4)
summary(test13)

test14 <- loglm(~current_credit:spending_habits + spending_habits:HH_credit, data = group4)
summary(test14)

### parameters and odds ratios
# group 1: test 3
# group 2: test 8
# group 3: test 13

test3$param
ftable(test3$fit)
ftable(group1)
exp(test3$param$community_savings.past_use_credit)^ 2
exp(test3$param$community_savings.belief_in_savings) ^2
exp(test3$param$past_use_credit.belief_in_savings) ^ 2


test8$param
exp(test8$param$community_savings.past_use_credit)^ 2
exp(test8$param$community_savings.too_much_debt) ^2
exp(test8$param$past_use_credit.too_much_debt) ^ 2
summary(too_much_debt)

test13$param
exp(test13$param$community_savings.HH_credit) ^ 2
exp(test13$param$community_savings.spending_habits) ^ 2

#AIC
extractAIC(test3)
extractAIC(test8)

#OTHER FINDINGS...
femaleheads <- (Indo$b22 == Indo$b23)
femaleheads
others <- xtabs(~spending_habits + femaleheads + income, data = Indo)
summary(Indo$d13)
summary(Indo$d15_1)
summary(Indo$d112)
summary(Indo$d36_5)
summary(Indo$d43_1)
summary(Indo$k13a)
summary(Indo$m11c)
summary(Indo$m12a)
summary(Indo$b211)
