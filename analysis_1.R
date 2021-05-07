
## Kaplan-Meier analysis 

library(survival)
library(survminer)
library(tidyverse)

setwd("~/UNI/FYP/R")

#------------------Dataset Formatting-------------------------------
# Renaming variables
emp_data2 <- read_csv("~/UNI/FYP/R/data/turnover.csv") %>%
             rename(Age = age, time = stag, Gender= gender)  %>%
             mutate(Age = trunc(Age)) %>%
  filter (industry %in% c("Banks","IT", "manufacture","Retail"))

#  Adding Levels and labels to Age and Gender
emp_data2$Gender <- factor(emp_data2$Gender,levels= c("f","m"),
                    label = c("Female","Male"))

age.groups <- cut(emp_data2$Age, breaks = c(17, 30, 40,50, 80),
                  labels = c("18-30yrs", "30-40yrs", "40-50 yrs", "50+ yrs"))
emp_data2$Age_Groups <- (age.groups)


##----------------------Overall Kaplan Meir Model analysis----------------------
# Kaplan Meir Model for the dataset
km_all <- survfit(Surv(time,event)~ 1, data = emp_data2)
km_all

# graph for survival curve for the dataset  
print(ggsurvplot(
  km_all,               # survfit object with calculated statistics.
  data = emp_data2,
  xscale = 12,
  conf.int = TRUE,          # show confidence intervals for survival curves.
  xlab = "Time (Years)",   # customize X axis label.
  break.time.by = 12,# break X axis in time intervals by 12.
  ggtheme = theme_light(),   # customize plot and risk table with a theme.
  risk.table = TRUE,    # absolute number and percentage at risk.
  cumevents = TRUE,   cumcensor = TRUE, tables.height = .17,
  risk.table.y.text.col = T, # color risk table text annotations.
  risk.table.y.text = FALSE, # show bars instead of names in text annotations
  # in legend of risk table.
  ncensor.plot = FALSE,      # plot the number of censored subjects at time t
  surv.median.line = "hv",   # add the median survival pointer.
  palette = "dark Green", title= "Overall Employees' Survival Probability"))


##-------------------------Kaplan Meir Model by Gender--------------------------

# KM  distribution by gender 
km_gender <- survfit(Surv(time,event) ~ Gender, data = emp_data2)
km_gender

# table of info for km_gender 
summary(km_gender)$table

# Log-Rank test for gender
surv_diff1 <- survdiff(Surv(time,event) ~ Gender, data = emp_data2)
surv_diff1

# graph of survival curve gender
print (ggsurvplot(
  km_gender,               # survfit object with calculated statistics.
  data = emp_data2,
  xscale = 12,
  pval = TRUE,               # show p-value of log-rank test.
  conf.int = FALSE,          # show confidence intervals for 
  xlab = "Time (Years)",   # customize X axis label.
  break.time.by = 12,     # break X axis in time intervals by 12.
  ggtheme = theme_light(),   # customize plot and risk table with a theme.
  risk.table = TRUE,         # absolute number and percentage at risk.
  cumevents = TRUE,   cumcensor = TRUE, tables.height = .17,
  risk.table.y.text.col = T, # color risk table text annotations.
  risk.table.y.text = TRUE, # show names instead of bars in text annotations
  # in legend of risk table.
  ncensor.plot = FALSE,      # plot the number of censored subjects at time t
  surv.median.line = "hv",   # add the median survival pointer.
  legend.labs = c("Female", "Male"),     # change legend labels.
  palette =  c("dark blue", "dark green"), # custom color palettes.
  title= "Employees' Survival Probability by Gender"))


#-----------------------------Kaplan Meir Model by Age--------------------------


# km by age  
km_age <- survfit(Surv(time,event) ~ Age_Groups, data = emp_data2)
km_age

# log-rank test
surv_diff2 <- survdiff( Surv(time,event) ~  Age_Groups, data = emp_data2)
surv_diff2

# graph of survival curve by age group
print(ggsurvplot(
  km_age,               # survfit object with calculated statistics.
  data = emp_data2,
  xscale = 12,
  pval = TRUE,               # show p-value of log-rank test.
  conf.int = FALSE,          # show confidence intervals for 
  xlab = "Time (Years)",   # customize X axis label.
  break.time.by = 12,     # break X axis in time intervals by 12.
  ggtheme = theme_light(),   # customize plot and risk table with a theme.
  risk.table = TRUE,         # absolute number and percentage at risk.
  cumevents = TRUE,   cumcensor = TRUE, tables.height = .17,
  risk.table.y.text.col = T, # color risk table text annotations.
  risk.table.y.text = TRUE, # show names instead of bars in text annotations
  # in legend of risk table.
  ncensor.plot = FALSE,      # plot the number of censored subjects at time t
  surv.median.line = "hv",   # add the median survival pointer.
  palette =  c("#E7B600", "dark blue","dark green","red"), # custom color palettes.
  title= "Employees' Survival Probability by Age"))
 