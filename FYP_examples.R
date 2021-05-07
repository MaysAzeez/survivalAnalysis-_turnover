
## R code for the examples in the document 

library(survival)
library(tidyverse)
library(ggplot2)
library(readr)
library(survminer)

setwd("~/UNI/FYP/R")

#-----------clinical trial examples for Kaplan-Meier (KM)-------------------
# Creating the survival data
tt <- c(3,3,2,5)
cens <- c(1,0,0,1)

# Analyzing with KM method 
Surv(tt,cens)
KM<- survfit(Surv(tt,cens)~1, conf.type= "log-log")
summary(KM)

#plotting the survival curve 
plot(KM,xlab = "Time (Years)",
     ylab = "Survival Probability S(t)")
abline(h=0.5,col="dark green")


#-------------------Lung cancer KM example (large data)---------------------- 

#getting the data
data("lung")
head (lung)

# Analyzing with KM method 
KM1<- survfit(Surv(time,status)~1,lung, conf.type= "log-log")
KM1

# plotting the survival curve 
print(ggsurvplot(KM1,lung, xlab = "Time (Days)",
     ylab = "Survival Probability S(t)",
     surv.median.line = "hv", conf.int = TRUE, 
     title = "Lung Cancer Patients Survival",
     palette = "dark blue", break.time.by = 100,
     ggtheme = theme_light()))


#---------------------Cox Residual example-----------------------------------

# creating the data
tt <- c(6, 7, 10, 15, 19, 25)  
delta <- c(1, 0, 1, 1, 0, 1)  
trt <- c(0, 0, 1, 0, 1, 1)  

# analyzing using cox model 
result.coxph <- coxph(Surv(tt,delta)~trt)  

# display the variable coefficient 
result.coxph$coef  

# check if the proportional Hazard assumption is met with hypothesis testing  
cox.zph(result.coxph)

# Schoenfeld Residuals Plot 
plot(cox.zph(result.coxph, transform="km", global=TRUE))
title("Schoenfeld Residuals plot")
