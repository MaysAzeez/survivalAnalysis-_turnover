
## Cox model analysis

library(survival)
library(survminer)
library(tidyverse)
library(broom)
library(pec)

setwd("~/UNI/FYP/R")

#---------------------format dataset-----------------------------------
emp_data2 <- read_csv("~/UNI/FYP/R/data/turnover.csv") %>%
  rename(Age = age, time = stag, Gender= gender)  %>%
  mutate(Age = trunc(Age)) %>%
   filter (industry %in% c("Banks","IT", "manufacture","Retail"))


# Adding Levels and labels to Age and Gender
emp_data2$Gender <- factor(emp_data2$Gender,levels= c("f","m"),
                          label = c("Female","Male"))
age.groups <- cut(emp_data2$Age, breaks = c(17, 30, 40,50, 80),
                  labels = c("18-30yrs", "30-40yrs", "40-50 yrs", "50+ yrs"))
emp_data2$Age_Groups <- (age.groups)



#----------------Selecting significant variables ----------------------------
# cox model to select variables  
modelall <- coxph(Surv(time,event) ~ greywage+ industry+ independ 
                  + Gender + Age + selfcontrol + Age_Groups + coach + anxiety + novator 
                  + extraversion , data = emp_data2 )

result.step <- step(modelall, scope= list(upper=~ industry+ greywage+ independ 
                   + Gender + Age + selfcontrol + Age_Groups + coach + anxiety + novator
                   + extraversion ,lower =~industry))

#----------------------multivariate analysis-----------------------------------

# Cox model for The Combined selected variables 
all_coxph <- coxph(Surv(time,event) ~  industry + Age + selfcontrol + greywage, 
                   data = emp_data2,x=TRUE,y=TRUE)
broom::tidy(all_coxph)
summary(all_coxph)


# forest plot
print(ggforest(all_coxph, data = emp_data2,
         main = "Covariates Hazrd Ratio and p-value"))


# Testing the Key Assumption: Proportional Hazards
cox.zph(all_coxph, transform="km", global=TRUE)
print(ggcoxzph(cox.zph(all_coxph), font.main = 12,
               ggtheme = theme_light(),point.col = "dark orange"))


# ---------Generating the survival curve plot for the multivariate analysis------

#creating new dataset for the factors
newdat <- expand.grid(industry = c("Retail", "IT","manufacture", "Banks"),
                      selfcontrol = mean(emp_data2$selfcontrol),
                      greywage = c("grey","white"),
                      Age = mean(emp_data2$Age))
newdat$id <- 1:8
newdat


# combining the new dataset with cox model outputs
sel <- surv_summary(survfit(all_coxph, data = emp_data2, newdata = newdat)) %>%
       merge(newdat, by.x = "strata", by.y = "id")


#  survival curve graph for selected variables (all_coxph) 
print(ggsurvplot_df(sel, color = "greywage", linetype = 1, censor = FALSE,
                    break.time.by = 12,  xscale = 12,
                    legend.title = "Wage Type",palette =c("#2E9FDF","orange")) +
                   facet_grid(industry~.) + labs(x = "Time (Years)",
                   y = "Survival probability") + theme_bw())

#------------------Univariate analysis-------------------------------------

#----analysis per industry-----
cox1 <- coxph(Surv(time,event) ~ industry  , data = emp_data2 )
broom::tidy(cox1)
summary(cox1)

# graph  for industry 
newdat1 <-with(emp_data2,data.frame(industry = c("Retail", "IT", 
                                      "manufacture", "Banks")))

plot(survfit(cox1,data = emp_data2,newdata = newdat1),
     conf.int = FALSE, xscale = 12,
     col = c("dark blue","dark green","#2E9FDF","orange"),
     xlab = "Time (Years)",
     ylab = "Survival Probability",  
     main = "Survival Probability by Industry")
legend("topright",inset = 0.04, legend=c("Retail", "IT","manufacture", "Banks"),
       box.lty = 0, lty = 1:1,
       col = c("dark blue","dark green","#2E9FDF","orange"),
       text.col = c("dark blue","dark green","#2E9FDF","orange"))



#---------analysis per Age---------- 
cox2 <- coxph(Surv(time,event) ~ Age  , data = emp_data2 )
broom::tidy(cox2)
summary(cox2)


#-----------analysis of  self-control--------------
cox3 <- coxph(Surv(time,event) ~ selfcontrol  , data = emp_data2 )
broom::tidy(cox3)
summary(cox3)



#--------------analysis of Wage----------------- 
cox4 <- coxph(Surv(time,event) ~ greywage  , data = emp_data2 )
broom::tidy(cox4)
summary(cox4)

# graph  for Wage
newdat4 <-with(emp_data2,data.frame(greywage = c("grey","white")))

plot(survfit(cox4, newdata = newdat4),
     conf.int = FALSE, xscale = 12,
     col = c("dark green","#2E9FDF"),
     xlab = "Time (Years)",
     ylab = "Survival Probability",  
     main = "Survival Probability by Wage")
     ggtheme = theme_light()
legend("topright",inset = 0.05, legend=c("Grey","White"),
       box.lty = 0, lty = 1:1,
       col = c("dark green","#2E9FDF"),
       text.col = c("dark green","#2E9FDF"))
grid()



#------------------predictions--------------------------------------

# cumulative base hazard for the model used for predication 
h <-basehaz(all_coxph)

# to find the base hazard at 5 years(60 months) (h_0(t=60)
#we take way the cumulative hazard value for the time just
#before 5 years from the cumulative hazard at 5 years
h_0 <- h[382,1] - h[381,1]
h_0

# dataset for predication
pred_data <- data.frame(Age = c(25,40), greywage = c("white","grey"),
                industry = c("IT","manufacture"), selfcontrol = c(7.5,6.5)) 

                
#predication of survival after certain time  (5 years= 60 months)
predictSurvProb(all_coxph,pred_data, time=60)

#plot the predicted survival 
plot(survfit(all_coxph, newdata = pred_data),
    xscale=12,
    col = c("dark green","#2E9FDF"),
    xlab = "Time (Years)",
    ylab = "Survival Probability",  
    main = "Predicated Survival Probability over 15 Years")
legend("topright",inset = 0.05, 
       legend=c("Subject 1","Subject 2"),
       box.lty = 0, lty = 1:1,
       col = c("dark green","#2E9FDF"),
       text.col = c("dark green","#2E9FDF"))
grid()
