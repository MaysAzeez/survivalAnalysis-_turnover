
## Initial analysis of the data


library(survival)
library(tidyverse)
library(survminer)
library(rms)
library(ggthemes)
library(corrplot)
library(forcats)
library(gridExtra)
library(stringr)
library(caret)
library(formattable)
library(rpart)
library(rpart.plot)
library(Boruta)
library(DMwR)
library(ROCR)
library(dummy)
library(neuralnet)
library(caretEnsemble)
library(h2o)

setwd("~/UNI/FYP/R")


#---------------------- The dataset---------------------------------
# formatting and reading  the data variables 

emp_data <- read_csv("~/UNI/FYP/R/data/turnover.csv") %>%
  rename(time = stag, Gender= gender)  %>%
  mutate(Age = trunc(age)) %>%
  select(-age)

# Adding factors to gender
emp_data$Gender <- factor(emp_data$Gender,levels= c("f","m"),
                          label = c("Female","Male"))


#------------------checking missing data------------------------------  
mis <-is.na(emp_data)
  

#----------------Counting the period of time--------------------------

# max number of years the subjects were followed
  c <- round((max(emp_data$time))/12,2) 
  c
 
#--------gender and age statistics------------------------------------  

# count number of each gender and age statistics
  genc <- emp_data %>%
    group_by(Gender) %>%
    summarise(num_gender = n() , mean_age = mean(Age), 
              min_age = min(Age), max_age = max(Age))
  genc

# distribution of age by gender
print(ggplot(emp_data,aes(x=Age, color = Gender, fill = Gender))+
        geom_histogram(binwidth=4,
                       alpha = 0.3,position = "identity",aes(y=..count..))+
        scale_color_manual(values = c("dark red", "dark green"))+ 
        theme_few()+theme(legend.position="right",
                          plot.title = element_text(hjust=0.5,size=15)) +
        scale_y_continuous(breaks=seq(0,350,50)) +
        labs(x="Age",y="Count",title="Distribution of Age according to Gender"))  

  
#--------------------------- industry statistic--------------------
 
# number in each industry 
Inc <- emp_data %>%
    group_by(industry) %>%
    count() 
Inc

# distribution of  attrition per industry 
emp_data$event <- factor(emp_data$event,levels= c("0","1"), 
                         label = c("Quit","Censored"))

print(ggplot(emp_data,aes(x=industry,group=event))+
        geom_bar(aes(y=..count..,fill=factor(..x..)),stat="count")+
        facet_grid(~event)+
        theme(axis.text.x=element_text(angle=90,vjust=0.5),legend.position="none",
              plot.title=element_text(size=14,hjust=0.5))+
        labs(x="Industry",y="Count",title="Attrition Vs Industry")+
        scale_x_discrete(labels=function(x) str_wrap(x,width=10)))



#--------------correlation matrix between variables and turnover time-----------
numeric <- emp_data %>% 
  select(time,Age, extraversion,independ,selfcontrol,anxiety,novator)
corrplot(cor(numeric),method="color", addCoef.col = "grey",
   type="lower",tl.col = "black")

