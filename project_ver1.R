project <- read.csv("D:\\Studia\\Magisterskie_HEBDA\\Biostatistics\\Project biostatistics_greg\\insurance.csv")

install.packages("psych")
library(psych)

install.packages("dplyr")
library(dplyr)
install.packages("Hmisc")
library(Hmisc)
install.packages("cowplot")
library(cowplot)

install.packages("WVPlots")
library(WVPlots)

install.packages("fastDummies")# to create dummy variable
library(fastDummies)

install.packages("tidyverse")
library(tidyverse)

summary(project)

psych::describe(project$age) 
hist(project$age,
     main="Histogram for the Age", 
     xlab="Age",
     col = 3)

aa<-ggplot(project, aes(x=age)) + 
  geom_histogram(color="black", fill="white")
aa


psych::describe(project$bmi)
hist(project$bmi,
     main="Histogram for the BMI index", 
     xlab="BMI index",
     col = 3)

psych::describe(project$children)
hist(project$children, 
     main="Histogram for number of children", 
     xlab="No. of children", 
     las=1, 
     xlim=c(0,5), 
     ylim=c(0,600), 
     col = 3)

#charges vs gender
 
summary(project$sex) 
tapply(project$charges, project$sex, psych::describe)

ggplot(data = project, aes(sex, charges)) + 
  geom_boxplot()+
  geom_boxplot(fill = c(6,8))+
  theme_classic()+
  ggtitle("Medical charges by gender")

#charges vs smoking

ggplot(data = project, aes(smoker, charges)) + 
  geom_boxplot()+
  geom_boxplot(fill = c(3,2))+
  theme_classic()+
  ggtitle("Medical charges by Smoking status")

#charges vs region 

ggplot(data = project,aes(region,charges)) + 
  geom_boxplot(fill = c(6:9)) +
  theme_classic() + 
  ggtitle("Medical Charges per Region")

psych::describe(project$charges)
hist(project$charges,
     main="Histogram for the Charges", 
     xlab="Charges",
     col = 3)
  
gender<-factor(project$sex,
            levels <- c("female","male"),
            labels = c(0,1)) #create lables for each category

smoker<-factor(project$smoker,
               levels <- c("no","yes"),
               labels = c(0,1))
smoker_dummy_0_N_1_Y<-dummy_cols(project$smoker)
gender_dummy_0_F_1_M<-dummy_cols(project$sex)

summary(project)

round(prop.table(table(project$sex, project$smoker),1),2)#sex perspective
#no  yes
#female 0.83 0.17
#male   0.76 0.24

round(prop.table(table(project$sex, project$smoker),2),2) #smoker perspective

#no  yes
#female 0.51 0.42
#male   0.49 0.58

#-----------------------

table(project$region)
table(region_smoker)
table(project$smoker == "yes")

hist(underweight)
qqnorm(underweight)
qqline(underweight)#show the distribution of real quantiles vs the predicted ones 
 #cannot install it ? 


## plot carges vs bmi with respect to BMI intervals
ggplot(data=project) +
  geom_point(mapping = aes(x=bmi, 
                           y=charges, 
                           color = BMIth, 
                           shape = smoker
                           )) 

#BMI vs charges vs SMOKING

ggplot(data=project) +
  geom_point(mapping = aes(x=bmi, y=charges, color = smoker)) 

## plot charges vs age
ggplot(data=project, mapping = aes(x=age, y=charges))+
    geom_point(alpha=1/5)
# or 
a<-qplot(age, charges, data = project, color = smoker)# good one
a

# to test ------------
x <- ggplot(project, aes(x=age, y=charges)) +
  geom_jitter(color = "blue", alpha = 0.5) +
  theme_light()

y <- ggplot(project, aes(x=bmi, y=charges)) +
  geom_jitter(color = "green", alpha = 0.5) +
  theme_light()

#-----------plotting tests------
p <- plot_grid(a,aa,x,y) 
plotsageandbmi<- ggdraw() + draw_label("1. Correlation between Charges and Age / BMI", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))



plot_grid(a,aa,x,y,
  align = "h", 
  axis = "b", 
  nrow = 2,
  labels = "AUTO",
  label_size = 8,
  rel_widths = c(2, 2))

# end of the testing ------------


ggplot(data=project)+
  geom_point(mapping = aes(x=BMIth == "obese", y=charges))





