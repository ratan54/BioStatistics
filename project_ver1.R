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

install.packages("stats")
library(stats)

install.packages("rstatix")
library(rstatix)

summary(project)

psych::describe(project$age) 

hist1age<-ggplot(project, aes(x=age)) + 
  geom_histogram(color="black", fill="green") + 
  ggtitle("Histogram for the Age")

hist1age

psych::describe(project$bmi)

hist2bmi<-ggplot(project, aes(x=bmi)) + 
  geom_histogram(color="black", fill="green") + 
  ggtitle("Histogram for the BMI")

hist2bmi

psych::describe(project$children)

hist3children<-ggplot(project, aes(x=children)) + 
  geom_histogram(color="black", fill="green") + 
  ggtitle("Histogram for the number of children")

hist3children

psych::describe(project$charges)

hist4charges<-(ggplot(project, aes(x=charges)) + 
                       geom_histogram(color="black", fill="green") + 
                 coord_cartesian(xlim = c(0, 60000), ylim = c(0, 250))+
                       ggtitle("Histogram for the Charges"))
hist4charges
hist(log(project$charges))

#ploting quantitative variables x 4

plot_grid(hist1age,hist2bmi,hist3children,hist4charges,
          align = "h", 
          axis = "b", 
          nrow = 2,
          labels = "AUTO",
          label_size = 8,
          rel_widths = c(2, 2))

# piechart of gender 

table(project$sex)

pie(table(gender_dummy_0_F_1_M$.data_female),labels = project$sex, 
    main = "Gender distribution",
    col = (6:5))

#piechart of smokers


table(project$smoker)

pie(table(smoker_dummy_0_N_1_Y$.data_no),labels = project$smoker, 
    main = "Smokers distribution",
    col = (6:5))

#piechart of region 
table(project$region)

pie(table(geoloc),labels = project$region, 
    main = "Geographical Distribution of The Population",
    col = (5:8))
?pie


geoloc<-factor(project$region,
               levels = c("northeast","northwest","southeast","southwest"),
               labels = c(1,2,3,4))

gender<-factor(project$sex,
               levels <- c("female","male"),
               labels = c(0,1)) #create lables for each category

smoker<-factor(project$smoker,
               levels <- c("no","yes"),
               labels = c(0,1))

smoker_dummy_0_N_1_Y<-dummy_cols(project$smoker)
gender_dummy_0_F_1_M<-dummy_cols(project$sex)
regions_dummy<-dummy_cols(project$region)

#-----------end of the descriptive stats------------------
#charges vs gender
 
summary(project$sex) 
tapply(project$charges, project$sex, psych::describe)

ggplot(data = project, aes(sex, charges)) + 
  geom_boxplot()+
  geom_boxplot(fill = c(6,8))+
  theme_classic()+
  ggtitle("Medical charges by gender")


#charges vs smoking

#Wilcoxon Sign-Rank Test - we will use this tes

#Tests for the difference between two related variables; 
#takes into account the magnitude and direction of difference

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

round(prop.table(table(project$sex, project$smoker),1),2)#sex perspective
        #no  yes
#female 0.83 0.17
#male   0.76 0.24

round(prop.table(table(project$sex, project$smoker),2),2) #smoker perspective

        #no  yes
#female 0.51 0.42
#male   0.49 0.58

# it will help us to count the odds 


#       no yes
#female 547 115
#male   517 159

round(table(project$sex, project$smoker),2)
ORMLSMK<-glm(project$sex~project$smoker,
             family = binomial(link="logit"))
summary(ORMLSMK)
#ods ratio with respect to male
exp(coef(ORMLSMK))


#-----------------------

table(project$region)
table(region_smoker)
table(project$smoker == "yes")

hist(underweight)
qqnorm(underweight)
qqline(underweight)#show the distribution of real quantiles vs the predicted ones 

## plot carges vs bmi with respect to BMI intervals
ggplot(data=project) +
  geom_point(mapping = aes(x=bmi, 
                           y=charges, 
                           color = BMIth, 
                           shape = smoker)) 


#BMI vs charges vs SMOKING

ggplot(data=project) +
  geom_point(mapping = aes(x=bmi, y=charges, color = smoker)) 

## plot charges vs age

a<-qplot(age, charges, data = project, color = smoker)# good one
a

# to test ------------
x <- ggplot(project, aes(x=age, y=charges)) +
  geom_jitter(color = "blue", alpha = 0.5) +
  theme_light()

y <- ggplot(project, aes(x=bmi, y=charges)) +
  geom_jitter(color = "green", alpha = 0.5) +
  theme_light()
y

#-----------plotting tests------
p <- plot_grid(a,aa,x,y) 
plotsageandbmi<- ggdraw() + draw_label("1. Correlation between Charges and Age / BMI", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))



plot_grid(hist1age,hist2bmi,hist3children,hist4charges,
  align = "h", 
  axis = "b", 
  nrow = 2,
  labels = "AUTO",
  label_size = 8,
  rel_widths = c(2, 2))

# end of the testing ------------

#--------------selecting the numeric variable to analisys using linear model-------

#corelation matrix for quantitative 
cormatquant<-project[c(1,3,4,7)]
cormatquant2<-rcorr(as.matrix(cormatquant))
cormatquant2


cormatqualit2<-rcorr(gender,project$charges,type=c("pearson"))
cormatqualit2

cormatqualit3<-rcorr(smoker,project$charges,type=c("pearson"))
cormatqualit3

?rcorr

#--------
cormatqualit4<-rcorr(BMIth,project$charges,type=c("spearman"))
cormatqualit4

rcorr(project$bmi,project$charges) # which approach to choose
#---------------

cormatqualit5<-rcorr(project$region,project$charges,type=c("spearman"))
cormatqualit5

?corr
?rcorr
# linear model without using values 

formulanumericvar<- as.formula("charges ~ age + children+bmi+smoker+region")
numericmodel<-lm(formulanumericvar,data=project)
hist(resid(numericmodel))
psych::describe((resid(numericmodel)))
#limit number of kids to categories

summary(formulanumericvar, data = project)
summary(numericmodel)

# linear model using values 
modelnumeric<-lm(project$charges~project$age+
                   BMIth+project$children+smoker_dummy_0_N_1_Y$.data+gender+geoloc)
summary(modelnumeric)
lm.beta(modelnumeric)#not working ;/
?lm.beta

plot(project$children,project$charges)

rstatix::kruskal_effsize(data, depVar~factor)

