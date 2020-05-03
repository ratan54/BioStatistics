project <- read.csv("D:\\Kozminski Data\\Second Sem\\Biostatistics\\BioStat_Project\\insurance.csv")

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

install.packages("lsr")
library(lsr)

install.packages("lm.beta")
library(lm.beta)

install.packages("QuantPsyc")
library(QuantPsyc)

install.packages("ggpubr")
library(ggpubr)

install.packages("car")
library(car)

install.packages("rcompanion")
library(rcompanion)

summary(project)

Medical_Charges<- project$charges

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

hist3children<-ggplot(Child_dummies) + 
  geom_bar(aes(x = Child_dummies$.data), fill = "green")+
  coord_cartesian(ylim = c(0, 1200))+
  ggtitle("Children Distribution")
                     
hist3children


psych::describe(Medical_Charges)

hist4charges<-(ggplot(project, aes(x=charges)) + 
                       geom_histogram(color="black", fill="green") + 
                 coord_cartesian(xlim = c(0, 60000), ylim = c(0, 250))+
                       ggtitle("Histogram for the Charges"))
hist4charges
hist(log(Medical_Charges))
lnCharge<- log(Medical_Charges)
hist(lnCharge)
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

BMIth<-cut(project$bmi, breaks = c(0, 18.5, 25, 30, 100),
           labels = c("underweight","normal weight","overweight", "obese"), right = TRUE)

ChildTh<- cut(project$children,breaks = c(-1,2,6),
              labels=c("=<2_children", "3_or_more"),right =TRUE)


Child_dummies
boxplot(ChildTh)

ChildTh

project$children
str(ChildTh)
?pie
plot(ChildTh)
plot(project$children)
sum(project$children)

#------------Dummy Variables----------------------------#

smoker_dummy_0_N_1_Y<-dummy_cols(project$smoker)
gender_dummy_0_F_1_M<-dummy_cols(project$sex)
regions_dummy<-dummy_cols(project$region)
Child_dummies<-dummy_cols(ChildTh)
BMIth_dummies<-dummy_cols(BMIth)


#-----------end of the descriptive stats------------------#
#----------Charges vs Number of child --------------------#

tapply(Medical_Charges,ChildTh,psych::describe)

ggplot(data=project,aes(ChildTh,Medical_Charges)) +
  geom_boxplot(fill = c(3,4))+
  ggtitle("Charges w.r.t no. of children")
sum(Child_dummies$`.data_=<2_children`)
sum(Child_dummies$.data_3_or_more)
  
cor.test(Medical_Charges,Child_dummies$.data_3_or_more,method = "spearman")
#charges vs gender
 
summary(project$sex) 
tapply(Medical_Charges, project$sex, psych::describe)

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
#charges vs BMIth
ggplot(data = project, aes(BMIth, charges)) + 
  geom_boxplot()+
  geom_boxplot(fill = c(1:4))+
  theme_classic()+
  ggtitle("Medical charges by BMI")

#charges vs region 

ggplot(data = project,aes(region,charges)) + 
  geom_boxplot(fill = c(6:9)) +
  theme_classic() + 
  ggtitle("Medical Charges per Region")

psych::describe(Medical_Charges)
hist(Medical_Charges,
     main="Histogram for the Charges", 
     xlab="Charges",
     col = 3)

round(prop.table(table(project$sex, project$smoker),1),2)#sex perspective
        #no  yes
#female 0.83 0.17
#male   0.76 0.24

probsexprs<-table(gender_dummy_0_F_1_M$.data, smoker_dummy_0_N_1_Y$.data)
round(prop.table(probsexprs, 1),2)


cramersV(probsexprs) # should be used for cross tabs 2x2 or more 
#2step it show how it is statisticaly significant 
chisq.test(probsexprs, simulate.p.value) # for more than two groups 
phi(probsexprs)# phi is for only two groups

?chisq.test

round(prop.table(table(project$sex, project$smoker),2),2)


#sex perspective
#         no  yes
# female 0.83 0.17
# male   0.76 0.24

#smoker perspective

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
cormatquant<-project[c(1,7)]
cormatquant2<-rcorr(as.matrix(cormatquant))
cormatquant2

cormatqualit2<-rcorr(gender,Medical_Charges,type=c("spearman"))
cormatqualit2

cormatqualit3<-rcorr(smoker,Medical_Charges,type=c("spearman"))
cormatqualit3

cormatqualit4<-rcorr(BMIth,Medical_Charges,type=c("spearman"))
cormatqualit4

cormatqualit5<-rcorr(project$region,Medical_Charges,type=c("spearman"))
cormatqualit5

cormatqualit6<-rcorr(BMIth, Medical_Charges, type = c("spearman"))
cormatqualit6

cormatqualit7<-rcorr(ChildTh, Medical_Charges, type = c("spearman"))
cormatqualit7

#------------------ normal distribution check#-------

shapiro.test(project$bmi)
shapiro.test(Medical_Charges)
shapiro.test(log(Medical_Charges))# need to put a log on the 
ggqqplot(project$bmi)
ggqqplot(Medical_Charges)
ggqqplot(log(Medical_Charges))

#---------------------- comparison of median betwen charges and smoking-nonparametric approach ---

describeBy(Medical_Charges,smoker) # non parametric approach - diffrent sample size, median approach 

wilcox.test(Medical_Charges ~ smoker,
            data=project, conf.int = TRUE)#Two-sample Mann–Whitney U test example
#With the Wilcoxon test, an obtained W is significant if it is LESS than or EQUAL to
#the critical value. 

?wilcox.test()
#As the p-value is less than the significance level 0.05, 
#we can conclude that there are significant differences between the treatment groups
wilcoxonRG(x = Medical_Charges, g=smoker, conf = 0.95, verbose = TRUE)
?wilcoxonRG
# rejecting the null hipothesis  = true location shift is not equal to 0, with p < 5 %

#difference in location  
# -23922.22


#----------------
describeBy(Medical_Charges,BMIth)

kruskal.test(Medical_Charges~BMIth)#Kruskal Wallis base od the eta squared H stats 
(16.956-4+1)/(1338-4) 
?kruskal_test

#significant difference between groups,
#but we don’t know which pairs of groups are different.
# weak effect 0.01046177 # percentage  of variance of the dependenta variable  explained by the independent variable

## plot carges vs bmi with respect to BMI intervals
ggplot(data=project) +
  geom_point(mapping = aes(x=bmi, 
                           y=charges, 
                           color = BMIth, 
                           shape = smoker)) 

#BMI vs charges vs SMOKING

ggplot(data=project) +
  geom_point(mapping = aes(x=bmi, y=charges, color = smoker)) 

psych::describe(project$charges)

#-------------------------------

# linear model without using values 


lm1_charges<-lm(Medical_Charges~project$age+
                  smoker_dummy_0_N_1_Y$.data+
                  Child_dummies$.data+
                  BMIth_dummies$.data)
summary(lm1_charges)
#---description of the residual of the model #----
hist(resid(lm1_charges),ylim = c(0,650))
psych::describe((resid(lm1_charges)))
lm.beta(lm1_charges)

resid(lm1_charges)

plot(lm1_charges)

#--------------
lm2_logcharges<-lm(log(Medical_Charges)~project$age+
                  smoker_dummy_0_N_1_Y$.data+
                  Child_dummies$.data+
                  BMIth_dummies$.data)
summary(lm2_logcharges)
lm.beta(lm2_logcharges)

(exp(0.07)-1)*100

#---description of the residual of the model #----
hist(resid(lm2_logcharges), ylim = c(0,500))
psych::describe(lm2_logcharges)
plot(lm2_logcharges)


d<-density(lm2_logcharges[['residuals']])
plot(d,main='Residual Linear Model Plot',xlab='Residual value')



# linear model using values 
modelnumeric<-lm(Medical_Charges~project$age+
                   BMIth+project$children+smoker_dummy_0_N_1_Y$.data+gender+geoloc)
summary(modelnumeric)
lm.beta(modelnumeric)

lm(lnCharge~smoker_dummy_0_N_1_Y$.data_yes+project$age)
lm(lnCharge~project$age)
lm(Medical_Charges~smoker_dummy_0_N_1_Y$.data_yes+project$age)

abline(lm(Medical_Charges~smoker_dummy_0_N_1_Y$.data_yes),col="red")


plot(project$children,Medical_Charges)

rstatix::kruskal_effsize(data, depVar~factor)

