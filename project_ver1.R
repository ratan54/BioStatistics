project <- read.csv("D:\\Studia\\Magisterskie_HEBDA\\Biostatistics\\Project biostatistics\\insurance.csv")
install.packages("psych")
library(psych)
psych::describe(project$age) 
hist(project$age)
psych::describe(project$bmi)
hist(project$bmi)
psych::describe(project$charges)
hist(project$charges)
install.packages("fastDummies")# to create dummy variable
library(fastDummies)
gender<-factor(project$sex,
            levels <- c("female","male"),
            labels = c(0,1)) #create lables for each category
smoker<-factor(project$smoker,
               levels <- c("no","yes"),
               labels = c(0,1))
smoker_dummy_0_N_1_Y<-dummy_cols(project$smoker)
gender_dummy_0_F_1_M<-dummy_cols(project$sex)

summary(project)

round(prop.table(table(gender, smoker)),2)

# ------------------------
# treshold for BMI factor TO DO 
BMIth<-cut(project$bmi, breaks = c(0, 18.5, 25, 30, 100), labels = c("underweight",
                  "normal weight","overweight", "obese"), right = TRUE)

sum(table(BMIth))
underweight<-subset(project$bmi, BMIth == "underweight")#ekstract the interval 
#data from the data set according the name of the interval
normalweight<-subset(project$bmi, BMIth == "normal weight")
table(normalweight)
table(underweight)
mean(underweight)
table(BMIth)

# source https://www.healthaffairs.org/doi/full/10.1377/hlthaff.W4.480
#-----------------------

table(project$region)
table(region_smoker)
table(project$smoker == "yes")

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

hist(underweight)
qqnorm(underweight)
qqline(underweight)#show the distribution of real quantiles vs the predicted ones 
install.packages("tidyverse")
library(tidyverse) #cannot install it ? 

## plot carges vs bmi with respect to BMI intervals
ggplot(data=project) +
  geom_point(mapping = aes(x=bmi, y=charges, color = BMIth))

## plot charges vs age
ggplot(data=project, mapping = aes(x=age, y=charges))+
    geom_point(alpha=1/5)

install.packages("dplyr")
library(dplyr)
install.packages("Hmisc")
library(Hmisc)
install.packages("cowplot")
library(cowplot)

install.packages("WVPlots")
library(WVPlots)

x <- ggplot(project, aes(x=age, y=charges)) +
  geom_jitter(color = "blue", alpha = 0.5) +
  theme_light()

y <- ggplot(project, aes(x=bmi, y=charges)) +
  geom_jitter(color = "green", alpha = 0.5) +
  theme_light()

p <- plot_grid(x, y) 
plotsageandbmi<- ggdraw() + draw_label("1. Correlation between Charges and Age / BMI", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))


ggplot(data=project)+
  geom_point(mapping = aes(x=BMIth == "obese", y=charges))

plot(BMIth == "obese", project$charges, na.rm = TRUE))

cbPalette #gray color scale  to use for scatter plot for AGE VS charges



