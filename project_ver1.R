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

round(prop.table(table(gender, smoker)),2)

# ------------------------
# treshold for BMI factor TO DO 
BMIth<-cut(project$bmi, breaks = c(0, 18.5, 25, 30, 100), labels = c("underweight",
                  "normal weight","overweight", "obese"), right = FALSE)

sum(table(BMIth))
underweight<-subset(project$bmi, BMIth == "underweight") #ekstract the interval 
#data from the data set according the name of the interval 
table(underweight)
mean(underweight)


# source https://www.healthaffairs.org/doi/full/10.1377/hlthaff.W4.480
#-----------------------

table(project$region)
table(region_smoker)
table(project$smoker == "yes")
