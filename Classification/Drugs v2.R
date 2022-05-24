#####################################
##### EXPLORATORY DATA ANALYSIS #####
#####################################
##### Packages #####

library(dplyr)
library(readr)
library(ggplot2)
library(caret)
library(ggpubr)
library(rcompanion)
library(corrplot)

##### Dataset 1 #####

##Data import
data = read.csv("drugs_train.csv")

##Checking for NAs
colSums(is.na(data)) %>% 
  sort()
#There is no NAs
sapply(data, is.character)
# Id - to be dropped
data = data[,-1]
# Age, gender, education, country etc. - into factors
##Age
table(data$age)
data$age[data$age=="55-64"] <- "55+"
data$age[data$age == "65+"] <- "55+"

data$age = factor(data$age, levels = c("18-24",
                                       "25-34",
                                       "35-44",
                                       "45-54",
                                       "55+"),
                  ordered = TRUE)


table(data$age)
data$age <- droplevels(data$age)


##Gender
data$gender = factor(data$gender, levels = c("male",
                                             "female"),
                     ordered = TRUE)

##Education
table(data$education)
# We decided to merge people who left school at or before 18 years old into one group
data$education[data$education %in% c("Left school before 16 years",
                                     "Left school at 16 years",
                                     "Left school at 17 years",
                                     "Left school at 18 years")] <- "Left school at or before 18"

data$education = factor(data$education, levels = c("Left school at or before 18",
                                                   "Some college or university, no certificate or degree",
                                                   "Professional certificate/ diploma",
                                                   "University degree",
                                                   "Masters degree",
                                                   "Doctorate degree"),
                        ordered = TRUE)



data$education <- droplevels(data$education)

table(data$education)

##Country
table(data$country)

data$country = as.factor(data$country)
# Canada and Ireland as others because there is not enough observations
data$country[data$country %in% c("Canada",
                                 "Ireland")] <- "Other"

data$country <- droplevels(data$country)

##Ethnicity
table(data$ethnicity)
#Ethnicity is dropped because the sample is homogeneous
data = data[,-5]

##Consumption Alcohol
table(data$consumption_alcohol)

data$consumption_alcohol[data$consumption_alcohol %in% c("used over a decade ago",
                                                         "used in last decade")] <- "never used"

data$consumption_alcohol = factor(data$consumption_alcohol, levels = c("never used",
                                                                       "used in last year",
                                                                       "used in last month",
                                                                       "used in last week",
                                                                       "used in last day"),
                                  ordered = TRUE)

##Consumption Amphetamines
table(data$consumption_amphetamines)

data$consumption_amphetamines = factor(data$consumption_amphetamines, 
                                       levels = c("never used",
                                                  "used over a decade ago",
                                                  "used in last decade",
                                                  "used in last year",
                                                  "used in last month",
                                                  "used in last week",
                                                  "used in last day"),
                                       ordered = TRUE)

## Consumption Caffeine
table(data$consumption_caffeine)

# We decided to drop this variable because we believe that it is not impacting whether person consume cocaine
data = data[,-14]

##Consumption Cannabis
table(data$consumption_cannabis)
data$consumption_cannabis = factor(data$consumption_cannabis, 
                                   levels = c("never used",
                                              "used over a decade ago",
                                              "used in last decade",
                                              "used in last year",
                                              "used in last month",
                                              "used in last week",
                                              "used in last day"),
                                   ordered = TRUE)

##Consumption Chocolate
table(data$consumption_chocolate)
# We decided to drop this because it is random variable
data = data[,-15]

##Consumption Mushrooms
table(data$consumption_mushrooms)

# We decided to merge "used in last day" and "used in last week" into "used in last month" because it would be more representative
data$consumption_mushrooms[data$consumption_mushrooms %in% c("used in last day",
                                                             "used in last week")] <- "used in last month"

data$consumption_mushrooms = factor(data$consumption_mushrooms, levels = c("never used",
                                                                           "used over a decade ago",
                                                                           "used in last decade",
                                                                           "used in last year",
                                                                           "used in last month"),
                                    ordered = TRUE)

##Consumption Nicotine
table(data$consumption_nicotine) 

# We decided to downgrade the variable into three groups because we believe that it would be more representative
data$consumption_nicotine[data$consumption_nicotine %in% c("never used",
                                                           "used over a decade ago",
                                                           "used in last decade")] <- "no smoker"

data$consumption_nicotine[data$consumption_nicotine %in% c("used in last year",
                                                           "used in last month")] <- "not regular smoker"

data$consumption_nicotine[data$consumption_nicotine %in% c("used in last week",
                                                           "used in last day")] <- "regular smoker"

data$consumption_nicotine = factor(data$consumption_nicotine, levels = c("no smoker",
                                                                         "not regular smoker",
                                                                         "regular smoker"),
                                   ordered = TRUE)

##Consumption Cocaine
table(data$consumption_cocaine_last_month)

data$consumption_cocaine_last_month = factor(data$consumption_cocaine_last_month, levels = c("No",
                                                                                             "Yes"),
                                             ordered = TRUE)

## Importance of the Categorical Variables
for(i in names(which(sapply(data,is.factor)))){
  print(paste(i,round(chisq.test(data[i], data$consumption_cocaine_last_month, correct=FALSE)$p.value,4),
              cramerV(unlist(data[i]), unlist(data["consumption_cocaine_last_month"]))
  ))  
}
# The p.value of the "education" variable is above 5%, thus we decided to drop it.
data = data[,-3]

## Continuous Variables
names(which(sapply(data,is.numeric)))

for(i in names(which(sapply(data,is.numeric)))){
  print(paste(i, min(data[i]), max(data[i])))
}

for(i in names(which(sapply(data,is.numeric)))){
  hist(unlist(data[i]))
}
## Histograms of the variables seem to be reasonable

for (i in names(which(sapply(data,is.numeric)))){
  print(paste(i,round(t.test(as.formula(paste(i,"~","consumption_cocaine_last_month")),
                       data=data, alternative = "two.sided", var.equal = FALSE)$p.value,4)))
}

# Based on the t.test results we decided to drop "personality_extraversion".
data = data[,-5]

## Correlation testing
corrplot(corr, method = 'circle', order = 'alphabet', is.corr = FALSE)
corr = data[, sapply(data,is.numeric)]
corr = cor(corr)
#Based on the plot we can observe high correlation between impulsiveness and sensation. 
#So we decided to ommit one of them, impulsiveness
colnames(data)
data = data[,-8]

##### Dataset 2 #####

## Data import
data2 = read.csv("drugs_train.csv")

##Checking for NAs
colSums(is.na(data2)) %>% 
  sort()
#There is no NAs

## Checking types of variables
sapply(data2, is.character)
# Id - to be dropped
data2 = data2[,-1]
# Converting categorical variables into factors

##Age
# Two last groups merged into one to be more representative
table(data2$age)
data2$age[data2$age=="55-64"] <- "55+"
data2$age[data2$age == "65+"] <- "55+"

data2$age = factor(data2$age, levels = c("18-24",
                                        "25-34",
                                        "35-44",
                                        "45-54",
                                        "55+"),
                     ordered = TRUE)


table(data2$age)
data2$age <- droplevels(data2$age)

prop.table(table(data2$age, data2$consumption_cocaine_last_month),1)

##Gender
data2$gender = factor(data2$gender, levels = c("male",
                                             "female"),
                                  ordered = TRUE)

prop.table(table(data2$gender, data2$consumption_cocaine_last_month),1)

##Education
table(data2$education)
# We decided to merge people who left school at or before 18 years old into one group
data2$education[data2$education %in% c("Left school before 16 years",
                                     "Left school at 16 years",
                                     "Left school at 17 years",
                                     "Left school at 18 years")] <- "Left school at or before 18"

data2$education = factor(data2$education, levels = c("Left school at or before 18",
                                       "Some college or university, no certificate or degree",
                                       "Professional certificate/ diploma",
                                       "University degree",
                                       "Masters degree",
                                       "Doctorate degree"),
                  ordered = TRUE)



data2$education <- droplevels(data2$education)

table(data2$education)
prop.table(table(data2$education, data2$consumption_cocaine_last_month),1)

##Country
table(data2$country)

data2$country = as.factor(data2$country)

# Canada and Ireland as others because there is not enough observations
data2$country[data2$country %in% c("Canada",
                                     "Ireland")] <- "Other"

data2$country <- droplevels(data2$country)
prop.table(table(data2$country, data2$consumption_cocaine_last_month),1)

##Ethnicity
table(data2$ethnicity)
#Ethnicity is dropped because the sample is homogeneous
data2 = data2[,-5]

##Consumption Alcohol
table(data2$consumption_alcohol)
prop.table(table(data2$consumption_alcohol, data2$consumption_cocaine_last_month),1)

data2$consumption_alcohol[data2$consumption_alcohol %in% c("never used",
                                                           "used over a decade ago",
                                                           "used in last decade")] <- "never"

data2$consumption_alcohol[data2$consumption_alcohol %in% c("used in last year",
                                                           "used in last month")] <- "occasionally"

data2$consumption_alcohol[data2$consumption_alcohol %in% c("used in last week",
                                                           "used in last day")] <- "regularly"

data2$consumption_alcohol = factor(data2$consumption_alcohol, levels = c("never",
                                                                         "occasionally",
                                                                         "regularly"),
                                   ordered = TRUE)


##Consumption Amphetamines
table(data2$consumption_amphetamines)

prop.table(table(data2$consumption_amphetamines, data2$consumption_cocaine_last_month),1)

data2$consumption_amphetamines[data2$consumption_amphetamines %in% c("never used",
                                                         "used over a decade ago",
                                                         "used in last decade")] <- "never"

data2$consumption_amphetamines[data2$consumption_amphetamines %in% c("used in last year",
                                                         "used in last month")] <- "occasionally"

data2$consumption_amphetamines[data2$consumption_amphetamines %in% c("used in last week",
                                                         "used in last day")] <- "regularly"

data2$consumption_amphetamines = factor(data2$consumption_amphetamines, levels = c("never",
                                                                       "occasionally",
                                                                       "regularly"),
                                  ordered = TRUE)
## Consumption Caffeine
table(data2$consumption_caffeine)

# We decided to drop this variable because we believe that it is quite homogeneous 
# sample and we believe it is not impacting whether person consume cocaine

prop.table(table(data2$consumption_caffeine, data2$consumption_cocaine_last_month),1)
data2 = data2[,-14]

##Consumption Cannabis
table(data2$consumption_cannabis)
data2$consumption_cannabis[data2$consumption_cannabis %in% c("never used",
                                                                   "used over a decade ago",
                                                                   "used in last decade")] <- "never"

data2$consumption_cannabis[data2$consumption_cannabis %in% c("used in last year",
                                                                   "used in last month")] <- "occasionally"

data2$consumption_cannabis[data2$consumption_cannabis %in% c("used in last week",
                                                                   "used in last day")] <- "regularly"

data2$consumption_cannabis = factor(data2$consumption_cannabis, levels = c("never",
                                                                                 "occasionally",
                                                                                 "regularly"),
                                       ordered = TRUE)
prop.table(table(data2$consumption_cannabis, data2$consumption_cocaine_last_month),1)

##Consumption Chocolate
table(data2$consumption_chocolate)

prop.table(table(data2$consumption_chocolate, data2$consumption_cocaine_last_month),1)

data2$consumption_chocolate[data2$consumption_chocolate %in% c("never used",
                                                           "used over a decade ago",
                                                           "used in last decade")] <- "never"

data2$consumption_chocolate[data2$consumption_chocolate %in% c("used in last year",
                                                           "used in last month")] <- "occasionally"

data2$consumption_chocolate[data2$consumption_chocolate %in% c("used in last week",
                                                           "used in last day")] <- "regularly"

data2$consumption_chocolate = factor(data2$consumption_chocolate, levels = c("never",
                                                                         "occasionally",
                                                                         "regularly"),
                                   ordered = TRUE)
prop.table(table(data2$consumption_chocolate, data2$consumption_cocaine_last_month),1)


## Consumption Mushrooms
table(data2$consumption_mushrooms)

# We decided to merge "used in last day" and "used in last week" into "used in last month" because it would be more representative

data2$consumption_mushrooms[data2$consumption_mushrooms %in% c("never used",
                                                             "used over a decade ago",
                                                             "used in last decade")] <- "never"

data2$consumption_mushrooms[data2$consumption_mushrooms %in% c("used in last year",
                                                             "used in last month")] <- "occasionally"

data2$consumption_mushrooms[data2$consumption_mushrooms %in% c("used in last week",
                                                             "used in last day")] <- "regularly"

data2$consumption_mushrooms = factor(data2$consumption_mushrooms, levels = c("never",
                                                                           "occasionally",
                                                                           "regularly"),
                                    ordered = TRUE)

prop.table(table(data2$consumption_mushrooms, data2$consumption_cocaine_last_month),1)

##Consumption Nicotine
table(data2$consumption_nicotine) 

# We decided to downgrade the variable into three groups because we believe that it would be more representative
data2$consumption_nicotine[data2$consumption_nicotine %in% c("never used",
                                                             "used over a decade ago",
                                                             "used in last decade")] <- "never"

data2$consumption_nicotine[data2$consumption_nicotine %in% c("used in last year",
                                                             "used in last month")] <- "occasionally"

data2$consumption_nicotine[data2$consumption_nicotine %in% c("used in last week",
                                                             "used in last day")] <- "regularly"

data2$consumption_nicotine = factor(data2$consumption_nicotine, levels = c("never",
                                                                         "occasionally",
                                                                         "regularly"),
                                  ordered = TRUE)
prop.table(table(data2$consumption_nicotine, data2$consumption_cocaine_last_month),1)

##Consumption Cocaine
table(data2$consumption_cocaine_last_month)

data2$consumption_cocaine_last_month = factor(data2$consumption_cocaine_last_month, levels = c("No",
                                                                         "Yes"),
                                             ordered = TRUE)

#Checking importance of the variables
for(i in names(which(sapply(data2,is.factor)))){
  print(paste(i,round(chisq.test(data2[i], data2$consumption_cocaine_last_month, correct=FALSE)$p.value,4),
              cramerV(unlist(data2[i]), unlist(data2["consumption_cocaine_last_month"]))
              ))  
}
#Numeric variables seem to be okay there are no outliers and data is distributed more or less normally.

# Based on the Chi Sqrt test the only variable that it is not significant is education
colnames(data2)
data2 = data2[,-3]
data3 = data2

# We treat contentious variables as in dataset 1
data2 = data2[,-c(9,5)] 

##### Dataset 3 #####

numeric_var = names(which(sapply(data3,is.numeric)))
for(i in numeric_var){
  data3[i][data3[i] >= 67] <- "high"
  data3[i][data3[i] < 67 & data3[i] >= 33] <- "medium"
  data3[i][data3[i] < 33] <- "low"
}

data3$personality_extraversion[data3$personality_extraversion == 8.3] <- "low"
data3$personality_conscientiousness[data3$personality_conscientiousness == 8.1] <- "low"


sapply(data3[,sapply(data3,is.numeric)], function(x) factor(x, levels = c("low","medium","high"),
                                                            ordered = TRUE))

for(i in numeric_var){
  print(paste(i,round(chisq.test(data3[i], data3$consumption_cocaine_last_month, correct=FALSE)$p.value,4),
              cramerV(unlist(data3[i]), unlist(data3["consumption_cocaine_last_month"]))))  
}

# It was decided to drop
colnames(data3)
data3 = data3[,-c(5,9)]

for(i in colnames(data3)){
  print(table(data3[i]))
}

####################################
######### DATA MODELLING ###########
####################################

options(contrasts = c("contr.treatment",  # for non-ordinal factors
                      "contr.treatment")) # for ordinal factors
##### Logistic Regression #####

set.seed(9432398) # We specify random seed

# train control with cross-validation
ctrl_cv5 <- trainControl(method = "cv",
                          number = 5)
# Dataset 1

# The Accuracy scores seem to suggest that model might be reliable, however the Kappa scores draw opposite conclusions.
# Firstly, couple of interactions have been added. It is believed that the impact of the addictive substances might differ 
# depending on people's character traits. We decided to go with the following variables, cannabis consumption, amphetamines 
# consumption and mushrooms consumption, because they constitute so called soft and hard drugs. Thus, these three variables have been 
# mixed with each other and with variables that aim to describe character traits. Additionally, it was also believed that people who take 
# more drugs are more willing to take also cocaine. This is the reason that interactions for these variables have been added.

data_logit_train <- 
  train(consumption_cocaine_last_month ~ . + consumption_cannabis*consumption_amphetamines 
        + consumption_cannabis*consumption_mushrooms + consumption_amphetamines*consumption_mushrooms + consumption_cannabis*consumption_amphetamines*consumption_mushrooms 
        + personality_neuroticism*consumption_cannabis + personality_agreeableness*consumption_cannabis + personality_sensation*consumption_cannabis 
        + personality_conscientiousness*consumption_cannabis + personality_openness*consumption_cannabis 
        + personality_neuroticism*consumption_amphetamines + personality_agreeableness*consumption_amphetamines + personality_sensation*consumption_amphetamines 
        + personality_conscientiousness*consumption_amphetamines + personality_openness*consumption_amphetamines 
        + personality_neuroticism*consumption_mushrooms + personality_agreeableness*consumption_mushrooms + personality_sensation*consumption_mushrooms 
        + personality_conscientiousness*consumption_mushrooms + personality_openness*consumption_mushrooms,
        data = data,        
        method = "glm",
        family = "binomial"(link = "logit"),
        trControl = ctrl_cv5)
options(max.print=2000)
summary(data_logit_train)
data_logit_train$resample

# Based on the  results we can notice that the model is over fitted. To overcome these problems we needed to get rid of some variables Thus, it was decided
# to run it without any interactions and try with downgraded dataset.

data_logit_train <- 
  train(consumption_cocaine_last_month ~ .,        
        data = data,        
        method = "glm",
        family = "binomial"(link = "logit"),
        trControl = ctrl_cv5)

summary(data_logit_train)
data_logit_train$resample

# Based on the results we can omit not significant variables. Moreover, we noticed that the average 
# accuracy and average Kappa index have increased and AIC decreased

colnames(data)
data_logit_train <- 
  train(consumption_cocaine_last_month ~ .,        
        data = data[,-c(2,3,4,11)],        
        method = "glm",
        family = "binomial"(link = "logit"),
        trControl = ctrl_cv5)

summary(data_logit_train)
data_logit_train$resample

## Dataset 2
View(data2)
colnames(data2)
data2_logit_train <- 
  train(consumption_cocaine_last_month ~ . + consumption_cannabis*consumption_amphetamines 
        + consumption_cannabis*consumption_mushrooms + consumption_amphetamines*consumption_mushrooms + consumption_cannabis*consumption_amphetamines*consumption_mushrooms 
        + personality_neuroticism*consumption_cannabis + personality_agreeableness*consumption_cannabis + personality_sensation*consumption_cannabis 
        + personality_conscientiousness*consumption_cannabis + personality_openness*consumption_cannabis 
        + personality_neuroticism*consumption_amphetamines + personality_agreeableness*consumption_amphetamines + personality_sensation*consumption_amphetamines 
        + personality_conscientiousness*consumption_amphetamines + personality_openness*consumption_amphetamines 
        + personality_neuroticism*consumption_mushrooms + personality_agreeableness*consumption_mushrooms + personality_sensation*consumption_mushrooms 
        + personality_conscientiousness*consumption_mushrooms + personality_openness*consumption_mushrooms,
        data = data2,        
        method = "glm",
        family = "binomial"(link = "logit"),
        trControl = ctrl_cv5)

summary(data2_logit_train)
data2_logit_train$resample

# It was decide to drop
View(data2)
colnames(data2)
data2_logit_train <- 
  train(consumption_cocaine_last_month ~ . 
        + personality_neuroticism*consumption_cannabis + personality_agreeableness*consumption_cannabis + personality_sensation*consumption_cannabis 
        + personality_conscientiousness*consumption_cannabis + personality_openness*consumption_cannabis 
        + personality_neuroticism*consumption_amphetamines + personality_agreeableness*consumption_amphetamines + personality_sensation*consumption_amphetamines 
        + personality_conscientiousness*consumption_amphetamines + personality_openness*consumption_amphetamines 
        + personality_neuroticism*consumption_mushrooms + personality_agreeableness*consumption_mushrooms + personality_sensation*consumption_mushrooms 
        + personality_conscientiousness*consumption_mushrooms + personality_openness*consumption_mushrooms,
        data = data2[,-c(2,3)],        
        method = "glm",
        family = "binomial"(link = "logit"),
        trControl = ctrl_cv5)

# Drop interaction between cannabis and 
# Dataset 3
colnames(data)
data_logit_train <- 
  train(consumption_cocaine_last_month ~ .,
        data[,-c(1,2,3,4,5,6,7,8,9,11,12,13,4)],        
        method = "glm",
        family = "binomial"(link = "logit"),
        trControl = ctrl_cv5)

summary(data_logit_train)
data_logit_train$resample


