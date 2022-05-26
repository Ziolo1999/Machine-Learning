#####################################
##    EXPLORATORY DATA ANALYSIS    ##
#####################################
#####      Packages             ##########

library(dplyr)
library(readr)
library(ggplot2)
library(caret)
library(ggpubr)
library(rcompanion)
library(corrplot)
library(smotefamily)
library(remotes)
library(ROSE)

#####      Dataset 1            ###########
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
prop.table(table(data$ethnicity, data2$consumption_cocaine_last_month),1)
#Ethnicity is dropped because the sample is homogeneous
data$ethnicity = as.factor(data$ethnicity)

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
prop.table(table(data$consumption_caffeine, data2$consumption_cocaine_last_month),1)

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
prop.table(table(data$consumption_chocolate, data$consumption_cocaine_last_month),1)
# We decided to drop this because it is random variable
colnames(data)
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
colnames(data)
# Based on the t.test results we decided to drop "personality_extraversion".
data = data[,-5]

## Correlation testing
corrplot(cor(data[,sapply(data,is.numeric)]), method = 'circle', 
         order = 'alphabet', is.corr = FALSE)

# Based on the plot we can observe high correlation between impulsiveness and sensation. 
# So we decided to ommit one of them, impulsiveness
colnames(data)
data = data[,-8] # Zagregowac dwie zmienne

#####      Dataset 2            ##########

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

# We decided to drop this variable because we believe that it is quite homogeneous 
# sample and we believe it is not impacting whether person consume cocaine

colnames(data2)

data2 = data2[,-15]


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
data2 = data2[,-c(5,9)] 

#####      Dataset 3            ##########

numeric_var = names(which(sapply(data3,is.numeric)))
num = sapply(data3,is.numeric)
for(i in numeric_var){
  data3[i][data3[i] >= 67] <- "high"
  data3[i][data3[i] < 67 & data3[i] >= 33] <- "medium"
  data3[i][data3[i] < 33] <- "low"
}

data3$personality_extraversion[data3$personality_extraversion == 8.3] <- "low"
data3$personality_conscientiousness[data3$personality_conscientiousness == 8.1] <- "low"


data3[,num] = lapply(data3[,num], function(x) factor(x, levels = c("low","medium","high"),
                                                            ordered = TRUE))

for(i in numeric_var){
  print(paste(i,round(chisq.test(data3[i], data3$consumption_cocaine_last_month, correct=FALSE)$p.value,4),
              cramerV(unlist(data3[i]), unlist(data3["consumption_cocaine_last_month"]))))  
}

# It was decided to drop
colnames(data3)
data3 = data3[,-c(6,8)]
head(data3)
for(i in colnames(data3)){
  print(table(data3[i]))
}

#####################################
##         DATA MODELLING          ##
#####################################

options(contrasts = c("contr.treatment",  # for non-ordinal factors
                      "contr.treatment")) # for ordinal factors
#####      Logistic Regression  ##########

set.seed(9432398) # We specify random seed

# Train control with cross-validation
ctrl_cv5 <- trainControl(method = "repeatedcv",
                         number = 5,
                         classProbs = TRUE,
                         summaryFunction = twoClassSummary,
                         repeats = 3)

# Dataset 1

# Firstly, couple of interactions have been added. It is believed that the impact of the addictive substances might differ 
# depending on people's character traits. We decided to go with the following variables, cannabis consumption, amphetamines 
# consumption and mushrooms consumption, because they constitute so called soft and hard drugs. Thus, these three variables have been 
# mixed with each other and with variables that aim to describe character traits. Additionally, it was also believed that people who take 
# more drugs are more willing to take also cocaine. This is the reason that interactions for these variables have been added.

data_logit_train1 <- 
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
        metric = "Spec",
        trControl = ctrl_cv5)

options(max.print=2000)
summary(data_logit_train1)
data_logit_train1$resample

# Based on the  results we can notice that the model fits well. However some 
# variables are not significant. So we decided to get rid of them step by step.
# When we have got rid of the interaction between all variables the significance 
# of the variables strongly decreased. So we decide to erase all interactions.

data_logit_train2 <- 
  train(consumption_cocaine_last_month ~ . ,
        data = data,        
        method = "glm",
        family = "binomial"(link = "logit"),
        metric = "Spec",
        trControl = ctrl_cv5)

summary(data_logit_train2)

# Based on the results we can omit not significant variables. Moreover, we noticed that the average 
# accuracy and average Kappa index have increased and AIC decreased

data_logit_train3 <- 
  train(consumption_cocaine_last_month ~ .,        
        data = data[,-c(2,3,4,11)],        
        method = "glm",
        family = "binomial"(link = "logit"),
        metric = "Spec",
        trControl = ctrl_cv5)

summary(data_logit_train3)

## Dataset 2
# In the second dataset we downgraded all our categorical variables in order to simplify our model.
# Each of the variable has three levels regularly, occasionally, never. The first step is again extending a model by
# adding similar interactions 

data2_logit_train1 <- 
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
        metric = "Spec",
        trControl = ctrl_cv5)

summary(data2_logit_train1)

# In this case our model is under fitting, so again we needed to try to significantly decrease number of our variables. 
# This time we decided to go with different approach. Firstly, we start looking for single variables that might be 
# bad predictors. Then we decided to omit interactions with soft drugs, leaving only hard ones.

colnames(data2)
data2_logit_train2 <- 
  train(consumption_cocaine_last_month ~ . 

        + personality_neuroticism*consumption_amphetamines + personality_agreeableness*consumption_amphetamines + personality_sensation*consumption_amphetamines 
        + personality_openness*consumption_amphetamines ,

        data = data2[,-c(2,3)],        
        method = "glm",
        family = "binomial"(link = "logit"),
        metric = "Spec",
        trControl = ctrl_cv5)

summary(data2_logit_train2)

# The last step was to get rid of all insignificant interactions.

data2_logit_train3 <- 
  train(consumption_cocaine_last_month ~ . ,
        data = data2[,-c(2,3,4,13)],        
        method = "glm",
        family = "binomial"(link = "logit"),
        metric = "Spec",
        trControl = ctrl_cv5)

summary(data2_logit_train3)

# Dataset 3

data3_logit_train1 <- 
  train(consumption_cocaine_last_month ~ . + consumption_cannabis*consumption_amphetamines 
        + consumption_cannabis*consumption_mushrooms + consumption_amphetamines*consumption_mushrooms + consumption_cannabis*consumption_amphetamines*consumption_mushrooms 
        + personality_neuroticism*consumption_cannabis + personality_agreeableness*consumption_cannabis + personality_sensation*consumption_cannabis 
        + personality_extraversion*consumption_cannabis + personality_impulsiveness*consumption_cannabis 
        + personality_neuroticism*consumption_amphetamines + personality_agreeableness*consumption_amphetamines + personality_sensation*consumption_amphetamines 
        + personality_extraversion*consumption_amphetamines + personality_impulsiveness*consumption_amphetamines 
        + personality_neuroticism*consumption_mushrooms + personality_agreeableness*consumption_mushrooms + personality_sensation*consumption_mushrooms 
        + personality_extraversion*consumption_mushrooms + personality_impulsiveness*consumption_mushrooms,
        data = data3,        
        method = "glm",
        family = "binomial",
        metric = "Spec",
        trControl = ctrl_cv5)

summary(data3_logit_train1)

# We have noticed that only several variables are significant. The situation is similar as in case of the dataset 2. 
# Thus, we decided to omit interactions with soft drugs, leaving only hard ones.

data3_logit_train2 <- 
  train(consumption_cocaine_last_month ~ . 
        + personality_neuroticism*consumption_amphetamines + personality_agreeableness*consumption_amphetamines + personality_sensation*consumption_amphetamines 
        + personality_extraversion*consumption_amphetamines + personality_impulsiveness*consumption_amphetamines,
        data = data3,        
        method = "glm",
        family = "binomial",
        metric = "Spec",
        trControl = ctrl_cv5)

summary(data3_logit_train2)

# Model without any interactions

data3_logit_train3 <- 
  train(consumption_cocaine_last_month ~ .,
        data3,        
        method = "glm",
        family = "binomial"(link = "logit"),
        metric = "Spec",
        trControl = ctrl_cv5)



# Summary of the models

logit_summary = rbind(c("Dataset 1",sapply(data_logit_train1$resample,mean), 
              mean((data_logit_train1$resample$Sens+data_logit_train1$resample$Spec)/2)),
            c("Dataset 1",sapply(data_logit_train2$resample, mean),
              mean((data_logit_train2$resample$Sens+data_logit_train2$resample$Spec)/2)),
            c("Dataset 1",sapply(data_logit_train3$resample, mean),
              mean((data_logit_train3$resample$Sens+data_logit_train3$resample$Spec)/2)),
            c("Dataset 2",sapply(data2_logit_train1$resample,mean), 
              mean((data2_logit_train1$resample$Sens+data2_logit_train1$resample$Spec)/2)),
            c("Dataset 2",sapply(data2_logit_train2$resample, mean),
              mean((data2_logit_train2$resample$Sens+data2_logit_train2$resample$Spec)/2)),
            c("Dataset 2",sapply(data2_logit_train3$resample, mean),
              mean((data2_logit_train3$resample$Sens+data2_logit_train3$resample$Spec)/2)),
            c("Dataset 3",sapply(data3_logit_train1$resample,mean), 
              mean((data3_logit_train1$resample$Sens+data3_logit_train1$resample$Spec)/2)),
            c("Dataset 3",sapply(data3_logit_train2$resample, mean),
              mean((data3_logit_train2$resample$Sens+data3_logit_train2$resample$Spec)/2)),
            c("Dataset 3",sapply(data3_logit_train3$resample, mean),
              mean((data3_logit_train3$resample$Sens+data3_logit_train3$resample$Spec)/2)))[,-5]
colnames(logit_summary)[c(1,5)] = c("Dataset", "Balanced Accuracy")
logit_summary

# We can notice that all datasets are characterized with similar Balanced Accuracy which is also relatively low.
# It was decided to take dataset 3 without any interactions. The decision was  motivated by the fact
# that it is the most simple one have similar accuracy. 

final_logit_train = data3_logit_train3

#####      KNN                  ##########

# One of the biggest advantage of the KNN algorithm is its simplicity. It is quite intuitive method which simply finds
# K nearest neighbours. However, in our case this algorithm might not be the best one because our dataset is strongly
# imbalanced. It might result in getting  less common class wrongly classified.

k_possible = data.frame(k=seq(1,15,1))

# Dataset 1
data_knn_train <- 
  train(consumption_cocaine_last_month ~ .,
        data,        
        method = "knn",
        metric = "Spec",
        trControl = ctrl_cv5,
        tuneGrid = k_possible,
        preProcess = c("range"))

data_knn_train$resample
summary((data_knn_train$resample$Sens+data_knn_train$resample$Spec)/2)
plot(data_knn_train)

# Dataset 2

data2_knn_train <- 
  train(consumption_cocaine_last_month ~ .,
        data2,        
        method = "knn",
        metric = "Spec",
        trControl = ctrl_cv5,
        tuneGrid = k_possible,
        preProcess = c("range"))

data2_knn_train$finalModel
data2_knn_train$resample

summary((data2_knn_train$resample$Sens+data2_knn_train$resample$Spec)/2)
plot(data2_knn_train)

# Dataset 3
data3_knn_train <- 
  train(consumption_cocaine_last_month ~ .,
        data3,        
        method = "knn",
        metric = "Spec",
        trControl = ctrl_cv5,
        tuneGrid = k_possible,
        preProcess = c("range"))

summary((data3_knn_train$resample$Sens+data3_knn_train$resample$Spec)/2)
plot(data_knn_train)

# The KNN results were as we predicted before the analysis. It means that the specifity of the model was extremely low.
# The plots suggest that we should take as low Ks as possible to maximize Specifity. We observed that 
# trade of between ROC and Spec and Sens and Spec are relatively low thus we decided to take 2 neighbours.
# This conclusion is not surprising due to the fact that we have extremely low observations who consume cocaine.

## KNN Summary

knn_summary = rbind(c("Dataset 1",sapply(data_knn_train$resample,mean), 
                        mean((data_knn_train$resample$Sens+data_knn_train$resample$Spec)/2)),
                      c("Dataset 2",sapply(data2_knn_train$resample, mean),
                        mean((data2_knn_train$resample$Sens+data2_knn_train$resample$Spec)/2)),
                      c("Dataset 3",sapply(data3_knn_train$resample, mean),
                        mean((data3_knn_train$resample$Sens+data3_knn_train$resample$Spec)/2)))[,-5]

colnames(knn_summary)[c(1,5)] = c("Dataset", "Balanced Accuracy")
knn_summary

# The final decision was to go with the second dataset because it is more simple then the first one and 
# has similar Specifity.

final_knn_train = data3_knn_train

#####      SVM                  ##########

parametersC <- data.frame(C = 50)

# Dataset 1

data_svm_train <- 
  train(consumption_cocaine_last_month ~ .,
        data,        
        method = "svmRadial",
        metric = "Spec",
        trControl = ctrl_cv5)

# Dataset 2

data2_svm_train <- 
  train(consumption_cocaine_last_month ~ .,
        data2,        
        method = "svmRadial",
        metric = "Spec",
        trControl = ctrl_cv5)

# Dataset 3

data3_svm_train <- 
  train(consumption_cocaine_last_month ~ .,
        data3,        
        method = "svmRadial",
        metric = "Spec",
        trControl = ctrl_cv5)

data3_svm_train$resample

## SVM Summary

svm_summary = rbind(c("Dataset 1",sapply(data_svm_train$resample,mean), 
                      mean((data_svm_train$resample$Sens+data_svm_train$resample$Spec)/2)),
                    c("Dataset 2",sapply(data2_svm_train$resample, mean),
                      mean((data2_svm_train$resample$Sens+data2_svm_train$resample$Spec)/2)),
                    c("Dataset 3",sapply(data3_svm_train$resample, mean),
                      mean((data3_svm_train$resample$Sens+data3_svm_train$resample$Spec)/2)))[,-5]

colnames(svm_summary)[c(1,5)] = c("Dataset", "Balanced Accuracy")
svm_summary

# Here we did not decide to choose any model because they wrongly predict. 

#####      Random Forest        #####

#Dataset 1

data_rf_train <- 
  train(consumption_cocaine_last_month ~ .,
        data,        
        method = "rf",
        metric = "Spec",
        trControl = ctrl_cv5)

data_rf_train$results
plot(data_rf_train)

# Dataset 2

data2_rf_train <- 
  train(consumption_cocaine_last_month ~ .,
        data2,        
        method = "rf",
        metric = "Spec",
        trControl = ctrl_cv5)

data2_rf_train$results
plot(data2_rf_train)

# Dataset 3

data3_rf_train <- 
  train(consumption_cocaine_last_month ~ .,
        data3,        
        method = "rf",
        metric = "Spec",
        trControl = ctrl_cv5)

data3_rf_train$results
plot(data3_rf_train)

# Modelling Summary
rf_summary = rbind(c("Dataset 1",sapply(data_rf_train$resample,mean), 
                     mean((data_rf_train$resample$Sens+data_rf_train$resample$Spec)/2)),
                   c("Dataset 2",sapply(data2_rf_train$resample, mean),
                     mean((data2_rf_train$resample$Sens+data2_rf_train$resample$Spec)/2)),
                   c("Dataset 3",sapply(data3_rf_train$resample, mean),
                     mean((data3_rf_train$resample$Sens+data3_rf_train$resample$Spec)/2)))[,-5]

colnames(rf_summary)[c(1,5)] = c("Dataset", "Balanced Accuracy")
rf_summary

# We decided to go on with the third dataset because it is simple and It has high balanced accuracy.
final_rf_train = data3_rf_train

#####################################
##         Data Balancing          ##
#####################################

# Firstly, it was noted that our dependent variable is strongly imbalanced. 
# In our dataset we have 127 people who consumed cocaine and 1373 who not. 
# This characteristic might have a negative impact on our modelling because
# our classifier may get biased towards the prediction and be inaccurate.
# To solve this problem we decided to build under-sampled dataset because 
# most of the over-sampling algorithms are used only for continuous dgata. 
# We also decided to do it manually by erasing random observations.  
# We still wanted to remain relative proportions of having more cases of people who did
# not consume cocaine.

#####      Downsampling         ##########
## Dataset 1
No = which(data$consumption_cocaine_last_month=="No")
Yes = which(data$consumption_cocaine_last_month=="Yes")

No.dwng = sample(No, length(Yes))
data.dwng = data[c(No.dwng, Yes),]
table(data.dwng$consumption_cocaine_last_month)

## Dataset 2
No = which(data2$consumption_cocaine_last_month=="No")
Yes = which(data2$consumption_cocaine_last_month=="Yes")

No.dwng = sample(No, length(Yes))
data2.dwng = data2[c(No.dwng, Yes),]
table(data2.dwng$consumption_cocaine_last_month)

## Dataset 3
No = which(data3$consumption_cocaine_last_month=="No")
Yes = which(data3$consumption_cocaine_last_month=="Yes")

No.dwng = sample(No, length(Yes))
data3.dwng = data3[c(No.dwng, Yes),]
table(data3.dwng$consumption_cocaine_last_month)

#####      Logistic Regression  ##########

# Dataset 1
# As data was reduced it was decided to decrease number of folds.
ctrl_cv5 <- trainControl(method = "repeatedcv",
                         number = 3,
                         classProbs = TRUE,
                         summaryFunction = twoClassSummary,
                         repeats = 2)

# We start our analysis as we did for the first time.

data_logit_train1 <- 
  train(consumption_cocaine_last_month ~ . + consumption_cannabis*consumption_amphetamines 
        + consumption_cannabis*consumption_mushrooms + consumption_amphetamines*consumption_mushrooms + consumption_cannabis*consumption_amphetamines*consumption_mushrooms 
        + personality_neuroticism*consumption_cannabis + personality_agreeableness*consumption_cannabis + personality_sensation*consumption_cannabis 
        + personality_conscientiousness*consumption_cannabis + personality_openness*consumption_cannabis 
        + personality_neuroticism*consumption_amphetamines + personality_agreeableness*consumption_amphetamines + personality_sensation*consumption_amphetamines 
        + personality_conscientiousness*consumption_amphetamines + personality_openness*consumption_amphetamines 
        + personality_neuroticism*consumption_mushrooms + personality_agreeableness*consumption_mushrooms + personality_sensation*consumption_mushrooms 
        + personality_conscientiousness*consumption_mushrooms + personality_openness*consumption_mushrooms,
        data = data.dwng,        
        method = "glm",
        family = "binomial"(link = "logit"),
        metric = "ROC",
        trControl = ctrl_cv5)
colnames(data.dwng)
summary(data_logit_train1)

# The model is quite odd so we decided to get rid of interactions.

data_logit_train2 <- 
  train(consumption_cocaine_last_month ~ .,        
        data = data.dwng,        
        method = "glm",
        family = "binomial"(link = "logit"),
        metric = "ROC",
        trControl = ctrl_cv5)

summary(data_logit_train2)

# By comparing this modelling to the previous one we noticed that Sensitivity 
# has slightly decreased, however Specifity significantly increased. It gave us
# an impression that training our data on balanced data might be more accurate.
# Our next step was to get rid of unsignificant variables.

data_logit_train3 <- 
  train(consumption_cocaine_last_month ~ .,        
        data = data.dwng[,-c(2,3,4,11)],        
        method = "glm",
        family = "binomial"(link = "logit"),
        metric = "ROC",
        trControl = ctrl_cv5)

summary(data_logit_train3)

## Dataset 2
# In the second dataset we downgraded all our categorical variables in order to simplify our model.
# Each of the variable has three levels regularly, occasionally, never. The first step is again extending a model by
# adding similar interactions 

data2_logit_train1 <- 
  train(consumption_cocaine_last_month ~ . + consumption_cannabis*consumption_amphetamines 
        + consumption_cannabis*consumption_mushrooms + consumption_amphetamines*consumption_mushrooms + consumption_cannabis*consumption_amphetamines*consumption_mushrooms 
        + personality_neuroticism*consumption_cannabis + personality_agreeableness*consumption_cannabis + personality_sensation*consumption_cannabis 
        + personality_conscientiousness*consumption_cannabis + personality_openness*consumption_cannabis 
        + personality_neuroticism*consumption_amphetamines + personality_agreeableness*consumption_amphetamines + personality_sensation*consumption_amphetamines 
        + personality_conscientiousness*consumption_amphetamines + personality_openness*consumption_amphetamines 
        + personality_neuroticism*consumption_mushrooms + personality_agreeableness*consumption_mushrooms + personality_sensation*consumption_mushrooms 
        + personality_conscientiousness*consumption_mushrooms + personality_openness*consumption_mushrooms,
        data = data2.dwng,        
        method = "glm",
        family = "binomial"(link = "logit"),
        metric = "ROC",
        trControl = ctrl_cv5)

summary(data2_logit_train1)

# In this case our model is under fitting, so again we needed to try to significantly decrease number of our variables. 
# This time we decided to go with different approach. Firstly, we start looking for single variables that might be 
# bad predictors. Then we decided to omit interactions with soft drugs, leaving only hard ones.

data2_logit_train2 <- 
  train(consumption_cocaine_last_month ~ . 
        
        + personality_neuroticism*consumption_amphetamines + personality_agreeableness*consumption_amphetamines + personality_sensation*consumption_amphetamines 
        + personality_openness*consumption_amphetamines ,
        
        data = data2.dwng[,-c(2,3)],        
        method = "glm",
        family = "binomial"(link = "logit"),
        metric = "ROC",
        trControl = ctrl_cv5)

summary(data2_logit_train2)

# The last step was to get rid of all insignificant interactions.

data2_logit_train3 <- 
  train(consumption_cocaine_last_month ~ . ,
        data = data2.dwng[,-c(2,3,4,13)],        
        method = "glm",
        family = "binomial"(link = "logit"),
        metric = "ROC",
        trControl = ctrl_cv5)

summary(data2_logit_train3)

# Dataset 3

data3_logit_train1 <- 
  train(consumption_cocaine_last_month ~ . + consumption_cannabis*consumption_amphetamines 
        + consumption_cannabis*consumption_mushrooms + consumption_amphetamines*consumption_mushrooms + consumption_cannabis*consumption_amphetamines*consumption_mushrooms 
        + personality_neuroticism*consumption_cannabis + personality_agreeableness*consumption_cannabis + personality_sensation*consumption_cannabis 
        + personality_extraversion*consumption_cannabis + personality_impulsiveness*consumption_cannabis 
        + personality_neuroticism*consumption_amphetamines + personality_agreeableness*consumption_amphetamines + personality_sensation*consumption_amphetamines 
        + personality_extraversion*consumption_amphetamines + personality_impulsiveness*consumption_amphetamines 
        + personality_neuroticism*consumption_mushrooms + personality_agreeableness*consumption_mushrooms + personality_sensation*consumption_mushrooms 
        + personality_extraversion*consumption_mushrooms + personality_impulsiveness*consumption_mushrooms,
        data = data3.dwng,        
        method = "glm",
        family = "binomial",
        metric = "ROC",
        trControl = ctrl_cv5)

summary(data3_logit_train1)

# We have noticed that only several variables are significant. The situation is similar as in case of the dataset 2. 
# Thus, we decided to omit interactions with soft drugs, leaving only hard ones.

data3_logit_train2 <- 
  train(consumption_cocaine_last_month ~ . 
        + personality_neuroticism*consumption_amphetamines + personality_agreeableness*consumption_amphetamines + personality_sensation*consumption_amphetamines 
        + personality_extraversion*consumption_amphetamines + personality_impulsiveness*consumption_amphetamines,
        data = data3.dwng,        
        method = "glm",
        family = "binomial",
        metric = "ROC",
        trControl = ctrl_cv5)

summary(data3_logit_train2)

# Model without any interactions?

data3_logit_train3 <- 
  train(consumption_cocaine_last_month ~ .,
        data3.dwng,        
        method = "glm",
        family = "binomial"(link = "logit"),
        metric = "ROC",
        trControl = ctrl_cv5)

logit_summary = rbind(c("Dataset 1",sapply(data_logit_train1$resample,mean), 
                        mean((data_logit_train1$resample$Sens+data_logit_train1$resample$Spec)/2)),
                      c("Dataset 1",sapply(data_logit_train2$resample, mean),
                        mean((data_logit_train2$resample$Sens+data_logit_train2$resample$Spec)/2)),
                      c("Dataset 1",sapply(data_logit_train3$resample, mean),
                        mean((data_logit_train3$resample$Sens+data_logit_train3$resample$Spec)/2)),
                      c("Dataset 2",sapply(data2_logit_train1$resample,mean), 
                        mean((data2_logit_train1$resample$Sens+data2_logit_train1$resample$Spec)/2)),
                      c("Dataset 2",sapply(data2_logit_train2$resample, mean),
                        mean((data2_logit_train2$resample$Sens+data2_logit_train2$resample$Spec)/2)),
                      c("Dataset 2",sapply(data2_logit_train3$resample, mean),
                        mean((data2_logit_train3$resample$Sens+data2_logit_train3$resample$Spec)/2)),
                      c("Dataset 3",sapply(data3_logit_train1$resample,mean), 
                        mean((data3_logit_train1$resample$Sens+data3_logit_train1$resample$Spec)/2)),
                      c("Dataset 3",sapply(data3_logit_train2$resample, mean),
                        mean((data3_logit_train2$resample$Sens+data3_logit_train2$resample$Spec)/2)),
                      c("Dataset 3",sapply(data3_logit_train3$resample, mean),
                        mean((data3_logit_train3$resample$Sens+data3_logit_train3$resample$Spec)/2)))[,-5]
colnames(logit_summary)[c(1,5)] = c("Dataset", "Balanced Accuracy")
logit_summary

# We decided to go on with the third model of the third dataset because it is 
# characterized with the simplicity and similar balanced accuracy

final_logitdwng_train = data3_logit_train3

#####      KNN                  ##########

k_possible = data.frame(k=seq(1,15,1))

# Dataset 1
data_knn_train <- 
  train(consumption_cocaine_last_month ~ .,
        data.dwng,        
        method = "knn",
        metric = "Spec",
        trControl = ctrl_cv5,
        tuneGrid = k_possible,
        preProcess = c("range"))

plot(data_knn_train)

# Dataset 2

data2_knn_train <- 
  train(consumption_cocaine_last_month ~ .,
        data2.dwng,        
        method = "knn",
        metric = "Spec",
        trControl = ctrl_cv5,
        tuneGrid = k_possible,
        preProcess = c("range"))

plot(data2_knn_train)

# Dataset 3
data3_knn_train <- 
  train(consumption_cocaine_last_month ~ .,
        data3.dwng,        
        method = "knn",
        metric = "Spec",
        trControl = ctrl_cv5,
        tuneGrid = k_possible,
        preProcess = c("range"))

plot(data_knn_train)

knn_summary = rbind(c("Dataset 1",sapply(data_knn_train$resample,mean), 
                      mean((data_knn_train$resample$Sens+data_knn_train$resample$Spec)/2)),
                    c("Dataset 2",sapply(data2_knn_train$resample, mean),
                      mean((data2_knn_train$resample$Sens+data2_knn_train$resample$Spec)/2)),
                    c("Dataset 3",sapply(data3_knn_train$resample, mean),
                      mean((data3_knn_train$resample$Sens+data3_knn_train$resample$Spec)/2)))[,-5]

colnames(knn_summary)[c(1,5)] = c("Dataset", "Balanced Accuracy")
knn_summary

# The KNN results become much more accurate after we balanced dataset. Again for 
# similar causes we decided to choose dataset 3 as a finale model.

final_knndwng_train = data3_knn_train

#####      SVM                  ##########

# Dataset 1

data_svm_train <- 
  train(consumption_cocaine_last_month ~ .,
        data.dwng,        
        method = "svmLinear",
        metric = "ROC",
        trControl = ctrl_cv5)

# Dataset 2

data2_svm_train <- 
  train(consumption_cocaine_last_month ~ .,
        data2.dwng,        
        method = "svmLinear",
        metric = "ROC",
        trControl = ctrl_cv5)

# Dataset 3
data3_svm_train <- 
  train(consumption_cocaine_last_month ~ .,
        data3.dwng,        
        method = "svmLinear",
        metric = "F1",
        trControl = ctrl_cv5)

## SVM Summary

svm_summary = rbind(c("Dataset 1",sapply(data_svm_train$resample,mean), 
                      mean((data_svm_train$resample$Sens+data_svm_train$resample$Spec)/2)),
                    c("Dataset 2",sapply(data2_svm_train$resample, mean),
                      mean((data2_svm_train$resample$Sens+data2_svm_train$resample$Spec)/2)),
                    c("Dataset 3",sapply(data3_svm_train$resample, mean),
                      mean((data3_svm_train$resample$Sens+data3_svm_train$resample$Spec)/2)))[,-5]

colnames(svm_summary)[c(1,5)] = c("Dataset", "Balanced Accuracy")
svm_summary

# Similarly as in case of the KNN

final_svmdwng_train = data3_svm_train

#####      Random Forest        #####
#Dataset 1

data_rf_train <- 
  train(consumption_cocaine_last_month ~ .,
        data.dwng,        
        method = "rf",
        metric = "Spec",
        trControl = ctrl_cv5)

data_rf_train$results
plot(data_rf_train)

# Dataset 2

data2_rf_train <- 
  train(consumption_cocaine_last_month ~ .,
        data2.dwng,        
        method = "rf",
        metric = "Spec",
        trControl = ctrl_cv5)

data2_rf_train$results
plot(data2_rf_train)

# Dataset 3

data3_rf_train <- 
  train(consumption_cocaine_last_month ~ .,
        data3.dwng,        
        method = "rf",
        metric = "Spec",
        trControl = ctrl_cv5)

data3_rf_train$results
plot(data3_rf_train)

# Modelling Summary
rf_summary = rbind(c("Dataset 1",sapply(data_rf_train$resample,mean), 
                      mean((data_rf_train$resample$Sens+data_rf_train$resample$Spec)/2)),
                    c("Dataset 2",sapply(data2_rf_train$resample, mean),
                      mean((data2_rf_train$resample$Sens+data2_rf_train$resample$Spec)/2)),
                    c("Dataset 3",sapply(data3_rf_train$resample, mean),
                      mean((data3_rf_train$resample$Sens+data3_rf_train$resample$Spec)/2)))[,-5]

colnames(rf_summary)[c(1,5)] = c("Dataset", "Balanced Accuracy")
rf_summary

final_rfdwng_train = data3_rf_train

#####################################
##         Model Summary           ##
#####################################




sum = rbind(c("Logit",sapply(final_logit_train$resample,mean), 
              mean((final_logit_train$resample$Sens+final_logit_train$resample$Spec)/2)),
            
            c("KNN",sapply(final_knn_train$resample, mean),
              mean((final_knn_train$resample$Sens+final_knn_train$resample$Spec)/2)),
            
            c("Random Forest",sapply(final_rf_train$resample,mean), 
              mean((final_rf_train$resample$Sens+final_rf_train$resample$Spec)/2)),
            
            c("Logit Downsampled",sapply(final_logitdwng_train$resample, mean),
              mean((final_logitdwng_train$resample$Sens+final_logitdwng_train$resample$Spec)/2)),
            
            c("KNN Downsampled",sapply(final_knndwng_train$resample, mean),
              mean((final_knndwng_train$resample$Sens+final_knndwng_train$resample$Spec)/2)),
            
            c("SVM Downsampled",sapply(final_svmdwng_train$resample,mean), 
              mean((final_svmdwng_train$resample$Sens+final_svmdwng_train$resample$Spec)/2)),
            
            c("Random Forest Downsampled",sapply(final_rfdwng_train$resample, mean),
              mean((final_rfdwng_train$resample$Sens+final_rfdwng_train$resample$Spec)/2)))[,-5]

colnames(sum)[c(1,5)] = c("Model","Balanced Accuracy")
sum


