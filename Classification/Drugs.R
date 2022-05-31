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
library(class)
source("functions/F_summary_binary_class.R")
#####      Dataset              ##########

## Data import
data2 = read.csv("drugs_train.csv")

##Checking for NAs
colSums(is.na(data2)) %>% 
  sort()
#There is no NAs

data2[which(duplicated(data2)),]

## Checking types of variables

# Id - to be dropped
data2 = data2[,-1]
data2[-c("ethnicity")]
# Converting categorical variables into factors

##Age
# Two last groups merged into one to be more representative

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
data2$ethnicity = as.factor(data2$ethnicity)

##Consumption Alcohol
table(data2$consumption_alcohol)

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
colnames(data2)
prop.table(table(data2$consumption_caffeine, data2$consumption_cocaine_last_month),1)
data2 = data2[,-15]

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

data2 = data2[,-16]


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
# The p.value of the "education" variable is above 5%, thus we decided to drop it.
data2 = data2[,-3]

## Continuous Variables
min_max = matrix(1:14,nrow = 7, ncol = 2)
colnames(min_max) = c("Minimum", "Maximum")
rownames(min_max) = names(which(sapply(data2,is.numeric)))
min_max[,1] = apply(data2[names(which(sapply(data2,is.numeric)))],2, min)
min_max[,2] = apply(data2[names(which(sapply(data2,is.numeric)))],2, max)

for(i in names(which(sapply(data2,is.numeric)))){
  print(paste(i, min(data2[i]), max(data2[i])))
}

for(i in names(which(sapply(data2,is.numeric)))){
  hist(unlist(data2[i]), main=i, xlab="level")
}
## Histograms of the variables seem to be reasonable

for (i in names(which(sapply(data2,is.numeric)))){
  print(as.data.frame(paste(i,round(t.test(as.formula(paste(i,"~","consumption_cocaine_last_month")),
                             data=data2, alternative = "two.sided", var.equal = FALSE)$p.value,4))))
}
#Numeric variables seem to be okay there are no outliers and data is distributed more or less normally.


data2["Personality"] =  
  colnames(data2[sapply(data2,is.numeric)])[apply(data2[sapply(data2,is.numeric)],1,which.max)]

data2$Personality = as.factor(data2$Personality)

data2 = data2[sapply(data2,is.factor)]

#####################################
##         DATA MODELLING          ##
#####################################
#####      Logistic Regression  ##########

set.seed(9432398) # We specify random seed

# Train control with cross-validation
ctrl_cv5 <- trainControl(method = "repeatedcv",
                         number = 5,
                         classProbs = TRUE,
                         summaryFunction = twoClassSummary,
                         repeats = 3)

ctrl_cv5$sampling = "up"

## Dataset 2
# To increase the performance of the model we decided to add some interactions.
# We believed that the impact of the addictive substances might differ 
# depending on people's character traits. We decided to go with the following 
# variables, cannabis consumption, amphetamines consumption and mushrooms 
# consumption, because they constitute so called soft and hard drugs. 
# Thus, these three variables have been mixed with each other and with the 
# personality variable.

data2_logit_train1 <- 
  train(consumption_cocaine_last_month ~ . + Personality*consumption_amphetamines 
        + Personality*consumption_cannabis + Personality*consumption_cannabis,
        data = data2,        
        method = "glm",
        family = "binomial",
        metric = "Spec",
        trControl = ctrl_cv5)

summary(data2_logit_train1)

# We have noticed that many results are not significant. Thus, we decided to 
# get rid off interactions because they are  not significant. 

data2_logit_train2 <- 
  train(consumption_cocaine_last_month ~ .,
        data = data2,        
        method = "glm",
        family = "binomial",
        metric = "Spec",
        trControl = ctrl_cv5)
summary(data2_logit_train2)
data2_logit_train2$resample
# We decided to drop ethnicity because it is not significant.
data2_logit_train3 <- 
  train(consumption_cocaine_last_month ~ .,
        data = data2[,-4],        
        method = "glm",
        family = "binomial",
        metric = "Spec",
        trControl = ctrl_cv5)

# In terms of Balanced Accuracy we decided to go with the second model

final_logit = data2_logit_train2

#####      KNN                  ##########

# One of the biggest advantage of the KNN algorithm is its simplicity. It is quite intuitive method which simply finds
# K nearest neighbours. However, in our case this algorithm might not be the best one because our dataset is strongly
# imbalanced. It might result in getting  less common class wrongly classified.

k_possible = data.frame(k=seq(1,10,1))

data2_knn_train <- 
  train(consumption_cocaine_last_month ~ .,
        data2,        
        method = "knn",
        metric = "Spec",
        trControl = ctrl_cv5,
        tuneGrid = k_possible,
        preProcess = c("range"))

data2_knn_train$resample
summary((data2_knn_train$resample$Sens+data2_knn_train$resample$Spec)/2)
plot(data2_knn_train)

# We decided to choose final model with k=1

#####      SVM                  ##########

data2_svm_train <- 
  train(consumption_cocaine_last_month ~ .,
        data2,        
        method = "svmRadial",
        metric = "Spec",
        trControl = ctrl_cv5)

# We decided to go with C=0.25 and sigma = 0.022

#####      Random Forest        #####

#Additionally, we decided to train our data using Random Forest algorithm. 

data2_rf_train <- 
  train(consumption_cocaine_last_month ~ .,
        data2,        
        method = "rf",
        metric = "Spec",
        trControl = ctrl_cv5)

data2_rf_train$results
plot(data2_rf_train)


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

No = which(data2$consumption_cocaine_last_month=="No")
Yes = which(data2$consumption_cocaine_last_month=="Yes")

No.dwng = sample(No, length(Yes))
data2.dwng = data2[c(No.dwng, Yes),]
table(data2.dwng$consumption_cocaine_last_month)



#####      Logistic Regression  ##########

# To increase the performance of the model we decided to add some interactions.
# We believed that the impact of the addictive substances might differ 
# depending on people's character traits. We decided to go with the following 
# variables, cannabis consumption, amphetamines consumption and mushrooms 
# consumption, because they constitute so called soft and hard drugs. 
# Thus, these three variables have been mixed with each other and with the 
# personality variable.

data2_logit_train1 <- 
  train(consumption_cocaine_last_month ~ . + Personality*consumption_amphetamines 
        + Personality*consumption_cannabis + Personality*consumption_cannabis,
        data = data2.dwng,        
        method = "glm",
        family = "binomial",
        metric = "Spec",
        trControl = ctrl_cv5)

summary(data2_logit_train1)

# We have noticed that many results are not significant. Thus, we decided to 
# get rid off interactions because they are  not significant. 

data2_logit_train2 <- 
  train(consumption_cocaine_last_month ~ .,
        data = data2.dwng,        
        method = "glm",
        family = "binomial",
        metric = "Spec",
        trControl = ctrl_cv5)
summary(data2_logit_train2)

# We decided to drop ethnicity because it is not significant.
data2_logit_train3 <- 
  train(consumption_cocaine_last_month ~ .,
        data = data2.dwng[,-4],        
        method = "glm",
        family = "binomial",
        metric = "Spec",
        trControl = ctrl_cv5)

# In terms of Balanced Accuracy we decided to go with the second model

final_logit = data2_logit_train3

#####      SVM                  ##########

parametersC <-   expand.grid(C = 0.20,
                             sigma = 0.1)

data2_svm_train <- 
  train(consumption_cocaine_last_month ~ .,
        data2.dwng,        
        method = "svmRadial",
        metric = "Spec",
        trControl = ctrl_cv5)

# We decided to go with C=0.25 and sigma = 0.02186885





#####      Random Forest        #####

#Additionally, we decided to train our data using Random Forest algorithm. 

data2_rf_train <- 
  train(consumption_cocaine_last_month ~ .,
        data2.dwng,        
        method = "rf",
        metric = "Spec",
        trControl = ctrl_cv5)

data2_rf_train$results
plot(data2_rf_train)


#####################################
##         Models Evaluation       ##
#####################################
#####      Logit Regression      ####

logit_fitted = predict(data2_logit_train2, data2)
table(logit_fitted)
logit_results = summary_binary_class(predicted_classes = logit_fitted,
                     real = data2$consumption_cocaine_last_month)

#####      KNN                   ####

knn_fitted = predict(data2_knn_train, data2)
table(knn_fitted)
knn_results = summary_binary_class(predicted_classes = knn_fitted,
                     real = data2$consumption_cocaine_last_month)
data2_knn_train$finalModel$k

#####      SVM                   ####
svm_fitted = predict(data2_svm_train, data2)
table(svm_fitted)
svm_results = summary_binary_class(predicted_classes = svm_fitted,
                     real = data2$consumption_cocaine_last_month)

#####      Random Forest         #####

rf_fitted = predict(data2_rf_train, data2)
table(rf_fitted)
rf_results = summary_binary_class(predicted_classes = rf_fitted,
                     real = data2$consumption_cocaine_last_month)

#####################################
##         Final Model             ##
#####################################

balanced_accuracy  = matrix(1:4, nrow=1, ncol=4)

balanced_accuracy[1,1]=confusionMatrix(as.factor(logit_fitted), data2$consumption_cocaine_last_month)$byClass[11]
balanced_accuracy[1,2]=confusionMatrix(as.factor(knn_fitted), data2$consumption_cocaine_last_month)$byClass[11]
balanced_accuracy[1,3]=confusionMatrix(as.factor(svm_fitted), data2$consumption_cocaine_last_month)$byClass[11]
balanced_accuracy[1,4]=confusionMatrix(as.factor(rf_fitted), data2$consumption_cocaine_last_month)$byClass[11]

colnames(balanced_accuracy) = c("Logit","KNN","SVM","RF")
rownames(balanced_accuracy) = c("Balanced Accuracy")

print(paste0('The best algorithm for our dataset is ', colnames(balanced_accuracy)[apply(balanced_accuracy,1,which.max)],
             ' with the predicted Balanced Accuracy equals to ', round(balanced_accuracy[apply(balanced_accuracy,1,which.max)]*100,2),'%'))


