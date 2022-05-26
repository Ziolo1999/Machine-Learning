############################# REGRESSION DATASET ##################################
## LOADING THE DATABASE ###########################################################
#### Preparing the data ###########################################################
data <- read.csv("C:/Users/orjen/OneDrive/Desktop/Projekt R/traffic_train.csv")

## Biblioteki
library(dplyr)
library(readr)
library(ggplot2)
library(zoo)

ggplot(data, aes(x=date_time, y=traffic))+
  geom_point()

## 0 NA
colSums(is.na(data)) %>% 
  sort()

## Duplicates - there are no duplicates
data[which(duplicated(data)),]

## Do we have a data in our set?
sapply(data, is.character)
is.date <- function(x) inherits(x, 'Date')
is.date(data$date_time)

## We decode data into interesting parts for us
data$date_time <- as.POSIXct(data$date_time, format = "%Y-%m-%d %H:%M:%S")
## We will check later if quarter, according to our intuition is relevant
yq <- as.yearqtr(data$date_time, format = "%Y-%m-%d %H:%M:%S")
data$kwartal<-format(yq, format = "%q")
## Hour is for sure relevant for the traffic
data$hour<-format(data$date_time, format='%H')
data$hour<-as.numeric(data$hour)

## Due to our transformation we receive 3 NAs but it shouldnt be relevant so we just drop them
data[which(is.na(data$date_time)),]
data<-na.omit(data)


## Accoring to our intuition we divide hours into 4 parts of the day according to our intuition and
## then later to boxplots we checked

time_of_the_day<-function(x) {
  result<-list()
  if(x>=6 & x<=8){
    result<-"Traffic_morning_peak_hour"
  } 
  else if(x>8 & x<15){
    result<-"Working_hours"}
  else if(x>=15 & x<=17){
    result<-"Traffic_evening_peak_hour"}
  else{
    result <-"night"}
  return(result)
}

## Then we apply this function to our dataset
data$hour2<-lapply(data$hour,time_of_the_day)
data$hour2<-as.character(data$hour2)

## We check which one is character
sapply(data, is.character)


## We inspect our first set of wether_general (detailed will be skipped because it's at all overfitting
## our model in our opinion)

table(data$weather_general) %>%
  sort(decreasing = FALSE)

## Merge fog and mist 
## Smoke and Haze 
## Squall and Thunderstorm


##then we merge some of them into factors. We don't really need to order them
data$weather_general <- as.factor(data$weather_general)

data$weather_general[data$weather_general == "Maze"] <- "Fog"
data$weather_general[data$weather_general == "Haze"] <- "Smoke"
data$weather_general[data$weather_general == "Squall"] <- "Thunderstorm"

data$weather_general <- droplevels(data$weather_general)
table(data$weather_general)

data$hour2<- as.factor(data$hour2)

data$kwartal<- as.factor(data$kwartal)

## There is no need to use this parameter
table(data$weather_detailed) %>%
  sort(decreasing = FALSE)

## We won't use it because it's pretty always equals to 0
table(data$snow_mm)

## let's check temperature -> We see 10 outliers, for sure on earth there is not
## a temperature equal to -273.1
table(data$temperature)
data<-data[-c(which(data$temperature==-273.1)),]

## RAIN -> we decided to drop 9831.3 because we reckon it's a bad leverage point or outlier
table(data$rain_mm)

data<-data[-c(which(data$rain_mm>100)),]

##Anyway this is also pretty always equal to 0, we won't use it, may overfit the model

## Dataset for now

data_final<-data[,c(8,2,4,5,9,11)]


## Feature selection - We need to at all decide if our preassumptions are good


houses_numeric_vars <- 
  # check if variable is numeric
  sapply(data_final, is.numeric) %>% 
  # select those which are
  which() %>% 
  # and keep just their names
  names()

houses_correlations <- 
  cor(data_final[, houses_numeric_vars],
      use = "pairwise.complete.obs")

houses_correlations ##clouds_coverage_pct seems to be irrelevant. We need to keep it in mind
## Maybe it will be necessary to drop it in the final model. By far I will use information criteria to 
## decide at the end


## Anova for categorical variables

houses_categorical_vars <- 
  # check if variable is a factor
  sapply(data_final, is.factor) %>% 
  # select those which are
  which() %>% 
  # and keep just their names
  names()



houses_F_anova <- function(categorical_var) {
  anova_ <- aov(data_final$traffic ~ 
                  data_final[[categorical_var]]) 
  
  return(summary(anova_)[[1]][1, 4])
}

sapply(houses_categorical_vars,
       houses_F_anova) %>% 
  # in addition lets sort them
  # in the decreasing order of F
  #  and store as an object
  sort(decreasing = TRUE) -> houses_anova_all_categorical

houses_anova_all_categorical
## Hour and general seems to be very relevant. Kwartal may be improved

##Some boxplots also
library(ggplot2)

ggplot(data, aes(x=kwartal, y=temperature)) + geom_boxplot()

ggplot(data, aes(x=kwartal, y=traffic)) + geom_boxplot()

ggplot(data, aes(x=hour2, y=traffic)) + geom_boxplot()

ggplot(data, aes(x=kwartal, y=clouds_coverage_pct)) + geom_boxplot()

ggplot(data, aes(x=day2, y=traffic)) + geom_line()

hist(data$traffic)

##Let's see if we can improve Kwartal
data$month<-format(data$date_time, format='%m')
data$day<-weekdays(data$date_time)




## quart -> winter/summer_holidays/the rest of the year
ggplot(data, aes(x=month, y=traffic)) + geom_boxplot()
ggplot(data, aes(x=month, y=temperature)) + geom_boxplot()

season_of_the_year <- function(x) {
  result<-list()
  if(x=='11' || x=='12' || x=='01'){
    result<-"Winter"
  } 
  else if(x=='07' || x=='08' || x=='09' ){
    result<-"Summer_Holidays"}
  else{
    result <-"The_Rest_of_the_season"}
  return(result)
}

day_of_the_wk<- function(x) {
  result<-list()
  if(x=='sobota' || x=='niedziela'){
    result<-"Weekend"
  } 
  else{
    result <-"Working_day"}
  return(result)
}

data$season<-lapply(data$month,season_of_the_year)
data$season<-as.character(data$season)
data$season<-as.factor(data$season)

data$day2<-lapply(data$day, day_of_the_wk)
data$day2<-as.character(data$day2)
data$day2<-as.factor(data$day2)

## By far better than the kwartal parametr (58 vs 22 in AnovaTest)

data_final<-data[,c(8,2,5,11,14,15)]


################# Test data
a<-data_final[which(data_final$traffic<10),]


data_test <- read.csv("C:/Users/orjen/OneDrive/Desktop/Projekt R/traffic_test.csv")

## Rozkodywanie daty
data_test$date_time <- as.POSIXct(data_test$date_time, format = "%Y-%m-%d %H:%M:%S")
yq <- as.yearqtr(data_test$date_time, format = "%Y-%m-%d %H:%M:%S")
data_test$kwartal<-format(yq, format = "%q")
data_test$hour<-format(data_test$date_time, format='%H')
data_test$hour<-as.numeric(data_test$hour)

data_test[which(is.na(data_test$date_time)),]
data_test[2566,9]<-2

time_of_the_day<-function(x) {
  result<-list()
  if(x>=6 & x<=8){
    result<-"Traffic_morning_peak_hour"
  } 
  else if(x>8 & x<15){
    result<-"Working_hours"}
  else if(x>=15 & x<=17){
    result<-"Traffic_evening_peak_hour"}
  else{
    result <-"night"}
  return(result)
}

data_test$hour2<-lapply(data_test$hour,time_of_the_day)
data_test$hour2<-as.character(data_test$hour2)

data_test$weather_general <- as.factor(data_test$weather_general)

data_test$weather_general[data_test$weather_general == "Maze"] <- "Fog"
data_test$weather_general[data_test$weather_general == "Haze"] <- "Smoke"
data_test$weather_general[data_test$weather_general == "Squall"] <- "Thunderstorm"

data_test$weather_general <- droplevels(data_test$weather_general)
table(data_test$weather_general)

data_test$hour2<- as.factor(data_test$hour2)

data_test$month<-format(data_test$date_time, format='%m')
data_test$day<-weekdays(data_test$date_time)

data_test[2566,11]<-"03"

season_of_the_year <- function(x) {
  result<-list()
  if(x=='11' || x=='12' || x=='01'){
    result<-"Winter"
  } 
  else if(x=='07' || x=='08' || x=='09'){
    result<-"Summer_Holidays"}
  else{
    result <-"The_Rest_of_the_season"}
  return(result)
}

data_test[2566,12]<-"czwartek"

day_of_the_wk<- function(x) {
  result<-list()
  if(x=='sobota' || x=='niedziela'){
    result<-"Weekend"
  } 
  else{
    result <-"Working_day"}
  return(result)
}

data_test$season<-lapply(data_test$month,season_of_the_year)
data_test$season<-as.character(data_test$season)
data_test$season<-as.factor(data_test$season)


data_test$day2<-lapply(data_test$day, day_of_the_wk)
data_test$day2<-as.character(data_test$day2)
data_test$day2<-as.factor(data_test$day2)

data_test_final<-data_test[,c(2,5,10,13,14)]
a<-data[which(data_final$traffic<1),]

######################### Regresssion estimating part ####################################################
## For sure we will try to chack both ends, I mean high variance and low bias plus low variance and high
## bias then we will use linear regression + Lasso/Ridge. Then we have two imporant efficient algorithms
## KNN which is one of the most optimal accoring to books and SVR which is also very sufficient
## especially if we have weird combinations in dataset

## Linear regression
library(olsrr)
## To encode dumies we set options

options(contrasts = c("contr.treatment",  # for non-ordinal factors
                      "contr.treatment")) # for ordinal factors

## The reason why don't I divide the dataset into train/test samples is that In this particular case I prefer
## other measures. I like to check R^2 and the chart of predicted/real_values. Also the F statistic is very helpful for me
## Of course in different algorithims I really value test/train sample especially in cross-validation approach but
## in this I just like to do with different statistics 

traffic_lm <- lm(traffic~., # formula
                 data = data_final) # training data

summary(traffic_lm)

## weather_generalSmoke seems to be irrelevant, but at all the weather seems good. 
## We can always do an automatic selection of variables to be sure in addition to our judgment
## AIC based critierion is by far the best by my knowledge based on literature so I will apply it

ols_step_backward_aic(traffic_lm, 
                      progress =  TRUE) ->traffic_lm_AIC

#Final results
summary(traffic_lm_AIC$model)

# Let's vizualise it

traffic_predicted <- predict(traffic_lm_AIC$model)


## We strongly lack middle values, that's why linear regression may not be efficient enough for this dataset

hist(traffic_predicted)
hist(data_final$traffic)

ggplot(data.frame(real = data_final$traffic,
                  predicted = traffic_predicted),
       aes(x = predicted, 
           y = real)) +
  geom_point(col = "blue") +
  theme_bw()

## Not the best results so far, let's try to apply combination of variables and to remove an intercept

traffic_lm <- lm(traffic~. + weather_general*hour2  + day2*hour2 + hour2*season*day2 -1, # formula
                 data = data_final) # training data

summary(traffic_lm)

ols_step_backward_aic(traffic_lm, 
                      progress =  TRUE) ->traffic_lm_AIC2

summary(traffic_lm_AIC2$model)
summary(traffic_lm)
## No colinearity, that's good

traffic_predicted2 <- predict(traffic_lm_AIC2$model)
traffic_predicted3 <- predict(traffic_lm)


ggplot(data.frame(real = data_final$traffic,
                  predicted = traffic_predicted3),
       aes(x = predicted, 
           y = real)) +
  geom_point(col = "blue") +
  theme_bw()
## Seems to work a little bit better but still we miss middle values
## Let's stay with this version of model -> Validation latter

#####################################################################################################
################################ LASSO/Ridge regression #############################################
#####################################################################################################

## Now let's use something on the second end of the balance variance/bias so lasso/ridge regression

library(caret)
library(glmnet)


## Ridge regression
ctrl_cv10 <- trainControl(method = "cv",
                         number = 10)

parameters_ridge <- expand.grid(alpha = 0,
                                lambda = seq(0, 1e3, 1))

traffic_ridge <- train(traffic~. + weather_general*hour2  + day2*hour2 + hour2*season*day2.,
                       data = data_final,
                       method = "glmnet", 
                       tuneGrid = parameters_ridge,
                       trControl = ctrl_cv5)

traffic_ridge
plot(traffic_ridge)


## Optimal lambda equals 79 so close to linear regression (lambda = 0)
traffic_ridge$bestTune$lambda

## lets try to estimate lasso as well
ctrl_cv5 <- trainControl(method = "cv",
                         number = 5)

parameters_lasso <- expand.grid(alpha = 1,
                                lambda = seq(1, 1e4, 10))

traffic_lasso<- train(traffic~. + weather_general*hour2  + day2*hour2 + hour2*season*day2,
                      data = data_final,
                      method = "glmnet", 
                      tuneGrid = parameters_lasso,
                      trControl = ctrl_cv5)

traffic_lasso
plot(traffic_lasso)

##lets check the optimal lambda, it's 1, close to linear regression, very close 

traffic_lasso$bestTune$lambda



## Let's check values between 0 and 1 for alpha in elastic approach


set.seed(2137) ## Let's set seed because lambda and alfa may vary but there are both really similar, just to keep it constant
parameters_elastic2 <- expand.grid(alpha = seq(0, 1, 0.2), 
                                   lambda = seq(0, 10, 0.1))

traffic_elastic <- train(traffic~. + weather_general*hour2  + day2*hour2 + hour2*season*day2,
                         data = data_final,
                         method = "glmnet", 
                         tuneGrid = parameters_elastic2,
                         trControl = ctrl_cv5)

traffic_elastic

plot(traffic_elastic)

traffic_elastic$bestTune

## Optimal model -> alpha = 0.8 lambda = 1
## Let's do a prediction

predicted2 = predict(traffic_elastic , data_final)

ggplot(data.frame(real = data_final$traffic,
                  predicted = predicted2 ),
       aes(x = predicted, 
           y = real)) +
  geom_point(col = "blue") +
  theme_bw()



ggplot(data.frame(lm=traffic_predicted3,
                  lasso=predicted2), aes(x=lm, y=lasso), color=true) + geom_point()



#####################################################################################################
################################ SVR ################################################################
#####################################################################################################
library(kernlab)
library(verification)
library(ggplot2)
library(tidyverse)
library(caret)

## Let's estimate the perfect parameters
## Because my laptop is kinda specified with low parameters and I don't wanna use cloud for this project
## mainly because I wanna keep it all here to make a good raport and presentation I will draw a sample
## around 10% so 3000 to make this algorithm work and try to specify the best parameteres.
## This should be enough especially If i would draw it twice. At the end I will fit values to 
## test sample which is nothing else than different parameters. It's just important to be close enough to the data we use

#x<-sample(c(1:29687),3000, replace=TRUE)
#x<-data_final[x,]

#data2.svm_Radial2 <- train( traffic ~., 
#                            data = x, 
#                            method = "svmRadial",
#                            tuneGrid = parametersC_sigma2,
#                            trControl = ctrl_cv5)
#data2.svm_Radial2



##Optimal sigma around 0.1
##Optimal C around around 415



## Let's estimate in on full dataset and see the results
parametersC_sigma2 <- 
  expand.grid(C = 415,
              sigma = 0.1)


data2.svm_Radial2 <- train(traffic~. + weather_general*hour2  + day2*hour2 + hour2*season*day2, 
                            data = data_final, 
                            method = "svmRadial",
                            tuneGrid = parametersC_sigma2,
                            trControl = ctrl_cv5)
data2.svm_Radial2

## prediction

predicted2 = predict(data2.svm_Radial2 , data_final)

ggplot(data.frame(real = data_final$traffic,
                  predicted = predicted2 ),
       aes(x = predicted, 
           y = real)) +
  geom_point(col = "blue") +
  theme_bw()

## Finaly something that fits the middle values :) 

#####################################################################################################
################################ KNN ################################################################
#####################################################################################################

## I decided to use this algorithm becuase in my books for Machine Learning they say this algorithm may be the
## most accurate one in many cases. What I mean, we don't really use any preasumptions and we only
## match something because of neighbours. In this case, when it's hard for my data to fit in any
## kind of regression that assume for example normal distribution, or try to fit to data any coefficients
## this one may be really relevant player. We will see at the end in the comparision!

library(class)
library(kernlab)
library(verification)
library(ggplot2)
library(tidyverse)
library(caret)

##lets see what K should we set at first

sqrt(nrow(data_final)) ## -> 173 seems to be ok as the highest value

## We need to remember to scale our data. In fact of using combined numeric and factors we should 
## set it to "range' instead of Z-Scale 
ctrl_cv5 <- trainControl(method = "cv",
                         number = 5)

k_possible <-data.frame(k=19)

traffic_knn <- 
  train(traffic~. + weather_general*hour2  + day2*hour2 + hour2*season*day2, 
        data = data_final,
        method = "knn",
        trControl = ctrl_cv5,
        tuneGrid = k_possible,
        preProcess = c("range"))

## k=19 is the lowest RMSE we can obtain, So I will go with that.
traffic_knn

## Lets predict the final results
predicted2 = predict(traffic_knn , data_final)


# By far looks the best
ggplot(data.frame(real = data_final$traffic,
                  predicted = predicted2 ),
       aes(x = predicted, 
           y = real)) +
  geom_point(col = "blue") +
  theme_bw()



#################################################################################
#################### Evaluation for all of them #################################
#################################################################################

## We are supposed then to chose the best algorithm for our dataset
## The measure should be done as MAPE 

library(MLmetrics)

predicted_ols <- predict(traffic_lm, data_final)
predicted_lasso_ridge <- predict(traffic_elastic , data_final)
predicted_svr <- predict(data2.svm_Radial2 , data_final)
predicted_knn <- predict(traffic_knn, data_final)

predictions<-data.frame(predicted_ols,predicted_lasso_ridge,predicted_svr,predicted_knn)

MAPE<-matrix(8,nrow=2,ncol=4)

## We need to have those values at more than 0 or drop them, because MAPE won't work 
## Of course we can switch them, but in the notation there is clearly said that (y_predicted, y_real)


for(i in 1:length(predictions))
{
  MAPE[1,i] <-colnames(predictions[i])
  MAPE[2,i] <-MAPE(data_final$traffic, unlist(predictions[i]))
  if(i==length(predictions))
  {
    print(paste0('The best algorithm for our dataset is ', substr(MAPE[1,which(MAPE[2,]==min(MAPE))],11,20), ' with the predicted MAPE equals to ',
           round(as.numeric(min(MAPE))*100,2),'%'))
    
  }
}

MAPE(predicted_knn,data_final$traffic)

# So at the end I can only cite the function
# "The best algorithm for our dataset is knn with the predicted MAPE equals to 36.67%"