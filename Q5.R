#Q5. Use the available variables to construct a model that predicts delays

#Importing the libraries

library(data.table)
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(caTools)
library(caret)
library(randomForest)

#Opening the datasets and binding them 

y1<- fread("2006.csv")
y2<- fread("2007.csv")
plane_data <- read.csv("plane-data.csv", header = TRUE)
main_dataset <- rbind.data.frame(y1,y2)

#Selecting features, converting to a dataframe and cleaning

df <- data.frame(main_dataset)
dfq5 <- df[ , c("Year","Month","DayOfWeek","TailNum","CRSDepTime","UniqueCarrier","DepDelay","CRSElapsedTime","Distance","LateAircraftDelay")]
dfq5 <- na.omit(dfq5)


plane_data <- data.frame(plane_data)
plane_data <- plane_data[ , c("tailnum","year")]
plane_data <- plane_data [!(plane_data$year=="" | plane_data$year=="None" | plane_data$year=="0000"),]

#Transforming DepTime to an hour format to reduce noice

dfq5$CRSDepTime  <- as.character(dfq5$CRSDepTime%/%100)

#Merging the two datasets on Tail number

colnames(dfq5)[4] <- "tailnum"

#Taking a representative sample of the data due to constraints

dfq5 <- dfq5[sample(nrow(dfq5), 70000),]
dfq5 <- merge(dfq5,plane_data, by = "tailnum")
dfq5 <- dfq5[sample(nrow(dfq5), 40000),]
colnames(dfq5)[11] <- "ManuYear"

#Making categorical features factors 

dfq5$CRSDepTime  <- as.factor(dfq5$CRSDepTime)
dfq5$tailnum  <- as.factor(dfq5$tailnum)
dfq5$Month  <- as.factor(dfq5$Month)
dfq5$DayOfWeek  <- as.factor(dfq5$DayOfWeek)
dfq5$UniqueCarrier  <- as.factor(dfq5$UniqueCarrier)
dfq5$ManuYear  <- as.factor(dfq5$ManuYear)

#Encoding the target value

dfq5 <- dfq5 %>% 
  mutate(delayed = if_else(DepDelay > 0, 1 , 0))

dfq5$delayed <- as.factor(dfq5$delayed)
drop <- c("DepDelay" , "tailnum", "Year")
dfq5 <- dfq5[ , !(names(dfq5) %in% drop)]

# Set Training and Testing Data

split <- sample.split(dfq5, SplitRatio = 0.7)
split

train <- subset(dfq5, split == "TRUE")
test <- subset(dfq5, split == "FALSE")

# Fitting Random Forest to the train dataset

set.seed(120)  # Setting seed
classifier_RF = randomForest(x = train[-9],
                             y = train$delayed,
                             ntree = 500)

classifier_RF

# Predicting the Test set results

y_pred = predict(classifier_RF, newdata = test[-9])

# Confusion Matrix

confusion_mtx = table(test[, 9], y_pred)
confusion_mtx
confusionMatrix(y_pred,test[, 9])

# Plotting model
plot(classifier_RF)

# Importance plot
importance(classifier_RF)

# Variable importance plot
varImpPlot(classifier_RF)
