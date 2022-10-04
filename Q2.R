#Q2. Do older planes suffer more delays?

#Importing the libraries

library(data.table)
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)

#Opening the datasets and binding them 

y1<- fread("2006.csv")
y2<- fread("2007.csv")
main_dataset <- rbind.data.frame(y1,y2)

#Converting to a dataframe 

df <- data.frame(main_dataset)

#Opening the datasets with necessary columns

dfq2 <- df[ , c("TailNum","DepDelay")]
plane_data<- read.csv("plane-data.csv", header = TRUE)
plane_data <- data.frame(plane_data)
plane_data <- plane_data[ , c("tailnum","year")]

#Filtering the dataset to obtain records of only delayed flights

dfq2 <- dfq2 %>% filter(DepDelay > 0)

#Cleaning the data of missing and inconsistent values, if any 

dfq2 <- na.omit(dfq2)
plane_data <- plane_data [!(plane_data$year=="" | plane_data$year=="None" | plane_data$year=="0000"),]

#Making a common column name to prepare for the inner merge

colnames(dfq2)[1] <- "tailnum"

#Merging the two datasets on Tail number

dfq2 <- merge(dfq2,plane_data, by = "tailnum")

#Grouping the mean departure delays by manufacturing year of the flight

delays_byage <- dfq2 %>% group_by (year) %>%
  summarise(dep_delay = mean(DepDelay),
            .groups = 'drop')

#Plotting the departure delays by manufacturing year of the flight

ggplot(delays_byage, aes(year,dep_delay, group = 1)) +
  geom_line(color = "blue",size=1) + #setting the line colour
  xlab("Manufacturing year") + #setting the label for the x axis
  ylab("Mean delay in minutes") + #setting the label for the y axis
  theme(axis.text.x = element_text(angle = 90)) + #rotating the xticks to increase readability
  geom_smooth(method=lm, se=FALSE, col='orange') #plotting the trend line for referencce

#Do older planes suffer more delays?

#Yes, there seems to be a declining trend between manufacturing year of the flight and departure delays. Meaning, newer planes suffer less delays and that older planes still in use seem to suffer greater delays.
