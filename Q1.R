#Q1. When is the best time of day, day of the week, and time of year to fly to minimise delays?

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

#Converting to a dataframe and filtering to obtain records of only delayed flights

df <- data.frame(main_dataset)
dfq1 <- df %>% filter(DepDelay > 0)

#Restricting to necessary columns only

dfq1 <- dfq1[ , c("Year","Month","DayofMonth","DayOfWeek","DepTime","DepDelay")]

#Cleaning the data of missing values, if any 

dfq1 <- na.omit(dfq1)

#Learning the data types of the columns

sapply(dfq1, class)

#Transforming DepTime to an hour format for convenient grouping

dfq1$DepTime <- dfq1$DepTime%/%100
dfq1$DepTime <- dfq1$DepTime*100
dfq1$DepTime <- sprintf("%04d",dfq1$DepTime)

#Eliminating values that are not in correct 24hour time format (Ex: 2500,2900)

dfq1 <- dfq1 %>% filter(DepTime <= 2300)

#Q1) i. Best time of day to fly to minimise delays = Between 0600-0700

#Grouping the departure delays by hour and obtaining the mean delay across hours

delays_byhour <- dfq1 %>% group_by (DepTime) %>%
  summarise(dep_delay = mean(DepDelay),
            .groups = 'drop')

#Plotting departure delay hourly

ggplot(delays_byhour, aes(x = DepTime, y = dep_delay)) +
  geom_bar(stat = "identity",
           show.legend = FALSE,
           aes(fill = dep_delay),  
           color = "gray30") + 
  xlab("DepTime") +
  ylab("Mean delay in minutes") +
  scale_fill_gradient2(low = "blue",
                       high = "red", midpoint = 54.77212)+
  geom_hline(aes(yintercept = mean(dep_delay)), linetype='dotted')+
  annotate("text", x = "1100", y = 55, label = "Average", vjust = -0.5)

#Q1) ii. Best day of the week to fly to minimise delays = On Saturday¶

#Grouping the departure delays by day and obtaining the mean delay across days

delays_byday <- dfq1 %>% group_by (DayOfWeek) %>%
  summarise(dep_delay = mean(DepDelay),
            .groups = 'drop')

delays_byday[,1] <- ifelse(delays_byday[,1] == 1, "Monday", 
                    ifelse(delays_byday[,1] == 2, "Tuesday",
                    ifelse(delays_byday[,1] == 3, "Wednesday",
                    ifelse(delays_byday[,1] == 4, "Thursday",
                    ifelse(delays_byday[,1] == 5, "Friday",
                    ifelse(delays_byday[,1] == 6, "Saturday",
                    ifelse(delays_byday[,1] == 7, "Sunday",0)))))))

delays_byday$DayOfWeek <- factor(delays_byday$DayOfWeek, levels = delays_byday$DayOfWeek)

ggplot(delays_byday, aes(x = DayOfWeek, y = dep_delay)) +
  geom_bar(stat = "identity",
           show.legend = FALSE,
           aes(fill = dep_delay),  
           color = "gray30") + 
  xlab("Day of week") +
  ylab("Mean delay in minutes") +
  scale_fill_gradient2(low = "blue",
                       high = "red", midpoint = 30.86452)+
  geom_hline(aes(yintercept = mean(dep_delay)), linetype='dotted')+
  annotate("text", x = "Saturday", y = 31, label = "Average", vjust = -0.5)

#Q1) iii. Best time of year to fly to minimise delays = In November

#Grouping the departure delays by month and obtaining the mean delay across months

delays_monthly <- dfq1 %>% group_by (Month) %>%
  summarise(dep_delay = mean(DepDelay),
            .groups = 'drop')

delays_monthly[,1] <- ifelse(delays_monthly[,1] == 1, "January", 
                      ifelse(delays_monthly[,1] == 2, "February",
                      ifelse(delays_monthly[,1] == 3, "March",
                      ifelse(delays_monthly[,1] == 4, "April",
                      ifelse(delays_monthly[,1] == 5, "May",
                      ifelse(delays_monthly[,1] == 6, "June",
                      ifelse(delays_monthly[,1] == 7, "July",
                      ifelse(delays_monthly[,1] == 8, "August", 
                      ifelse(delays_monthly[,1] == 9, "September",
                      ifelse(delays_monthly[,1] == 10, "October",
                      ifelse(delays_monthly[,1] == 11, "November",
                      ifelse(delays_monthly[,1] == 12, "Deccember",0))))))))))))

delays_monthly$Month <- factor(delays_monthly$Month, levels = delays_monthly$Month)

ggplot(delays_monthly, aes(x = Month, y = dep_delay)) +
  geom_bar(stat = "identity",
           show.legend = FALSE,
           aes(fill = dep_delay),  
           color = "gray30") + 
  xlab("Month") +
  ylab("Mean delay in minutes") +
  scale_fill_gradient2(low = "blue",
                       high = "red", midpoint = 30.81496)+
  geom_hline(aes(yintercept = mean(dep_delay)), linetype='dotted')+
  annotate("text", x = "October", y = 31, label = "Average", vjust = -0.5)
