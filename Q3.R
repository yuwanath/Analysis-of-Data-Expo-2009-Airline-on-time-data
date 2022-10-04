#Q3. How does the number of people flying between different locations change over time?

#Importing the libraries

library(data.table)
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)

#Opening the datasets and binding them 

y1<- fread("2006.csv")
y2<- fread("2007.csv")

#Converting to dataframes 

y1 <- data.frame(y1)
y2 <- data.frame(y2)

#Opening the datasets with necessary columns

y1 <- y1[ , c("Month","FlightNum", "Origin")]
y2 <- y2[ , c("Month","FlightNum", "Origin")]

#Cleaning the data of missing and inconsistent values, if any 

y1 <- na.omit(y1)
y2 <- na.omit(y2)

#Grouping the departure delays by month and obtaining the mean delay across months

Volume_y1 <- y1 %>% group_by (Month) %>%
  tally()

colnames(Volume_y1)[2] <- "2006"

Volume_y2 <- y2 %>% group_by (Month) %>%
  tally()

colnames(Volume_y2)[2] <- "2007"

plot <- merge(Volume_y1,Volume_y2, by = "Month")

#Renaming the index names since dataset is coded -> Month 1 (January) - 7 (December)

plot[,1] <- ifelse(plot[,1] == 1, "January", 
                   ifelse(plot[,1] == 2, "February",
                          ifelse(plot[,1] == 3, "March",
                                 ifelse(plot[,1] == 4, "April",
                                        ifelse(plot[,1] == 5, "May",
                                               ifelse(plot[,1] == 6, "June",
                                                      ifelse(plot[,1] == 7, "July",
                                                             ifelse(plot[,1] == 8, "August", 
                                                                    ifelse(plot[,1] == 9, "September",
                                                                           ifelse(plot[,1] == 10, "October",
                                                                                  ifelse(plot[,1] == 11, "November",
                                                                                         ifelse(plot[,1] == 12, "Deccember",0))))))))))))

plot$Month <- factor(plot$Month, levels = plot$Month)

#Plotting flight counts by month for both years

ggplot(plot, aes(x = Month, group = 1)) +
  geom_line(aes(y = `2006`), color = "blue",size=1) + #setting the line colour
  geom_line(aes(y = `2007`), color = "orange",size=1) + #setting the line colour
  xlab("Month") + #setting the label for the x axis
  ylab("Number of Flights") + #setting the label for the y axis
  geom_text(x="January",y=625000,label= "2007") +
  geom_text(x="January",y=587500,label= "2006")

#Grouping the flight count for each airport among the two years

origin_y1 <- y1 %>% group_by (Origin) %>%
  tally()

colnames(origin_y1)[2] <- "2006"

origin_y2 <- y2 %>% group_by (Origin) %>%
  tally()

colnames(origin_y2)[2] <- "2007"

test <- merge(origin_y1,origin_y2, by = "Origin")

#Employing a test to see whether there has been an increase over the years, where a 1 shows an increase

test <- transform(
  test, 'results' = ifelse(test$`2007`> test$`2006`, "1", "0"))

table(test$results)

(185/285) * 100

#Almost to 65% of airports have had an icrease in flights i.e. increase in passengers from 2006 to 2007