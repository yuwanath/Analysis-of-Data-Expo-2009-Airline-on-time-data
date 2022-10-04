#Q4. Can you detect cascading failures as delays in one airport create delays in others?

#Importing the libraries

library(data.table)
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(reticulate)

#Opening the datasets and binding them 

y1<- fread("2006.csv")
y2<- fread("2007.csv")
main_dataset <- rbind.data.frame(y1,y2)

#Converting to dataframes, filtering the necessary columns and cleaning null values 

main_dataset <- data.frame(main_dataset)
main_dataset <- main_dataset[ , c('Year','Month','DayofMonth','DepTime','TailNum','DepDelay')]
main_dataset <- na.omit(main_dataset)

#Creating a column with timestamps and sorting

main_dataset$DepTime <- str_pad(main_dataset$DepTime, 4, pad = "0")

main_dataset$timestamp <- paste(main_dataset$Year, main_dataset$Month, main_dataset$DayofMonth, main_dataset$DepTime, sep = "-")

main_dataset$timestamp <- as.POSIXct(main_dataset$timestamp, format="%Y-%m-%d-%H%M")

main_dataset <- main_dataset[do.call(order, main_dataset), ]

#Making a 2D dataframe with tail number and the specific plane's delays in the same order of their occurrence

heatmap <- main_dataset %>% group_by (TailNum,timestamp) %>% 
           summarise(delays = DepDelay)

set.seed(123)
heatmap <-heatmap %>%
           arrange(timestamp, delays, .by_group = TRUE) %>%
           group_by(TailNum) %>%
           mutate(COUNTER = 1:n()) %>%  ## as per comment, can use row_number()
           ungroup()

heatmap <- heatmap[ , c('TailNum','delays','COUNTER')]

heatmap <- heatmap %>%
  pivot_wider(names_from = COUNTER, values_from = delays)

heatmap$TailNum  <- as.double(heatmap$TailNum)

#Choosing a 100by100 snippet of the data due to the size of the dataset

heatmap_100 <- heatmap[0:100,0:100 ]

use_python("C:/ProgramData/Anaconda3/python.exe")
sns <- import('seaborn')
plt <- import('matplotlib.pyplot')
sns$heatmap(heatmap_100, fmt="g", cmap='coolwarm', vmax=60 , center=0)
plt$show()

#The horizontal clusters of red suggest the cascading delays of airplanes, an increase of redness in those same clusters shows the worsening of the delay while return to normalcy is shown by the shading out effect