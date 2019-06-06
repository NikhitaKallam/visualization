library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(data.table)
library(DT)
library(tm)
library(wordcloud)
library(RColorBrewer)
#library(SDMTools)
library(faraway)
library(useful)
library(coefplot)

#library(corrplot)
library(hexbin)
#library(lattice)
library(MASS)
library(car)
library(lattice)
library(hexbin)
library(grid)
library(gridExtra)

flight <- read_csv("flights.csv")


#Determine ailine with most number of delays
mostdelays <- flight %>%
  group_by(AIRLINE) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  head(10)

ggplot(mostdelays, aes(x =reorder(AIRLINE, n), y = n )) +
  geom_bar(stat='identity',colour="white", fill = c("darkorchid4")) +
  labs(x = 'AIRLINE', y = 'COUNT', title = 'Airlines with most number of delays') +
  coord_flip() + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))



library(ggplot2)
ggplot(data=flight, aes(x=CANCELLED))+geom_bar(aes(fill=CANCELLED), alpha=0.5)


ggplot(flights.delay.sub.orig.lax.dest.sea, aes(DAY_OF_WEEK, 
                                                ARRIVAL_DELAY)) + geom_point(aes(color = factor(AIRLINE), 
                                                                                 shape = factor(AIRLINE),
                                                                                 size  = ARRIVAL_DELAY, alpha = 0.5)) + 
  xlab('Day of week in numbers (MON=1...SUN=7)') + ylab('Arrival delay in minutes') +
  ggtitle('Relationship between Departure delay and Arrival delay with Airline shown')




flights  = read.table("flights.csv")', sep =",", header = TRUE)
head(flights)

require(dplyr)
library(MASS)
require(repr)
require(ggplot2)
require(gridExtra)
require(glmnet)
flights.delay.sub <- flights[,c('YEAR','MONTH','DAY','DAY_OF_WEEK','AIRLINE','ORIGIN_AIRPORT','DESTINATION_AIRPORT',
                                     'SCHEDULED_DEPARTURE','SCHEDULED_ARRIVAL','DEPARTURE_DELAY','ARRIVAL_DELAY',
'AIR_SYSTEM_DELAY','SECURITY_DELAY','AIRLINE_DELAY','LATE_AIRCRAFT_DELAY',
'WEATHER_DELAY')]
flights.delay.sub <- flights.delay.sub[complete.cases(flights.delay.sub), ]
flights.delay.sub.orig.lax.dest.sea <- flights.delay.sub[ which(flights.delay.sub$ORIGIN_AIRPORT=='LAX' & 
flights.delay.sub$DESTINATION_AIRPORT=='SEA'), ]
numcols <- c('DEPARTURE_DELAY','ARRIVAL_DELAY','AIR_SYSTEM_DELAY','SECURITY_DELAY','AIRLINE_DELAY',
'LATE_AIRCRAFT_DELAY','WEATHER_DELAY')