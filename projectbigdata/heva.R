#load library
library(tidyverse)
library(lubridate)
library(gridExtra)
library(ggplot2)
library(dbplyr)
library(grid)

#load data from east borneo
east_borneo <- read.csv("eastborneo.csv")

#create bar chart vessel by year (2000-2015): East Kalimantan (Plot1)
ggplot(east_borneo, aes(Year, Vessel)) +
geom_bar(stat="identity", width = 0.5, fill="tomato2") + 
  labs(title="Vessel by Year",
       subtitle="East Kalimantan Province", 
       caption="Source: Statistic Bureau of Indonesia") +
  theme(axis.text.x = element_text(angle=65, vjust=0.5)) +
  theme(plot.title = element_text(hjust=0.5)) + 
  theme(plot.subtitle = element_text(hjust=0.5)) +
  scale_x_continuous(breaks = seq(2000, 2015, 1))
#Save plot as jpeg
ggsave("image/plot1.jpg")

#create trend series of production by year (2000-2015): East Kalimantan (Plot2)
ggplot(east_borneo, aes(Year, Prod)) +
geom_line(colour = "green") + 
  labs(title="Production by Year",
       subtitle="East Kalimantan Province", 
       caption="Source: Statistic Bureau of Indonesia") +
  theme(axis.text.x = element_text(angle=35, vjust=0.5)) +
  theme(plot.title = element_text(hjust=0.5)) + 
  theme(plot.subtitle = element_text(hjust=0.5)) +
  scale_x_continuous(breaks = seq(2000, 2015, 1))
ggsave("image/plot2.jpg")

#Read file of type boat
boattype <- read.csv("typeboat.csv")
#Delete column 6:7
boatype <-  boattype[, -c(6:7)]
#Delete rows of NA
boatype[rowSums(is.na(boattype))!=ncol(boattype),]
boatype <- boattype[0:16, ]

#Add new columns and titles in boatype table
boatype$newcolumn<-"na"
colnames(boatype)[6] <- "%none"
boatype$newcolumn<-"na"
colnames(boatype)[7] <- "%outboard"
boatype$newcolumn<-"na"
colnames(boatype)[8] <- "%motorboard"
#Calculate percentage of boat type (none, outboard, motorboat)
boatype$`%none` <- boatype$none / boatype$total * 100
boatype$`%outboard` <- boatype$outboard / boatype$total * 100
boatype$`%motorboard` <- boatype$motorboat / boatype$total * 100
print(boatype)

#Create the none, outboard, and motorboard graph from 2001-2015
barplot(cbind(boatype$none, boatype$outboard, boatype$motorboat), 
        main="year", ylab="Total", beside=TRUE)

# Fit regression model: Relationship between motorboard and outboard:East Kalimantan
motorboat1 <- lm(motorboat ~ outboard, data=boatype) 
# Summarize and print the results
summary(motorboat1) # show regression coefficients table
#Call:
lm(formula = motorboat ~ outboard, data=boatype)

# Fit our regression model: Relationship between motorboard and none motor:East Kalimantan
motorboat2 <- lm(motorboat ~ none, data=boatype) # data set
# Summarize and print the results
summary(motorboat2) # show regression coefficients table
#Call:
lm(formula = none ~ motorboat, data=boatype)

# Fit  regression model
vessel_prod <- lm(Vessel ~ Prod, data=east_borneo) 
# Summarize and print the results
summary(vessel_prod) # show regression coefficients table
#Call:
lm(formula = Vessel ~ Prod, data = east_borneo)

#Read file of kalimantan province
borneoprov <- read.csv("borneoprov.csv")
stringsAsFactors=FALSE
#Delete rows containing NA values
NROW(borneoprov)
borneoprov <- borneoprov[!is.na(borneoprov$year),]
# Create graph of Kalimantan province
ggplot(borneoprov, aes(x=year, y=prod, colour=kalprov, group=kalprov)) +
  geom_line() +
  geom_point() +
  labs(title="Total Production",
       subtitle="Kalimantan Province", 
       caption="Source: Statistic Bureau of Indonesia") +
  theme(plot.title = element_text(hjust=0.5)) + 
  theme(plot.subtitle = element_text(hjust=0.5)) +
  scale_y_continuous(breaks = seq(0, 200000, 25000))
ggsave("image/plot3.jpg")

#Read file of kalimantan province
fisher_men <- read.csv("fishers.csv")
fisher_menn <-  fisher_men[0:12, ]
ggplot(fisher_menn, aes(y=prod, x=year, color=kalprov, fill=kalprov)) + 
  geom_bar( stat="identity") +
  labs(title="All Kalimantan Production",
       subtitle="Kalimantan Province", 
       caption="Source: Statistic Bureau of Indonesia") +
  theme(plot.title = element_text(hjust=0.5)) + 
  theme(plot.subtitle = element_text(hjust=0.5)) 
ggsave("image/plot4.jpg")

# Fit  regression model
fisher_prod <- lm(fishermen ~ prod, data=fisher_menn) 
# Summarize and print the results
summary(fisher_prod) # show regression coefficients table
#Call:
lm(formula = fishermen ~ prod, data=fisher_menn)
library(reshape2)
