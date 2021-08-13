setwd("/Users/sophiema/Desktop/CHL7001_Deep_Learning")
library(readxl)
library(tidyverse)
library(ggplot2)
library(ggpubr)
options(digits = 2)
# install.packages("viridis")
library("viridis")

#Dataset
#Hourly_demand <- read_excel("SSC2020_hourly_demand.xlsx", sheet = "Hourly Demand")
Hourly_demand <- read_csv("SSC2020_hourly_demand.csv")


#Trend over year by Month 
Hour <- Hourly_demand %>% group_by(Year, Month) %>%
  summarise(average_usage = mean(`Total Energy Use from Electricity (MW)`))
ggplot(Hour, aes(x=as.factor(Month), y = average_usage)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = "grey")) + 
  theme_minimal()+theme(plot.title = element_text(hjust=0.5, lineheight = .8, face = "bold"))+
  geom_line(aes(group = Year, color = factor(Year))) +
  labs(x = "Month", y = "Electricity Demand (MW)", color = "Year", title = "Trend Over Year by Month") +
  scale_y_continuous(limits = c(14000, 20000))+
  scale_color_viridis(discrete = TRUE, option = "D")
Hour2<-Hourly_demand %>% group_by(Year,Hour) %>%
  summarise(average_usage = mean(`Total Energy Use from Electricity (MW)`))

#Trend over year by Hour 
ggplot(Hour2, aes(x=as.factor(Hour), y = average_usage)) +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = "grey")) + 
  theme_minimal()+theme(plot.title = element_text(hjust=0.5, lineheight = .8, face = "bold"))+
  geom_line(aes(group = Year, color = factor(Year))) +
  labs(x = "Hour", y = "Electricity Demand (MW)", color = "Year", title = "Trend Over Year by Hour") +
  scale_y_continuous(limits = c(12000, 20000))+
  scale_color_viridis(discrete = TRUE, option = "D")
