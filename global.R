library(shiny)
library(lubridate)
library(tidyverse)
library(leaflet)
library(leaflegend)
library(plotly)
library(fontawesome)
library(emojifont)


#read the data and create a new variable to be able to select annual timepoints while preserving the original timestamp
eq <- read.csv("C:/Users/Samuel/OneDrive - West Chester University of PA/STA 610/R Examples/Tohoku-Earthquakes/earthquakes.csv") %>% 
  mutate(timestamp = ymd_hms(time)) %>% 
  arrange(timestamp) %>% 
  mutate(year = year(timestamp))


#subsets the data to include only the highest magnitude earthquake for each year (plus the most recent before the big one)
yearly <- eq %>% group_by(year) %>% 
  slice_max(mag) %>% 
  distinct(year,.keep_all = T) %>% 
  rbind(eq[3158,]) %>% 
  arrange(timestamp)


#yearly$time <- yearly$time %>% ymd_hms() %>% year()


date_start <- min(yearly$year)
date_end <- max(yearly$year)



#-----just keep for testing for now...---

eq <- eq[c(3:3159),] #I only need the data up to that last point (for now) and only entire years up to then

#If I want to subset by the same geographic area as the book
#eq <- eq[which(eq$latitude > 37.72 & eq$latitude < 38.82 & eq$longitude > 141.87 & eq$longitude < 142.87),]

#Fine-tuning the geographic area os the model...
eq <- eq[which(eq$latitude > 35.72 & eq$latitude < 40.82 & eq$longitude > 139.37 & eq$longitude < 143.37),]

eq <- eq[-as.numeric(count(eq)),] #to omit the 9.1

#---data frame of magnitudes (rounded to .1) and relative frequencies
gg <- data.frame(table(round(eq$mag, 1)) ) %>% 
  rename(mag = Var1,
         freq = Freq)

gg$mag <- as.numeric(as.character(gg$mag)) #change to numeric rather than factors



gg$freq <- gg$freq/(2011-1965) #AVERAGE annual frequencies over the 46-year span


#creates a new variable representing the frequency of earthquakes of AT LEAST that magnitude
for(i in 1:as.numeric(count(gg))){
  gg$freqc[i] <- sum( gg$freq[c(i:as.numeric(count(gg)))] )
}
#------------------------------------------------------

