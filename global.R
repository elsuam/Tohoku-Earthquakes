library(shiny)
library(lubridate)
library(tidyverse)
library(leaflet)
library(leaflegend)
library(plotly)
library(fontawesome)
library(emojifont)
library(caret)
library(brnn)
library(neuralnet)


#read the data and create a new variable to be able to select annual timepoints while preserving the original timestamp
earthquakes_full <- read.csv("data/earthquakes.csv") %>% 
  mutate(timestamp = ymd_hms(time)) %>% 
  arrange(timestamp) %>% 
  mutate(year = year(timestamp))


#subsets the data to include only the highest magnitude earthquake for each year (plus the most recent before the big one)
yearly <- earthquakes_full %>% group_by(year) %>% 
  slice_max(mag) %>% 
  distinct(year,.keep_all = T) %>% 
  rbind(earthquakes_full[3158,]) %>% 
  arrange(timestamp)
yearly <- yearly[c(2:49),]


#yearly$time <- yearly$time %>% ymd_hms() %>% year()


date_start <- min(yearly$year)
date_end <- max(yearly$year)


#-----just keep for testing for now...---

#Subset data to include observations from January 1, 1965 to the Greak Quake of March 11, 2011
earthquakes_subset <- earthquakes_full[which(earthquakes_full$time >= "1965-01-26T23:47:37.120Z" &                                                       earthquakes_full$time <= "2011-03-11T05:46:24.120Z"),]

#Fine-tuning the geographic area of the model...
earthquakes_subset <- earthquakes_subset[which(earthquakes_subset$latitude > 35.72 &
                                                 earthquakes_subset$latitude < 40.82 &
                                                 earthquakes_subset$longitude > 139.37 &
                                                 earthquakes_subset$longitude < 143.37),]

earthquakes_subset <- earthquakes_subset[-as.numeric(count(earthquakes_subset)),] #to omit the 9.1

#---data frame of magnitudes (rounded to .1) and relative frequencies
eq <- data.frame(table(round(earthquakes_subset$mag, 1)) ) %>% 
  rename(mag = Var1,
         freq = Freq)

eq$mag <- as.numeric(as.character(eq$mag)) #change to numeric rather than factors

eq$freq <- eq$freq/(2011-1965) #AVERAGE annual frequencies over the 46-year span


#creates a new variable representing the frequency of earthquakes of AT LEAST that magnitude
for(i in 1:as.numeric(count(eq))){
  eq$freqc[i] <- sum( eq$freq[c(i:as.numeric(count(eq)))] )
}

eq_log <- eq %>% mutate(freqc = log10(freqc)) #transforming to the log scale
#------------------------------------------------------
gg <- eq #delete this soon
gg_log <- eq_log #this too

# #-----Linear model prerequisites-----
# gg_log <- gg %>%                             #transforming to the log scale
#   mutate(freqc = log10(freqc))
# 
 fitControl <- trainControl(method = "cv", number = 10) # 10-fold cross-validation
# #-----------------------------------



#-----MLP prerequisites-----
set.seed(4723)

#shuffle the data
df <- eq_log[sample(nrow(eq_log)), ]

#Extract 70% of data into train set and the remaining 30% in test set
train_test_split <- 0.7 * nrow(df)
train <- df[1:train_test_split,]
test <- df[(train_test_split+1): nrow(df),]

actual_log_mlp <- eq_log %>%
  mutate(type = "actual")

#---predicted data---
mdp <- seq(8,9.1, by = .1) #additional magnitude data points to add to prediction data

mlp_preds <- eq_log$mag %>% append(mdp) #append additional magnitudes to make predictions
#-----------------------------------





#-----Neural network prerequisites-----
gg_net <- gg
x <- gg_net$mag
y <- gg_net$freqc
bnn <- brnn(y~x,neurons=6)
#--------------------------------------








