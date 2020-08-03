## Data Script

# use xwoba factor

rm(list=ls())
setwd("~/Spellman")

library(dplyr)
library(plyr)
library(readr)
library(tidyverse)
library(randomForest)
library(MASS)
library(corrplot)

# Getting the Data
sc19<-read.csv("Statcast2019.csv")

# Changing xwOBA name
names(sc19)[72] <- "xwOBA"

# Getting rid random events
sc19 <- subset(sc19, events == "single" |
                 events == "double" |
                 events == "triple" |
                 events == "home_run" |
                 events == "walk" |
                 events == "strikeout" |
                 events == "field_out" |
                 events == "hit_by_pitch" |
                 events == "force_out" |
                 events == "sac_fly" |
                 events == "field_error" |
                 events == "grounded_into_double_play" |
                 events == "double_play" |
                 events == "sac_fly_double_play" |
                 events == "fielders_choice_out" |
                 events == "fielders_choice" |
                 events == "run" |
                 events == "null")

# Getting rid of a foul ball with two strikes
sc19 <- subset(sc19, strikes!=2 | description!="foul")


# Getting rid of pitchouts, bunt sequences, and hit by pitch
sc19 <- subset(sc19, description == "hit_into_play" |
                 description == "swinging_strike" |
                 description == "blocked_ball" |
                 description == "called_strike" |
                 description == "hit_into_play_no_out" |
                 description == "ball" |
                 description == "foul" |
                 description == "hit_into_play_score" |
                 description == "foul_tip" |
                 description == "swinging_strike_blocked"
               )

sc19 <- sc19 %>%
  mutate(oc = case_when(description=="called_strike" | description=="swinging_strike" | description=="foul" | description=="swinging_strike_blocked" | description=="foul_tip" ~ "S",
                        description=="blocked_ball" | description=="ball" | description=="intent_ball" ~ "B",
                        description=="hit_into_play" | description == "hit_into_play_no_out" | description == "hit_into_play_score" ~"X"))

# Creating Run Values
sc19 <- sc19 %>%
  mutate(rv = case_when(
    
                      # Run Values For Pitched Strikes
                      balls==0 & strikes==0 & events=="null" & oc=="S" ~ -0.037,
                      balls==1 & strikes==0 & events=="null" & oc=="S" ~ -0.035,
                      balls==2 & strikes==0 & events=="null" & oc=="S" ~ -0.062,
                      balls==3 & strikes==0 & events=="null" & oc=="S" ~ -0.117,
                      balls==0 & strikes==1 & events=="null" & oc=="S" ~ -0.051,
                      balls==1 & strikes==1 & events=="null" & oc=="S" ~ -0.054,
                      balls==2 & strikes==1 & events=="null" & oc=="S" ~ -0.069,
                      balls==3 & strikes==1 & events=="null" & oc=="S" ~ -0.066,
                      balls==0 & strikes==2 & events=="strikeout" & oc=="S" ~ -0.150,
                      balls==1 & strikes==2 & events=="strikeout" & oc=="S" ~ -0.171,
                      balls==2 & strikes==2 & events=="strikeout" & oc=="S" ~ -0.209,
                      balls==3 & strikes==2 & events=="strikeout" & oc=="S" ~ -0.294,
                      
                      # Run Values For Pitched Balls
                      balls==0 & strikes==0 & events=="null" & oc=="B" ~ 0.032,
                      balls==1 & strikes==0 & events=="null" & oc=="B" ~ 0.088,
                      balls==2 & strikes==0 & events=="null" & oc=="B" ~ 0.143,
                      balls==3 & strikes==0 & events=="walk" & oc=="B" ~ 0.051,
                      balls==0 & strikes==1 & events=="null" & oc=="B" ~ 0.024,
                      balls==1 & strikes==1 & events=="null" & oc=="B" ~ 0.048,
                      balls==2 & strikes==1 & events=="null" & oc=="B" ~ 0.064,
                      balls==3 & strikes==1 & events=="walk" & oc=="B" ~ 0.168,
                      balls==0 & strikes==2 & events=="null" & oc=="B" ~ 0.021,
                      balls==1 & strikes==2 & events=="null" & oc=="B" ~ 0.038,
                      balls==2 & strikes==2 & events=="null" & oc=="B" ~ 0.085,
                      balls==3 & strikes==2 & events=="walk" & oc=="B" ~ 0.234,
                      
                      # Run Values For Balls in Play
                      balls==0 & strikes==0 & oc=="X" ~ (xwOBA-.310)/1.157,
                      balls==1 & strikes==0 & oc=="X" ~ (xwOBA-.355)/1.157,
                      balls==2 & strikes==0 & oc=="X" ~ (xwOBA-.436)/1.157,
                      balls==3 & strikes==0 & oc=="X" ~ (xwOBA-.622)/1.157,
                      balls==0 & strikes==1 & oc=="X" ~ (xwOBA-.262)/1.157,
                      balls==1 & strikes==1 & oc=="X" ~ (xwOBA-.293)/1.157,
                      balls==2 & strikes==1 & oc=="X" ~ (xwOBA-.352)/1.157,
                      balls==3 & strikes==1 & oc=="X" ~ (xwOBA-.470)/1.157,
                      balls==0 & strikes==2 & oc=="X" ~ (xwOBA-.196)/1.157,
                      balls==1 & strikes==2 & oc=="X" ~ (xwOBA-.196)/1.157,
                      balls==2 & strikes==2 & oc=="X" ~ (xwOBA-.273)/1.157,
                      balls==3 & strikes==2 & oc=="X" ~ (xwOBA-.352)/1.157))

# Setting up a dummy variable for RHB vs RHP

sc19 <- sc19 %>%
  mutate(right_hit = ifelse(stand == "R", 1, 0), right_pitch = ifelse(p_throws == "R", 1, 0))

# Dummy Variable for Fastball
sc19 <- sc19 %>%
  mutate(isFB = ifelse(pitch_type == "FA" | pitch_type == "FT" | pitch_type == "FF" | pitch_type == "FC", 1, 0))


# Getting the columns of the data that I need for the random forest

s19 <- sc19 %>%
  dplyr::select(release_speed, plate_x, plate_z, pfx_x, pfx_z, release_spin_rate, rv, right_hit, right_pitch, isFB)
  
s19 <- s19 %>%
  na.omit(s19)

s19 <- s19 %>%
  na.exclude(s19)


# Getting a 5000 observation sample
set.seed(3)
ss19 <- sample(1:nrow(s19), 100000)

## Fitting Regression Trees ##

train = sample(1:nrow(s19), nrow(s19)/2)


# Random Forest #
set.seed(3)

rf.s19 = randomForest(rv~., data=s19, subset = ss19,
                      mtry = 3, importance = TRUE)

importance(rf.s19)

varImpPlot(rf.s19)

# Save the model
saveRDS(rf.s19, "./rf10wFB.rds")

# Load Model
#rfs19 <- readRDS("./.rds")
print(rfs19)

# Finding the correlation between variables

c <- cor(s19)
corrplot(c, method = "number", type = "lower")

## Saved Models ##
# "./final_model.rds" - 50000 obs w RvR
# "./model100000.rds" - 100000 obs w RvR
# "./rf5wFB.rds" - 50000 obs w isFB
# "./rf10wFB.rds" -100000 obs w isFB
