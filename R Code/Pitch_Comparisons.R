library(tidyverse)
library(randomForest)

## Load Data

# Raw Statcast
load("s19.RData")

# Statcast with Predicted Run Values also called s19 Rename if want both
load("s19rv.RData")

# Load Random Forest
rv.rf <- readRDS("rf10wFB.rds")

## Make Variables

s19 <- s19 %>% 
  
  # Change xwOBA Name
  rename(xwOBA = estimated_woba_using_speedangle) %>% 
  filter(
         # Getting rid random events
         events == "single" | 
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
           events == "null",
         
         # Getting rid of a foul ball with two strikes
         strikes!=2 | description!="foul",
         
         # Getting rid of pitchouts, bunt sequences, and hit by pitch
         description == "hit_into_play" |
           description == "swinging_strike" |
           description == "blocked_ball" |
           description == "called_strike" |
           description == "hit_into_play_no_out" |
           description == "ball" |
           description == "foul" |
           description == "hit_into_play_score" |
           description == "foul_tip" |
           description == "swinging_strike_blocked") %>% 
  mutate(oc = case_when(description=="called_strike" | description=="swinging_strike" | description=="foul" | description=="swinging_strike_blocked" | description=="foul_tip" ~ "S",
                        description=="blocked_ball" | description=="ball" | description=="intent_ball" ~ "B",
                        description=="hit_into_play" | description == "hit_into_play_no_out" | description == "hit_into_play_score" ~"X"),
         rv = case_when(
           
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
           balls==3 & strikes==2 & oc=="X" ~ (xwOBA-.352)/1.157),
         
         # Right Pitch Dummy
         right_pitch = ifelse(p_throws == "R", 1, 0),
         
         # Right Hit Dummy
         right_hit = ifelse(stand == "R", 1, 0),
         
         # Fastball Dummy
         isFB = ifelse(pitch_type == "FF" |
                         pitch_type == "FT" |
                         pitch_type == "FS" |
                         pitch_type == "FC", 1, 0)
         )

# Make RV Prediction
s19 <- s19 %>% 
  drop_na(plate_x, plate_z, 
           pfx_x, pfx_z, release_speed, release_spin_rate,
           right_pitch, right_hit, isFB) %>% 
  mutate(
    rv.hat = predict(rv.rf, .)
  ) 

## Leaderboards

# RV Leaderboard
rv_leader_pitch <- s19 %>% 
  select(pitch_type, player_name, plate_x, plate_z, 
         pfx_x, pfx_z, release_speed, release_spin_rate,
         right_pitch, right_hit, isFB, rv.hat) %>% 
  group_by(player_name, pitch_type) %>%
  mutate(
    N = n()
  ) %>% 
  filter(N > 50) %>% 
  summarise_all(mean, na.rm = T) %>% 
  arrange(rv.hat) %>% 
  select(player_name, pitch_type, rv.hat, N, everything())

#save(rv_leader_pitch, file = "rv_leader_pitch.RData")
load("rv_leader_pitch.RData")

# RV Leader for arsenal, weighted by use
rv_leader_arsenal <- rv_leader_pitch %>% 
  group_by(player_name) %>% 
  mutate(
    pct = N/sum(N),
    rv.weight = rv.hat*pct
  ) %>% 
  summarise(
    rv.ars = sum(rv.weight)
  ) %>% 
  arrange(rv.ars)

#save(rv_leader_arsenal, file = "rv_leader_arsenal.RData")
load('rv_leader_arsenal.RData')

# Actual v Predicted
actual_predict <- s19 %>% 
  select(player_name, pitch_type, rv, rv.hat) %>% 
  mutate(
    Diff = rv - rv.hat
  ) %>% 
  group_by(player_name, pitch_type) %>% 
  mutate(
    N = n()
  ) %>% 
  filter(N > 50) %>% 
  summarise_all(mean) %>% 
  arrange(Diff)

#save(actual_predict, file = 'actual_predict.RData')
load('actual_predict.RData')
  
## Best Location

# Average Stuff for each pitch type by pitcher hand
avg_stuff <- s19 %>% 
  select(pitch_type, pfx_x, pfx_z, release_speed, release_spin_rate,
         right_pitch, right_hit) %>% 
  group_by(right_pitch, right_hit, pitch_type) %>% 
  summarise_all(mean)

avg_stuff_quantile <- s19 %>% 
  select(pitch_type, pfx_x, pfx_z, release_speed, release_spin_rate,
         right_pitch, right_hit) %>% 
  group_by(right_pitch, right_hit, pitch_type) %>% 
  summarise_all(.funs = function(x) list(enframe(quantile(x, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE)))) %>% 
  unnest(c(pfx_x, pfx_z, release_speed, release_spin_rate), names_sep = "_") %>% 
  rename_all(funs(str_replace(.,"_name", "_pct"))) %>% 
  rename_all(funs(str_remove(.,"_value")))

# Join Average stuff to pitcher locations and run model
location_avgstuff <- s19 %>% 
  select(player_name, pitch_type, right_pitch, right_hit, plate_x, plate_z, isFB) %>% 
  left_join(.,avg_stuff) %>% 
  mutate(
    rv.hat = predict(rv.rf, .)
  ) %>% 
  group_by(player_name, pitch_type) %>% 
  mutate(
    N = n()
  ) %>% 
  filter(N > 50) %>% 
  summarise_all(mean) %>% 
  select(player_name, pitch_type, rv.hat, N) %>% 
  arrange(rv.hat)

#save(location_avgstuff, file = 'location_avgstuff.RData')
load('location_avgstuff.RData')

# Comparison within Zone

# Function to make plate grid with pitch type
plate_grid <- function(pitch = "FF") {
  plate_grid <- expand.grid(plate_x = seq(-1.5, 1.5, length=50),
                            plate_z = seq(1, 4, length=50)) %>% # Create zone grid
    mutate( # Label Zone parts
      zone = case_when(plate_x > -6.7/12 & plate_x <= 0 & plate_z > 22/12 & plate_z <= 2.5 ~ "Heart Low Left",
                       plate_x > -6.7/12 & plate_x <= 0 & plate_z > 2.5 & plate_z <= 38/12 ~ "Heart High Left",
                       plate_x <= 6.7/12 & plate_x > 0 & plate_z > 22/12 & plate_z <= 2.5 ~ "Heart Low Right",
                       plate_x <= 6.7/12 & plate_x > 0 & plate_z > 2.5 & plate_z <= 38/12 ~ "Heart High Right",
                       plate_x > -6.7/12 & plate_x <= 0 & plate_z <= 22/12 & plate_z > 14/12 |
                         plate_x > -13.3/12 & plate_x <= -6.7/12 & plate_z <= 2.5 & plate_z > 14/12 ~ "Shadow Low Left",
                       plate_x > -6.7/12 & plate_x <= 0 & plate_z > 38/12 & plate_z <= 46/12 |
                         plate_x > -13.3/12 & plate_x <= -6.7/12 & plate_z > 2.5 & plate_z <= 46/12 ~ "Shadow High Left",
                       plate_x <= 6.7/12 & plate_x > 0 & plate_z <= 22/12 & plate_z > 14/12 |
                         plate_x <= 13.3/12 & plate_x > 6.7/12 & plate_z <= 2.5 & plate_z > 14/12 ~ "Shadow Low Right",
                       plate_x <= 6.7/12 & plate_x > 0 & plate_z > 38/12 & plate_z <= 46/12 |
                         plate_x <= 13.3/12 & plate_x > 6.7/12 & plate_z > 2.5 & plate_z <= 46/12 ~ "Shadow High Right",
                       plate_x > -20/12 & plate_x <= 0 & plate_z <= 4.5 & plate_z > 46/12 |
                         plate_x <= -13.3/12 & plate_x > -20/12 & plate_z > 2.5 & plate_z <= 46/12 ~ "Chase High Left",
                       plate_x > -20/12 & plate_x <= 0 & plate_z <= 14/12 & plate_z > 0.5 |
                         plate_x <= -13.3/12 & plate_x > -20/12 & plate_z <= 2.5 & plate_z > 0.5  ~ "Chase Low Left",
                       plate_x <= 20/12 & plate_x > 0 & plate_z <= 4.5 & plate_z > 46/12 |
                         plate_x > 13.3/12 & plate_x <= 20/12 & plate_z > 2.5 & plate_z <= 46/12 ~ "Chase High Right",
                       plate_x <= 20/12 & plate_x > 0 & plate_z <= 14/12 & plate_z > 0.5 |
                         plate_x > 13.3/12 & plate_x <= 20/12 & plate_z <= 2.5 & plate_z > 0.5  ~ "Chase Low Right",
                       TRUE ~ "Waste"),
      pitch_type = pitch
    )
  
  return(plate_grid)
}

# Find average run value within each zone
zone_rv <- left_join(plate_grid("FF"), filter(avg_stuff, pitch_type == "FF", right_pitch == 1, right_hit == 1)) %>% 
  mutate(
    isFB = 1
  ) %>% 
  mutate(
    rv.hat = predict(rv.rf,.)
  ) %>% 
  group_by(zone) %>% 
  summarise(
    zone_rv_hat = mean(rv.hat)
  )

# Compare two players
player_stuff <- s19 %>%
  select(player_name, pitch_type, pfx_x, pfx_z, release_speed, release_spin_rate,
         right_pitch) %>% 
  group_by(player_name, pitch_type) %>% 
  summarise_all(mean)

player_zone_compare <- left_join(plate_grid("FF"), filter(player_stuff, player_name == "Gerrit Cole" | player_name == "Kyle Hendricks", pitch_type == "FF")) %>% 
  mutate(
    isFB = 1,
    right_hit = 1
  ) %>% 
  mutate(
    rv.hat = predict(rv.rf,.)
  ) %>% 
  group_by(player_name, zone) %>% 
  summarise(
    zone_rv.hat = mean(rv.hat)
  ) %>% 
  arrange(zone)

## Velo Effect on RV
velo_FF <- tibble(pitch_type = "FF",
                  plate_x = 0, 
                  plate_z = 2.5, 
                  release_speed = rep(87:100, 2),
                  right_pitch = 1,
                  right_hit =1,
                  release_spin_rate = rep(c(2300, 2800), each = 14),
                  isFB = 1
                  ) %>%  
  left_join(., select(avg_stuff, -release_speed, -release_spin_rate)) %>% 
  mutate(
    rv.hat = predict(rv.rf,.)
  )

ggplot(velo_FF, aes(x = release_speed, y = rv.hat)) +
  geom_line(aes(color = as.factor(release_spin_rate))) +
  theme_minimal()
