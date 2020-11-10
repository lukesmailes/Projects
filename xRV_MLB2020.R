#Luke Smailes
#2020 xRV

setwd("~/Desktop/Baseball Analytics/MLB")

library(dplyr)
#library(plyr)
library(readr)
library(tidyverse)
library(randomForest)
library(MASS)
library(corrplot)
library(Metrics)

# Getting the Data
sc20 <-read.csv("SavantData2020.csv")

sc20$xwOBA = ifelse(sc20$estimated_woba_using_speedangle != "null", as.numeric(as.character(sc20$estimated_woba_using_speedangle)), 0)

# Getting rid of random events
sc20 <- subset(sc20, events == "single" |
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
sc20 <- subset(sc20, strikes!=2 | description!="foul")


# Getting rid of pitchouts, bunt sequences, and hit by pitch
sc20 <- subset(sc20, description == "hit_into_play" |
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

sc20 <- sc20 %>%
  mutate(oc = case_when(description=="called_strike" | description=="swinging_strike" | description=="foul" | description=="swinging_strike_blocked" | description=="foul_tip" ~ "S",
                        description=="blocked_ball" | description=="ball" | description=="intent_ball" ~ "B",
                        description=="hit_into_play" | description == "hit_into_play_no_out" | description == "hit_into_play_score" ~"X"))


# Creating Run Values
sc20 <- sc20 %>%
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
    balls==0 & strikes==0 & oc=="X" ~ (xwOBA-.310)/1.185,
    balls==1 & strikes==0 & oc=="X" ~ (xwOBA-.355)/1.185,
    balls==2 & strikes==0 & oc=="X" ~ (xwOBA-.436)/1.185,
    balls==3 & strikes==0 & oc=="X" ~ (xwOBA-.622)/1.185,
    balls==0 & strikes==1 & oc=="X" ~ (xwOBA-.262)/1.185,
    balls==1 & strikes==1 & oc=="X" ~ (xwOBA-.293)/1.185,
    balls==2 & strikes==1 & oc=="X" ~ (xwOBA-.352)/1.185,
    balls==3 & strikes==1 & oc=="X" ~ (xwOBA-.470)/1.185,
    balls==0 & strikes==2 & oc=="X" ~ (xwOBA-.196)/1.185,
    balls==1 & strikes==2 & oc=="X" ~ (xwOBA-.196)/1.185,
    balls==2 & strikes==2 & oc=="X" ~ (xwOBA-.273)/1.185,
    balls==3 & strikes==2 & oc=="X" ~ (xwOBA-.352)/1.185))

# Setting up a dummy variable for RHB vs RHP
sc20 <- sc20 %>%
  mutate(right_hit = ifelse(stand == "R", 1, 0), right_pitch = ifelse(p_throws == "R", 1, 0))

# Dummy Variable for Fastball
sc20 <- sc20 %>%
  mutate(isFB = ifelse(pitch_type == "FA" | pitch_type == "FT" | pitch_type == "FF" | pitch_type == "FC", 1, 0))

# Dallas Keuchel 
DK <- sc20 %>%
  dplyr::select(player_name, events, description, des, balls, strikes, pitch_name, rv) %>%
  filter(player_name == "Dallas Keuchel")

# Getting the columns of the data needed for the random forest
s20 <- sc20 %>%
  dplyr::select(release_speed, plate_x, plate_z, pfx_x, pfx_z, release_spin_rate, rv, right_hit, right_pitch, isFB)

s20 <- s20 %>%
  na.omit(s20)

s20 <- s20 %>%
  na.exclude(s20)

# Getting a 50,000 observation sample
set.seed(3) #Number means nothing
ss20 <- sample(1:nrow(s20), 50000)

## Fitting Regression Trees ##
train = sample(1:nrow(s20), nrow(s20)/2)

# Random Forest #
set.seed(3)

rf.s20 = randomForest(rv~., data=s20, subset = ss20,
                      mtry = 3, importance = TRUE)

importance(rf.s20)

varImpPlot(rf.s20)

# Save the model
saveRDS(rf.s20, "./rf20.rds")

# Load Model
rfs20 <- readRDS("./rf20.rds")
print(rfs20)
rfs20_2 <- readRDS("./rf20_2.rds")

# Finding the correlation between variables
c <- cor(s20)
corrplot(c, method = "number", type = "lower")

# Make RV Prediction
sc20 <- sc20 %>% 
  drop_na(plate_x, plate_z, 
          pfx_x, pfx_z, release_speed, release_spin_rate,
          right_pitch, right_hit, isFB) %>% 
  mutate(
    xRV = round(predict(rfs20_2, .), 3)
  ) 

worst <- subset(sc20, xRV >= .16)
best <- subset(sc20, xRV <= -.2)

library(Boruta)
dataSample <- sc20[sample(nrow(sc20), size = nrow(sc20)*.10),]

Boruta_test <- Boruta(xRV ~ release_speed + plate_x + plate_z + pfx_x + pfx_z + release_spin_rate +
                        right_hit + right_pitch, data = dataSample)

print(Boruta_test)
plot(Boruta_test)

Boruta_stats <- data.frame(attStats(Boruta_test))

Boruta_stats %>%
  dplyr::select(meanImp, decision)

#importance(rf.s20)

testing <- s20[-train,]
predictions <- predict(rfs20, testing)
rmse(testing$rv, predictions) #=0.06272389

## Leaderboards

GC <- sc20 %>%
  dplyr::select(pitch_type, player_name, plate_x, plate_z, 
                pfx_x, pfx_z, release_speed, release_spin_rate,
                right_pitch, right_hit, isFB, des, xRV) %>%
  filter(player_name == "Garrett Crochet")

look <- sc20 %>%
  dplyr::select(pitch_type, player_name, plate_x, plate_z, 
                pfx_x, pfx_z, release_speed, release_spin_rate,
                right_pitch, right_hit, isFB, des, launch_speed, launch_angle,balls, strikes, xwOBA, xRV)
  

# RV Leaderboard
rv_leader_pitch2020 <- sc20 %>% 
  dplyr::select(pitch_type, player_name, plate_x, plate_z, 
         pfx_x, pfx_z, release_speed, release_spin_rate,
         right_pitch, right_hit, isFB, xRV) %>% 
  group_by(player_name, pitch_type) %>%
  dplyr::mutate(
    N = n()
  ) %>% 
  filter(N > 50) %>% 
  summarise_all(mean, na.rm = T) %>% 
  arrange(xRV) %>% 
  dplyr::select(player_name, pitch_type, xRV, N, everything())

#save(rv_leader_pitch2020, file = "rv_leader_pitch2020.RData")
load("rv_leader_pitch2020.RData")

# RV Leader for arsenal, weighted by use
rv_leader_arsenal2020 <- rv_leader_pitch2020 %>% 
  group_by(player_name) %>% 
  mutate(
    pct = N/sum(N),
    rv.weight = xRV*pct
  ) %>%
  filter(sum(N)>=400) %>%
  summarise(
    rv.ars = sum(rv.weight),
    pitches = sum(N)
  ) %>% 
  arrange(rv.ars)

#save(rv_leader_arsenal2020, file = "rv_leader_arsenal2020.RData")
load('rv_leader_arsenal2020.RData')

avg_stuff <- sc20 %>% 
  dplyr::select(pitch_type, pfx_x, pfx_z, release_speed, release_spin_rate,
         right_pitch, right_hit) %>% 
  group_by(right_pitch, right_hit, pitch_type) %>% 
  summarise_all(mean)

avg_stuff_quantile <- sc20 %>% 
  dplyr::select(pitch_type, pfx_x, pfx_z, release_speed, release_spin_rate,
         right_pitch, right_hit) %>% 
  group_by(right_pitch, right_hit, pitch_type) %>% 
  summarise_all(.funs = function(x) list(enframe(quantile(x, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE)))) %>% 
  unnest(c(pfx_x, pfx_z, release_speed, release_spin_rate), names_sep = "_") %>% 
  rename_all(funs(str_replace(.,"_name", "_pct"))) %>% 
  rename_all(funs(str_remove(.,"_value")))

# Join Average stuff to pitcher locations and run model
location_avgstuff <- sc20 %>% 
  dplyr::select(player_name, pitch_type, right_pitch, right_hit, plate_x, plate_z, isFB) %>% 
  left_join(.,avg_stuff) %>% 
  mutate(
    xRV = predict(rfs20_2, .) #or rfs20
  ) %>% 
  group_by(player_name, pitch_type) %>% 
  mutate(
    N = n()
  ) %>% 
  filter(N > 50) %>% 
  summarise_all(mean) %>% 
  dplyr::select(player_name, pitch_type, xRV, N) %>% 
  arrange(xRV)

save(Command_rv_leader_arsenal2020, file = "Command_rv_leader_arsenal2020")
load("Command_rv_leader_arsenal2020")

# Holding stuff constant -- pitcher arsenals with only command
Command_rv_leader_arsenal2020 <- location_avgstuff %>% 
  group_by(player_name) %>% 
  mutate(
    pct = N/sum(N),
    rv.weight = xRV*pct
  ) %>%
  filter(sum(N)>=400) %>%
  summarise(
    rv.ars = sum(rv.weight),
    pitches = sum(N)
  ) %>% 
  arrange(rv.ars)

save(location_avgstuff, file = "location_avgstuff2020")
load("location_avgstuff2020")

#Actual vs. Expected Run Values
actual_expected <- sc20 %>% 
  dplyr::select(player_name, pitch_type, rv, xRV) %>% 
  mutate(
    Diff = rv - xRV
  ) %>% 
  group_by(player_name, pitch_type) %>% 
  mutate(
    N = n()
  ) %>%
  filter(N > 50) %>% 
  summarise_all(mean, na.rm = TRUE) %>% 
  arrange(Diff)

actual_expected <- actual_expected %>%
  mutate_at(vars(rv,xRV,Diff), funs(round(., 5)))

#Actual vs. Expected Run Values Arsenal
actual_expected_arsenal <- actual_expected %>%
  group_by(player_name) %>%
  mutate(
    pct = N/sum(N),
    rv.weight = rv*pct,
    xRV.weight = xRV*pct
  ) %>%
  summarise(
    rv.ars = sum(rv.weight),
    xRV.ars = sum(xRV.weight),
    pitches = sum(N)
  )
  
actual_expected_arsenal <- actual_expected_arsenal %>%
  mutate(Diff = rv.ars - xRV.ars) %>%
  mutate_at(vars(rv.ars,xRV.ars,Diff), funs(round(., 5))) %>%
  arrange(-Diff)

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

# Compare two players
player_stuff <- sc20 %>%
  dplyr::select(player_name, pitch_type, pfx_x, pfx_z, release_speed, release_spin_rate,
         right_pitch) %>% 
  group_by(player_name, pitch_type) %>% 
  summarise_all(mean)

player_zone_compare <- left_join(plate_grid("FF"), filter(player_stuff, player_name == "Dylan Cease" | player_name == "Gerrit Cole", pitch_type == "FF")) %>% 
  mutate(
    isFB = 1,
    right_hit = 1
  ) %>% 
  mutate(
    xRV = predict(rf.s20,.)
  ) %>% 
  group_by(player_name, zone) %>% 
  summarise(
    zone_xRV = mean(xRV)
  ) %>% 
  arrange(zone)

## Velo Effect on RV
velo_FF <- tibble(pitch_type = "FF",
                  plate_x = 0, 
                  plate_z = 2.5, 
                  release_speed = rep(87:100, 2),
                  right_pitch = 1,
                  right_hit =1,
                  release_spin_rate = rep(c(2100, 2500), each = 14),
                  isFB = 1
) %>%  
  left_join(., dplyr::select(avg_stuff, -release_speed, -release_spin_rate)) %>% 
  mutate(
    xRV = predict(rf.s20,.)
  )

ggplot(velo_FF, aes(x = release_speed, y = xRV)) +
  geom_line(aes(color = as.factor(release_spin_rate))) +
  theme_minimal()

#Compare 2020 & 2019
load("rv_leader_pitch2019.RData")
load("rv_leader_arsenal2019.RData")
compare_xRV <- merge(rv_leader_pitch2020, rv_leader_pitch, by = c("player_name", "pitch_type"), na.rm = TRUE)
compare_xRV$diff <-  compare_xRV$xRV - compare_xRV$rv.hat

compare_ars <- merge(rv_leader_arsenal2020, rv_leader_arsenal, by = "player_name", na.rm = TRUE)
compare_ars$ars_dif <- compare_ars$rv.ars.x - compare_ars$rv.ars.y
compare_ars$BothYears <- compare_ars$rv.ars.x + compare_ars$rv.ars.y




