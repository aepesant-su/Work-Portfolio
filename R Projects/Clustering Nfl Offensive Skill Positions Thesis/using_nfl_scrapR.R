library(nflscrapR)
library(tidyverse)
library(dplyr)
library(na.tools)
library(ggimage)

pbp <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2018.csv"))

pbp_rp <- pbp %>% 
  filter(!is_na(epa), play_type=="no_play" | play_type=="pass" | play_type=="run")

pbp_rp <- pbp_rp %>%
  mutate(
    pass = if_else(str_detect(desc, "( pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
    success = ifelse(epa>0, 1 , 0)
  )

pbp_rp <- pbp_rp %>% filter(pass==1 | rush==1)

pbp_rp %>%
  filter(posteam == "LA", rush == 1, down<=4) %>%
  group_by(rusher_player_name) %>%
  summarize(mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()) %>%
  arrange(desc(mean_epa)) %>%
  filter(plays>40)

rush_data <- pbp_rp %>%
  filter(rush == 1, down<=4) %>%
  group_by(rusher_player_name) %>%
  summarize(mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()) %>%
  arrange(desc(mean_epa)) %>%
  filter(plays>40)

ggplot(rush_data, aes(x = mean_epa, y = success_rate)) + geom_point(aes(size=plays)) + geom_smooth() + ggtitle("Success Rate vs. EPA 2018 Running Backs")

pass_data <- pbp_rp %>%
  filter(pass == 1, down<=4) %>%
  group_by(passer_player_name) %>%
  summarize(mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()) %>%
  arrange(desc(mean_epa)) %>%
  filter(plays>40)           

ggplot(pass_data, aes(x = mean_epa, y = success_rate)) + geom_point(aes(size=plays)) + geom_smooth() + ggtitle("Success Rate vs. EPA 2018 QuarterBacks")

receiving_data <- pbp_rp %>%
  filter(pass == 1, down<=4) %>%
  group_by(receiver_player_name) %>%
  summarize(mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()) %>%
  arrange(desc(mean_epa)) %>%
  filter(plays>40)

receiving_data <- na.omit(receiving_data)

ggplot(receiving_data, aes(x = mean_epa, y = success_rate)) + geom_point(aes(size=plays)) + geom_smooth() + ggtitle("Success Rate vs. EPA 2018 Wide Recievers")
