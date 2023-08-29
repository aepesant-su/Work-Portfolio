#necessary packages
library(tidyverse)
library(lubridate) 
library(beeswarm) 
library(gganimate)
library(ggridges)  
library(tidyr)
library(cowplot)

#loading in tracking data

tracking2020 <- read_csv('tracking2020.csv')
tracking2019 <- read_csv('tracking2019.csv')
tracking2018 <- read_csv('tracking2018.csv')

tracking2020$FinalID <- paste0(tracking2020$gameId, tracking2020$playId)
tracking2019$FinalID <- paste0(tracking2019$gameId, tracking2019$playId)
tracking2018$FinalID <- paste0(tracking2018$gameId, tracking2018$playId)


#loading in other data

games <- read_csv('games.csv')
PFF <- read_csv('PFFScoutingData.csv')
players <- read_csv('players.csv')
plays <- read_csv('plays.csv')

#keeping punt plays

plays_punt <- plays %>% filter(specialTeamsPlayType == 'Punt')

plays_punt$FinalID <- paste0(plays_punt$gameId, plays_punt$playId)

#play animation as well as filtering tracking data

tracking2020_merged <- tracking2020 %>% inner_join(plays_punt) %>% inner_join(games)

tracking2019_merged <- tracking2019 %>% inner_join(plays_punt) %>% inner_join(games)

tracking2018_merged <- tracking2018 %>% inner_join(plays_punt) %>% inner_join(games)

play <- tracking2018_merged %>% filter(FinalID == 2018090600366)

play %>% select(playDescription) %>% slice(1)

## General field boundaries
xmin <- 0
xmax <- 160/3
hash.right <- 38.35
hash.left <- 12
hash.width <- 3.3


## Specific boundaries for a given play
ymin <- max(round(min(play$x, na.rm = TRUE) - 10, -1), 0)
ymax <- min(round(max(play$x, na.rm = TRUE) + 10, -1), 120)
df_hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df_hash <- df_hash %>% filter(!(floor(y %% 5) == 0))
df_hash <- df_hash %>% filter(y < ymax, y > ymin)

animate_play <- ggplot() +
  scale_size_manual(values = c(6, 4, 6), guide = FALSE) + 
  scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
  scale_fill_manual(values = c("#e31837", "#654321", "#002244"), guide = FALSE) + 
  scale_colour_manual(values = c("black", "#654321", "#c60c30"), guide = FALSE) + 
  annotate("text", x = df_hash$x[df_hash$x < 55/2], 
           y = df_hash$y[df_hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
  annotate("text", x = df_hash$x[df_hash$x > 55/2], 
           y = df_hash$y[df_hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) + 
  annotate("segment", x = xmin, 
           y = seq(max(10, ymin), min(ymax, 110), by = 5), 
           xend =  xmax, 
           yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
  annotate("text", x = rep(hash.left, 11), y = seq(10, 110, by = 10), 
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
           angle = 270, size = 4) + 
  annotate("text", x = rep((xmax - hash.left), 11), y = seq(10, 110, by = 10), 
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
           angle = 90, size = 4) + 
  annotate("segment", x = c(xmin, xmin, xmax, xmax), 
           y = c(ymin, ymax, ymax, ymin), 
           xend = c(xmin, xmax, xmax, xmin), 
           yend = c(ymax, ymax, ymin, ymin), colour = "black") + 
  geom_point(data = play, aes(x = (xmax-y), y = x, shape = team,
                                      fill = team, group = nflId, size = team, colour = team), alpha = 0.7) + 
  geom_text(data = play, aes(x = (xmax-y), y = x, label = jerseyNumber), colour = "white", 
            vjust = 0.36, size = 3.5) + 
  ylim(ymin, ymax) + 
  coord_fixed() +  
  theme_nothing() + 
  transition_time(frameId)  +
  ease_aes('linear') + 
  NULL

## Ensure timing of play matches 10 frames-per-second
play.length.ex <- length(unique(play$frameId))

#to animate play
animate(animate_play, fps = 10, nframe = play.length.ex)

###More Data Manipulation

tracking <- rbind(tracking2018_merged,tracking2019_merged,tracking2020_merged)

#write_csv(tracking, "punt_tracking_data.csv")

##########start here#############

tracking <- read_csv("punt_tracking_data.csv")

#using df_tracking to merge to jersey numbers
df_jerseyMap <- tracking %>% 
  
  #selecting variables of interest
  select(gameId, team, jerseyNumber, nflId) %>%
  
  #dropping duplicates - jersey # is constant throughout game
  distinct() %>%
  
  #joining to games
  inner_join(games, by = 'gameId') %>%
  
  #getting name of team
  mutate(team = ifelse(team == 'home', homeTeamAbbr, visitorTeamAbbr),
         
         #adjusting jersey number so that it includes 0 when < 10
         jerseyNumber = ifelse(jerseyNumber < 10,
                               paste0('0', jerseyNumber),
                               as.character(jerseyNumber)),
         
         #getting team and jersey
         teamJersey = paste0(team, ' ', jerseyNumber)) %>%
  
  #map to merge nflId to teamJersey
  select(gameId, nflId, teamJersey) %>%
  
  arrange(gameId, nflId, teamJersey)

####################################################################################

###Gunner
df_PFF_specialGunners <- PFF %>%
  
  #splitting into a column for each special teams gunner
  separate(gunners, sep = '; ', into = c("teamJersey1",
                                                      "teamJersey2",
                                                      "teamJersey3",
                                                      "teamJersey4",
                                                      "teamJersey5",
                                                      "teamJersey6",
                                                      "teamJersey7",
                                                      "teamJersey8",
                                                      "teamJersey9",
                                                      "teamJersey10",
                                                      "teamJersey11")) %>%
  
  
  #selecting jersey numbers for each team
  select(gameId, playId, teamJersey1, teamJersey2,
         teamJersey3, teamJersey4, teamJersey5,
         teamJersey6, teamJersey7, teamJersey8,
         teamJersey9, teamJersey10, teamJersey11) %>%
  
  #gathering data
  gather(key = 'Type', value = 'teamJersey', -gameId, -playId) %>%
  
  #dropping NA rows
  drop_na() %>%
  
  #joining to jersey map
  inner_join(df_jerseyMap, by = c("gameId", 'teamJersey')) %>%
  
  #selecting variables of interest
  select(gameId, playId, nflId) %>%
  
  arrange(gameId, playId, nflId)

###Vises

df_PFF_specialVises <- PFF %>%
  
  #splitting into a column for each special teams vise
  separate(vises, sep = '; ', into = c("teamJersey1",
                                         "teamJersey2",
                                         "teamJersey3",
                                         "teamJersey4",
                                         "teamJersey5",
                                         "teamJersey6",
                                         "teamJersey7",
                                         "teamJersey8",
                                         "teamJersey9",
                                         "teamJersey10",
                                         "teamJersey11")) %>%
  
  
  #selecting jersey numbers for each team
  select(gameId, playId, teamJersey1, teamJersey2,
         teamJersey3, teamJersey4, teamJersey5,
         teamJersey6, teamJersey7, teamJersey8,
         teamJersey9, teamJersey10, teamJersey11) %>%
  
  #gathering data
  gather(key = 'Type', value = 'teamJersey', -gameId, -playId) %>%
  
  #dropping NA rows
  drop_na() %>%
  
  #joining to jersey map
  inner_join(df_jerseyMap, by = c("gameId", 'teamJersey')) %>%
  
  #selecting variables of interest
  select(gameId, playId, nflId) %>%
  
  arrange(gameId, playId, nflId)

###getting max speeds for gunners before punt return

unique(tracking$event)

df_maxSpeeds <- tracking2020 %>%
  
  #joining games
  inner_join(games, by = 'gameId') %>%
  
  #only including gunners
  inner_join(df_PFF_specialGunners1, by = c("gameId", 'playId', 'nflId'))  %>%
  
  #joining punt plays
  inner_join(plays_punt1, by = c('gameId', 'playId')) %>%
  
  #removing the kicker from the tracking data
  filter(kickerId != nflId,
         
         #player is on home team and kicking team is home
         ((team == 'home') & (possessionTeam == homeTeamAbbr)) |
           ((team == 'away') & (possessionTeam == visitorTeamAbbr))  ) %>%
  
  filter(cumsum(event %in% c('punt', 'punt_received')) >= 1) %>%
  
  #ungrouping
  ungroup() %>%
  
  #grouping by gameId, playId and nflId
  group_by(gameId, playId, nflId) %>%
  
  #filtering for first 40 observations
  filter(row_number() <= 40) %>%
  
  #calculating max speed for given play / player
  summarize(maxSpeed = max(s, na.rm = T), .groups = 'keep') %>%
  
  #ungrouping
  ungroup()
