library(tidyverse)
library(haven)
library(formattable)
library(dplyr)
library(sportyR)
library(ggplot2)
library(gganimate)
library(teamcolors)

players <- read.csv("~/Desktop/Grad School/DA 63/Group Project/players.csv",header = TRUE)
plays <- read.csv("~/Desktop/Grad School/DA 63/Group Project/plays.csv",header = TRUE)
games <- read.csv("~/Desktop/Grad School/DA 63/Group Project/games.csv",header = TRUE)
week1 <- read.csv("~/Desktop/Grad School/DA 63/Group Project/week1.csv",header = TRUE)
week2 <- read.csv("~/Desktop/Grad School/DA 63/Group Project/week2.csv",header = TRUE)
week3 <- read.csv("~/Desktop/Grad School/DA 63/Group Project/week3.csv",header = TRUE)
week4 <- read.csv("~/Desktop/Grad School/DA 63/Group Project/week4.csv",header = TRUE)
week5 <- read.csv("~/Desktop/Grad School/DA 63/Group Project/week5.csv",header = TRUE)
week6 <- read.csv("~/Desktop/Grad School/DA 63/Group Project/week6.csv",header = TRUE)
week7 <- read.csv("~/Desktop/Grad School/DA 63/Group Project/week7.csv",header = TRUE)
week8 <- read.csv("~/Desktop/Grad School/DA 63/Group Project/week8.csv",header = TRUE)
week9 <- read.csv("~/Desktop/Grad School/DA 63/Group Project/week9.csv",header = TRUE)

#week1
week1_merged <- week1 %>% inner_join(games) %>% inner_join(plays) 
week1_filter <- week1_merged %>% filter(gameId == '2018090910' | gameId == '2018090600')

#week2
week2_merged <- week2 %>% inner_join(games) %>% inner_join(plays) 
week2_filter <- week2_merged %>% filter(gameId == '2018091613' | gameId == '2018091606')

#week3
week3_merged <- week3 %>% inner_join(games) %>% inner_join(plays) 
week3_filter <- week3_merged %>% filter(gameId == '2018092312'| gameId == '2018092308')

#week4
week4_merged <- week4 %>% inner_join(games) %>% inner_join(plays) 
week4_filter <- week4_merged %>% filter(gameId == '2018093002'| gameId == '2018093007')

#week5
week5_merged <- week5 %>% inner_join(games) %>% inner_join(plays) 
week5_filter <- week5_merged %>% filter(gameId == '2018100712'| gameId == '2018100709')

#week6
week6_merged <- week6 %>% inner_join(games) %>% inner_join(plays) 
week6_filter <- week6_merged %>% filter(gameId == '2018101410'| gameId == '2018101100')

#week7
week7_merged <- week7 %>% inner_join(games) %>% inner_join(plays) 
week7_filter <- week7_merged %>% filter(gameId == '2018102110'| gameId == '2018102108')

#week8Dallas off week
week8_merged <- week8 %>% inner_join(games) %>% inner_join(plays) 
week8_filter <- week8_merged %>% filter(gameId == '2018102800')



#week1
theme_set(theme_bw())
week1_PHI <- week1_filter %>%
  filter(gameId== "2018090600") %>%
  group_by(playId,frameId) %>%
  mutate(new_frameId = group_indices())

week1_PHI[week1_PHI['team'] == 'home', 'color'] = '#004953' #eagles
week1_PHI[week1_PHI['team'] == 'away', 'color'] = '#a71930' #Falcons 
week1_PHI[week1_PHI['team'] == 'football', 'color'] = '#624a2e'

#length(unique(week1_PHI$playId))
game_anim1 = geom_football(league = "NFL",full_surf = T,rotate = F) +
  geom_point(data = week1_PHI, aes(x, y), 
             size = 5 , color = week1_PHI$color) +
  geom_text(data = week1_PHI, aes(x, y, label = jerseyNumber), 
            colour = "white", vjust = 0.36, size = 3.5) +
  transition_time(week1_PHI$new_frameId)

# Show the animation
animate(game_anim1, nframes = 89, fps = 8, duration = 712) +
  labs(title = "Eagles vs Falcons")


#week2
theme_set(theme_bw())
week2_PHI <- week2_filter %>%
  filter(gameId== "2018091606") %>%
  group_by(playId,frameId) %>%
  mutate(new_frameId = group_indices())

week2_PHI[week2_PHI['team'] == 'away', 'color'] = '#004953' #eagles
week2_PHI[week2_PHI['team'] == 'home', 'color'] = '#d50a0a' #Buccaneers
week2_PHI[week2_PHI['team'] == 'football', 'color'] = '#624a2e'

game_anim2 = geom_football(league = "NFL",full_surf = T,rotate = F) +
  geom_point(data = week2_PHI, aes(x, y), 
             size = 5 , color = week2_PHI$color) +
  geom_text(data = week2_PHI, aes(x, y, label = jerseyNumber), 
            colour = "white", vjust = 0.36, size = 3.5) +
  transition_time(week2_PHI$new_frameId)

n2 <- length(unique(week2_PHI$playId))
d2 <- n2*8
# Show the animation
animate(game_anim2, nframes = n2 , fps = 8, duration = d2) +
  labs(title = "Eagles vs Buccaneers")


#week3
theme_set(theme_bw())
week3_PHI <- week3_filter %>%
  filter(gameId== "2018092308") %>%
  group_by(playId,frameId) %>%
  mutate(new_frameId = group_indices())

week3_PHI[week3_PHI['team'] == 'home', 'color'] = '#004953' #eagles
week3_PHI[week3_PHI['team'] == 'away', 'color'] = '#002c5f' # Colts
week3_PHI[week3_PHI['team'] == 'football', 'color'] = '#624a2e'


game_anim3 = geom_football(league = "NFL",full_surf = T,rotate = F) +
  geom_point(data = week3_PHI, aes(x, y), 
             size = 5 , color = week3_PHI$color) +
  geom_text(data = week3_PHI, aes(x, y, label = jerseyNumber), 
            colour = "white", vjust = 0.36, size = 3.5) +
  transition_time(week3_PHI$new_frameId)
n3 <- length(unique(week3_PHI$playId))
d3 <- n3*8
# Show the animation
animate(game_anim3, nframes = n3, fps = 8, duration = d3) +
  labs(title = "Eagles vs Colts")


#week4
theme_set(theme_bw())
week4_PHI <- week4_filter %>%
  filter(gameId== "2018093007") %>%
  group_by(playId,frameId) %>%
  mutate(new_frameId = group_indices())

week4_PHI[week4_PHI['team'] == 'away', 'color'] = '#004953' #eagles
week4_PHI[week4_PHI['team'] == 'home', 'color'] = '#002244' #Titans
week4_PHI[week4_PHI['team'] == 'football', 'color'] = '#624a2e'


game_anim4 = geom_football(league = "NFL",full_surf = T,rotate = F) +
  geom_point(data = week4_PHI, aes(x, y), 
             size = 5 , color = week4_PHI$color) +
  geom_text(data = week4_PHI, aes(x, y, label = jerseyNumber), 
            colour = "white", vjust = 0.36, size = 3.5) +
  transition_time(week4_PHI$new_frameId)
n4 <-length(unique(week4_PHI$playId))
d4 <- n4*8
# Show the animation
animate(game_anim4, nframes = n4, fps = 8, duration = d4) +
  labs(title = "Eagles vs Titans")


#week5
theme_set(theme_bw())
week5_PHI <- week5_filter %>%
  filter(gameId== "2018100709") %>%
  group_by(playId,frameId) %>%
  mutate(new_frameId = group_indices())

week5_PHI[week5_PHI['team'] == 'home', 'color'] = '#004953' #eagles
week5_PHI[week5_PHI['team'] == 'away', 'color'] = '#4f2683' #Vikings
week5_PHI[week5_PHI['team'] == 'football', 'color'] = '#624a2e'

game_anim5 = geom_football(league = "NFL",full_surf = T,rotate = F) +
  geom_point(data = week5_PHI, aes(x, y), 
             size = 5 , color = week5_PHI$color) +
  geom_text(data = week5_PHI, aes(x, y, label = jerseyNumber), 
            colour = "white", vjust = 0.36, size = 3.5) +
  transition_time(week5_PHI$new_frameId)
n5 <- length(unique(week5_PHI$playId))
d5 <-n5*8
# Show the animation
animate(game_anim5, nframes = n5, fps = 8, duration = d5) +
  labs(title = "Eagles vs Vikings")



#week6
theme_set(theme_bw())
week6_PHI <- week6_filter %>%
  filter(gameId== "2018101100") %>%
  group_by(playId,frameId) %>%
  mutate(new_frameId = group_indices())

week6_PHI[week6_PHI['team'] == 'away', 'color'] = '#004953' #eagles
week6_PHI[week6_PHI['team'] == 'home', 'color'] = '#0b2265' #Giants
week6_PHI[week6_PHI['team'] == 'football', 'color'] = '#624a2e'


game_anim6 = geom_football(league = "NFL",full_surf = T,rotate = F) +
  geom_point(data = week6_PHI, aes(x, y), 
             size = 5 , color = week6_PHI$color) +
  geom_text(data = week6_PHI, aes(x, y, label = jerseyNumber), 
            colour = "white", vjust = 0.36, size = 3.5) +
  transition_time(week6_PHI$new_frameId)
n6 <- length(unique(week6_PHI$playId))
d6 <-n6*8
# Show the animation
animate(game_anim6, nframes = n6, fps = 8, duration = d6) +
  labs(title = "Eagles vs Giants")




#week7
theme_set(theme_bw())
week7_PHI <- week7_filter %>%
  filter(gameId== "2018102108") %>%
  group_by(playId,frameId) %>%
  mutate(new_frameId = group_indices())

week7_PHI[week7_PHI['team'] == 'home', 'color'] = '#004953' #eagles
week7_PHI[week7_PHI['team'] == 'away', 'color'] = '#0085ca' #Panthers
week7_PHI[week7_PHI['team'] == 'football', 'color'] = '#624a2e'


game_anim7 = geom_football(league = "NFL",full_surf = T,rotate = F) +
  geom_point(data = week7_PHI, aes(x, y), 
             size = 5 , color = week7_PHI$color) +
  geom_text(data = week7_PHI, aes(x, y, label = jerseyNumber), 
            colour = "white", vjust = 0.36, size = 3.5) +
  transition_time(week7_PHI$new_frameId)

n7 <- length(unique(week7_PHI$playId))
d7 <- n7*8
# Show the animation
animate(game_anim7, nframes = n7, fps = 8, duration = d7) +
  labs(title = "Eagles vs Panthers")




#week8
theme_set(theme_bw())
week8_PHI <- week8_filter %>%
  filter(gameId== "2018102800") %>%
  group_by(playId,frameId) %>%
  mutate(new_frameId = group_indices())

week8_PHI[week8_PHI['team'] == 'away', 'color'] = '#004953' #eagles
week8_PHI[week8_PHI['team'] == 'home', 'color'] = '#000000' #Jaguars "#006778" 
week8_PHI[week8_PHI['team'] == 'football', 'color'] = '#624a2e'


game_anim8 = geom_football(league = "NFL",full_surf = T,rotate = F) +
  geom_point(data = week8_PHI, aes(x, y), 
             size = 5 , color = week8_PHI$color) +
  geom_text(data = week8_PHI, aes(x, y, label = jerseyNumber), 
            colour = "white", vjust = 0.36, size = 3.5) +
  transition_time(week8_PHI$new_frameId)
n8 <- length(unique(week8_PHI$playId))
d8 <- n8 *8
# Show the animation
animate(game_anim8, nframes = n8, fps = 8, duration = d8) + 
  labs(title = "Eagles vs Jaguars")