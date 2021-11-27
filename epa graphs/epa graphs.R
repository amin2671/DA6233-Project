library(tidyverse); library(plotly); library(highcharter); theme_set(theme_minimal())

setwd("~/HOMEWORK FOLDER - FALL 2021/DA 6233/PROJECT/nfl-big-data-bowl-2021") # set your own path

playsOut = read.csv("out.csv", header=TRUE) 
# out.csv is just plays.csv but with more columns that I needed


# While PHI is not in possession
p3 <- playsOut %>%
  group_by(offenseFormation, nonPossession) %>%
  filter(nonPossession == "PHI") %>%
  mutate(meanEPA = mean(epa))

p32 <- playsOut %>%
  group_by(offenseFormation, possessionTeam) %>%
  filter(possessionTeam == "PHI") %>%
  mutate(meanEPA = mean(epa))

# While PHI is in possession
p4 <- playsOut %>%
  group_by(playType) %>%
  filter(possessionTeam == "PHI") %>%
  mutate(meanEPA = mean(epa))

p5 <- playsOut %>%
  group_by(playType) %>%
  filter(nonPossession == "PHI") %>%
  mutate(meanEPA = mean(epa))

# Boxplot of EPA for offensive formations while PHI is not in possession
G2 <- p3 %>%
  ggplot(aes(x=offenseFormation, y=epa)) + 
  labs(title="EPA for Offensive Formations while not in Possession",
       x = "Offensive Formations", y = "EPA") +
  geom_boxplot() + 
  geom_point(shape=17, size=1, color="red", aes(x=offenseFormation, y=meanEPA)) +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))

ggplotly(G2)

# Boxplot of EPA for offensive formations while PHI is in possession
G22 <- p32 %>%
  ggplot(aes(x=offenseFormation, y=epa)) + 
  labs(title="EPA for Offensive Formations while in Possession",
       x = "Offensive Formations", y = "EPA") +
  geom_boxplot() + 
  geom_point(shape=17, size=1, color="red", aes(x=offenseFormation, y=meanEPA)) +
  theme(legend.position = "none") + 
  scale_x_discrete(limits=c("EMPTY","I_FORM","JUMBO", "PISTOL", "SHOTGUN", "SINGLEBACK")) +
  theme(plot.title = element_text(hjust = 0.5))

ggplotly(G22)


# Boxplot of EPA for play types while PHI is in possession
G3 <- p4 %>% 
  ggplot(aes(x=playType, y=epa)) + 
  labs(title="EPA for Play Types while in Possession",
       x = "Play Type", y = "EPA") +
  geom_boxplot() + 
  geom_point(shape=17, size=1, color="red", aes(x=playType, y=meanEPA)) +
  theme(legend.position = "none") + 
  scale_x_discrete(breaks=c("play_type_pass","play_type_sack","play_type_unknown"), 
                   labels=c("Pass", "Sack", "No Play")) +
  theme(plot.title = element_text(hjust = 0.5))

ggplotly(G3)

# Boxplot of EPA for play types while PHI is not in possession
G4 <- p5 %>% 
  ggplot(aes(x=playType, y=epa)) + 
  labs(title="EPA for Play Types while not in Possession",
       x = "Play Type", y = "EPA") +
  geom_boxplot() + 
  geom_point(shape=17, size=1, color="red", aes(x=playType, y=meanEPA)) +
  theme(legend.position = "none") + 
  scale_x_discrete(breaks=c("play_type_pass","play_type_sack","play_type_unknown"), 
                   labels=c("Pass", "Sack", "No Play")) +
  theme(plot.title = element_text(hjust = 0.5))

ggplotly(G4)




