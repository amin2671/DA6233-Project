library(tidyverse); library(plotly); theme_set(theme_minimal())

setwd("~/HOMEWORK FOLDER - FALL 2021/DA 6233/PROJECT/nfl-big-data-bowl-2021")

playsOut = read.csv("out.csv", header=TRUE) 
# out.csv is just plays.csv but with more columns that I needed

########## SIDE NOTE: NEGATIVE EPA = BETTER DEFENSE; POSITIVE EPA = BETTER OFFENSE

# Not in possession
DefenseEPA_NonPossession <- playsOut %>%
  group_by(offenseFormation) %>%
  filter(nonPossession == "PHI") %>%
  mutate(meanEPA = mean(epa))

PlayTypeEPA_NonPossession <- playsOut %>%
  group_by(playType) %>%
  filter(nonPossession == "PHI") %>%
  mutate(meanEPA = mean(epa))

# In possession
OffenseEPA_Possession <- playsOut %>%
  group_by(offenseFormation) %>%
  filter(possessionTeam == "PHI") %>%
  mutate(meanEPA = mean(epa))

PlayTypeEPA_Possession <- playsOut %>%
  group_by(playType) %>%
  filter(possessionTeam == "PHI") %>%
  mutate(meanEPA = mean(epa))



# EPA of Offensive formations while not in possession
PLOT_Defense_Non <- DefenseEPA_NonPossession %>%
  ggplot(aes(x=offenseFormation, y=epa)) + 
  labs(title="EPA for Offensive Formations while not in Possession",
       x = "Offensive Formations", y = "EPA") +
  geom_boxplot() + 
  geom_point(shape=17, size=1, color="red", aes(x=offenseFormation, y=meanEPA)) +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5))

ggplotly(PLOT_Defense_Non)


# EPA for play types while not in possession
PLOT_PlayType_Non <- PlayTypeEPA_NonPossession %>% 
  ggplot(aes(x=playType, y=epa)) + 
  labs(title="EPA for Play Types while not in Possession",
       x = "Play Type", y = "EPA") +
  geom_boxplot() + 
  geom_point(shape=17, size=1, color="red", aes(x=playType, y=meanEPA)) +
  theme(legend.position = "none") + 
  scale_x_discrete(breaks=c("play_type_pass","play_type_sack","play_type_unknown"), 
                   labels=c("Pass", "Sack", "No Play")) +
  theme(plot.title = element_text(hjust = 0.5))

ggplotly(PLOT_PlayType_Non)

# ABOVE IS FOR NONPOSSESSION
######################################################
# BELOW IS FOR POSSESSION

# EPA for offensive formations while in possession
PLOT_Offense_Pos <- OffenseEPA_Possession %>%
  ggplot(aes(x=offenseFormation, y=epa)) + 
  labs(title="EPA for Offensive Formations while in Possession",
       x = "Offensive Formations", y = "EPA") +
  geom_boxplot() + 
  geom_point(shape=17, size=1, color="red", aes(x=offenseFormation, y=meanEPA)) +
  theme(legend.position = "none") + 
  scale_x_discrete(limits=c("EMPTY","I_FORM","JUMBO", "PISTOL", "SHOTGUN", "SINGLEBACK")) +
  theme(plot.title = element_text(hjust = 0.5))

ggplotly(PLOT_Offense_Pos)


# EPA for play types while in possession
PLOT_PlayType_Pos <- PlayTypeEPA_Possession %>% 
  ggplot(aes(x=playType, y=epa)) + 
  labs(title="EPA for Play Types while in Possession",
       x = "Play Type", y = "EPA") +
  geom_boxplot() + 
  geom_point(shape=17, size=1, color="red", aes(x=playType, y=meanEPA)) +
  theme(legend.position = "none") + 
  scale_x_discrete(breaks=c("play_type_pass","play_type_sack","play_type_unknown"), 
                   labels=c("Pass", "Sack", "No Play")) +
  theme(plot.title = element_text(hjust = 0.5))

ggplotly(PLOT_PlayType_Pos)