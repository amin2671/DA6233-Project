library(tidyverse); library(plotly); library(highcharter); theme_set(theme_minimal())

setwd("~/HOMEWORK FOLDER - FALL 2021/DA 6233/PROJECT/nfl-big-data-bowl-2021") # set your own path

playsOut = read.csv("out.csv", header=TRUE) 
# out.csv is just plays.csv but with more columns that I needed


# While PHI is not in possession
p3 <- playsOut %>%
  group_by(offenseFormation, nonPossession) %>%
  filter(nonPossession == "PHI") %>%
  mutate(meanEPA = mean(epa))

# While PHI is in possession
p4 <- playsOut %>%
  group_by(playType) %>%
  filter(possessionTeam == "PHI") %>%
  mutate(meanEPA = mean(epa))


# Boxplot of EPA for offensive formations while PHI is not in possession
G2 <- p3 %>%
  ggplot(aes(x=offenseFormation, y=epa)) + 
  geom_boxplot() + 
  geom_point(shape=17, size=1, color="red", aes(x=offenseFormation, y=meanEPA)) +
  theme(legend.position = "none")

ggplotly(G2)


# Boxplot of EPA for play types while PHI is in possession
G3 <- p4 %>% 
  ggplot(aes(x=playType, y=epa)) + 
  geom_boxplot() + 
  geom_point(shape=17, size=1, color="red", aes(x=playType, y=meanEPA)) +
  theme(legend.position = "none")

ggplotly(G3)