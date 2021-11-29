library(tidyverse); library(plotly); theme_set(theme_minimal())

setwd("~/HOMEWORK FOLDER - FALL 2021/DA 6233/PROJECT/nfl-big-data-bowl-2021")

pbp2018 <- read_csv("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2018.csv.gz")
plays = read.csv("plays.csv", header=TRUE)


topReceiversDATA <- pbp2018 %>%
  filter(receiver %in% c("Z.Ertz", "A.Cooper"), week <= 9) %>%
  group_by(week, receiver) %>%
  summarize(mean_epa = mean(epa, na.rm = TRUE))

topRushersDATA <- pbp2018 %>%
  filter(rusher %in% c("E.Elliott", "W.Smallwood"), week <= 9) %>%
  group_by(week, rusher) %>%
  summarize(mean_epa = mean(epa, na.rm = TRUE))


OffenseEPA <- plays %>%
  group_by(offenseFormation) %>%
  filter(possessionTeam == "PHI") %>%
  mutate(meanEPA = mean(epa))

PlayTypeEPA <- plays %>%
  group_by(playType) %>%
  filter(possessionTeam == "PHI") %>%
  mutate(meanEPA = mean(epa))

# EPA for offensive formations
PLOT_Offense_Pos <- OffenseEPA %>%
  ggplot(aes(x=offenseFormation, y=epa)) + 
  labs(title="EPA for Offensive Formations while in Possession",
       x = "Offensive Formations", y = "EPA") +
  geom_boxplot() + 
  geom_point(shape=17, size=1, color="red", aes(x=offenseFormation, y=meanEPA)) +
  theme(legend.position = "none") + 
  scale_x_discrete(limits=c("EMPTY","I_FORM","JUMBO", "PISTOL", "SHOTGUN", "SINGLEBACK")) +
  theme(plot.title = element_text(hjust = 0.5))

ggplotly(PLOT_Offense_Pos)


# EPA for play types
PLOT_PlayType_Pos <- PlayTypeEPA %>% 
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


# Average EPA per week for Top Receiver for Cowboys vs Eagles
receivers <- topReceiversDATA %>% 
  ggplot(aes(x = week, y = mean_epa, color = receiver)) +
  geom_line(size = .75) +
  geom_hline(yintercept = 0, size = .5, color = "black") +
  labs(
    x = "Weeks",
    y = "EPA (Average)",
    title = "Comparison of Cooper (DAL) vs Ertz (PHI)"
  ) +
  geom_point(size = 1.5) +
  scale_x_continuous(breaks = seq(1, 9, 1)) +
  scale_color_manual(values=c('#003594','#004C54')) +
  theme(
    legend.title = element_blank(),
    legend.position = "top"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

ggplotly(receivers)


# Average EPA per week for Top Rusher for Cowboys vs Eagles
rushers <- topRushersDATA %>%
  ggplot(aes(x = week, y = mean_epa, color = rusher)) +
  geom_line(size = .75) +
  geom_hline(yintercept = 0, size = .5, color = "black") +
  labs(
    x = "Weeks",
    y = "EPA (Average)",
    title = "Comparison of Elliott (DAL) vs Smallwood (PHI)"
  ) +
  geom_point(size = 1.5) +
  scale_x_continuous(breaks = seq(1, 9, 1)) +
  scale_color_manual(values=c('#003594','#004C54')) +
  theme(
    legend.title = element_blank(),
    legend.position = "top"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

ggplotly(rushers)
