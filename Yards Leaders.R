library(dplyr)
pacman::p_load(showtext, jpeg, png)
library(ggplot2)
library(highcharter)

Rushing18 = read.csv('EaglesRushing2018.csv')
Receiving18 = read.csv('EaglesReceiving2018.csv')


TotalPlayerRush = Rushing18 %>%
  mutate(YPA = Yds/Att) %>% 
  select(Week,Player,Yds,Att) %>%
  na.omit() %>% 
  group_by(Player) %>% 
  summarise(RushingYards = sum(Yds)) %>% 
  arrange(desc(RushingYards))

TopRushers <- TotalPlayerRush %>%
  hchart("bar", hcaes(x = Player, y = RushingYards)) %>% 
  hc_add_theme(hc_theme_google()) %>%
  hc_colors('#004C54') %>% 
  hc_tooltip(pointFormat = "<b>Total Rushing Yards: {point.y}</b>") %>% 
  hc_xAxis(title = list(text = "Player")) %>% 
  hc_yAxis(title = list(text = "Total Rushing Yards")) %>% 
  hc_title(text = "Rushing Leaders",
           useHTML = TRUE, align = "center")
TopRushers


TotalPlayerRec = Receiving18 %>%
  select(Week,Player,Yds) %>%
  na.omit() %>% 
  group_by(Player) %>% 
  summarise(ReceivingYards = sum(Yds)) %>% 
  arrange(desc(ReceivingYards))

TopReceivers <- TotalPlayerRec[1:10,] %>%
  hchart("bar", hcaes(x = Player, y = ReceivingYards)) %>% 
  hc_add_theme(hc_theme_google()) %>%
  hc_colors('#004C54') %>% 
  hc_tooltip(pointFormat = "<b>Total Receiving Yards: {point.y}</b>") %>%
  hc_xAxis(title = list(text = "")) %>% 
  hc_yAxis(title = list(text = "Total Receiving Yards")) %>% 
  hc_title(text = "Receiving Leaders",
           useHTML = TRUE, align = "center")
TopReceivers



