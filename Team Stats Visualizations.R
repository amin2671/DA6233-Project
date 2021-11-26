library(dplyr)
pacman::p_load(showtext, jpeg, png)
library(ggplot2)
library(highcharter)
library(scales)

Phi18 = read.csv('Eagles2018.csv')

PhiWk10 = Phi18 %>% 
  mutate(Rec = as.character(Rec),PassYdShare = PassY/TotYd, RushYdShare = RushY/TotYd) %>% 
  filter(Week < 10)

PassYPG <- PhiWk10 %>%
  hchart("line", hcaes(x = Opp, y = PassY)) %>% 
  hc_add_theme(hc_theme_google()) %>%
  hc_colors('#004C54') %>% 
  hc_tooltip(pointFormat = "<b>Passing Yards: {point.y}</b>") %>% 
  hc_xAxis(title = list(text = "Opponent")) %>% 
  hc_yAxis(title = list(text = "Passing Yards")) %>% 
  hc_title(text = "Team Passing Yards by Week",
           useHTML = TRUE, align = "center")
PassYPG


RushYPG <- PhiWk10 %>%
  hchart("line", hcaes(x = Opp, y = RushY)) %>% 
  hc_add_theme(hc_theme_google()) %>% 
  hc_colors('#004C54') %>% 
  hc_tooltip(pointFormat = "<b>Rushing Yards: {point.y}</b>") %>% 
  hc_xAxis(title = list(text = "Opponent")) %>% 
  hc_yAxis(title = list(text = "Rushing Yards")) %>% 
  hc_title(text = "Team Rushing Yards by Week",
           useHTML = TRUE, align = "center")
RushYPG


PassShare = PhiWk10 %>% 
  hchart("line", hcaes(x = Opp, y = PassYdShare*100)) %>% 
  hc_add_theme(hc_theme_google()) %>%
  hc_colors('#004C54') %>% 
  hc_tooltip(pointFormat = "<b>Passing Yard Share: {point.y}%</b>") %>% 
  hc_xAxis(title = list(text = "Opponent")) %>% 
  hc_yAxis(title = list(text = "Passing Yard Share"),labels = list(format = "{value}%")) %>% 
  hc_title(text = "Passing Yard Share by Week",
           useHTML = TRUE, align = "center")
PassShare


RushShare = PhiWk10 %>%
  hchart("line", hcaes(x = Opp, y = RushYdShare*100)) %>% 
  hc_add_theme(hc_theme_google()) %>%
  hc_colors('#004C54') %>% 
  hc_tooltip(pointFormat = "<b>Rushing Yard Share: {point.y}%</b>") %>% 
  hc_xAxis(title = list(text = "Opponent")) %>% 
  hc_yAxis(title = list(text = "Rushing Yard Share"),labels = list(format = "{value}%")) %>% 
  hc_title(text = "Rushing Yard Share by Week",
           useHTML = TRUE, align = "center")
RushShare
