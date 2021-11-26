#install.packages('sportyR')
library(sportyR)
library(dplyr)
pacman::p_load(showtext, jpeg, png)
library(ggplot2)

games = read.csv('games.csv')
wk1 = read.csv('week1.csv') 
wk2 = read.csv('week2.csv')
wk3 = read.csv('week3.csv')
wk4 = read.csv('week4.csv')
wk5 = read.csv('week5.csv')
wk6 = read.csv('week6.csv')
wk7 = read.csv('week7.csv')
wk8 = read.csv('week8.csv')

wks1to8 = rbind(wk1, wk2,wk3,wk4,wk5,wk6,wk7,wk8) %>% 
  select(x, y, displayName)


ZErtz= wks1to8 %>% 
  filter(displayName == "Zach Ertz")

wr1 =geom_football(league= "NFL")+
  annotation_raster(readPNG("NFL Logo.png"),
                    xmin = 55, xmax = 65, 
                    ymin = 22, ymax = 32, interpolate = T)+
  ggtitle("Zach Ertz Location Heat Map")+
  geom_density_2d_filled(ZErtz, mapping = aes(x,y),alpha=.80,contour_var = "ndensity")+
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5))
wr1

NAgholor = wks1to8 %>% 
  filter(displayName == "Nelson Agholor")

wr2 =geom_football(league= "NFL")+
  annotation_raster(readPNG("NFL Logo.png"),
                    xmin = 55, xmax = 65, 
                    ymin = 22, ymax = 32, interpolate = T)+
  ggtitle("Nelson Agholor Location Heat Map")+
  geom_density_2d_filled(NAgholor, mapping = aes(x,y),alpha=.80,contour_var = "ndensity")+
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5))
wr2

AJeffery = wks1to8 %>% 
  filter(displayName == "Alshon Jeffery")

wr3 =geom_football(league= "NFL")+
  annotation_raster(readPNG("NFL Logo.png"),
                    xmin = 55, xmax = 65, 
                    ymin = 22, ymax = 32, interpolate = T)+
  ggtitle("Alshon Jeffery Location Heat Map")+
  geom_density_2d_filled(AJeffery, mapping = aes(x,y),alpha=.80,contour_var = "ndensity")+
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5))
wr3

JMatthews = wks1to8 %>% 
  filter(displayName == "Jordan Matthews")

wr4 =geom_football(league= "NFL")+
  annotation_raster(readPNG("NFL Logo.png"),
                    xmin = 55, xmax = 65, 
                    ymin = 22, ymax = 32, interpolate = T)+
  ggtitle("Jordan Matthews Location Heat Map")+
  geom_density_2d_filled(JMatthews, mapping = aes(x,y),alpha=.80,contour_var = "ndensity")+
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5))
wr4

WSmallwood = wks1to8 %>% 
  filter(displayName == "Wendell Smallwood")

rb1 =geom_football(league= "NFL")+
  annotation_raster(readPNG("NFL Logo.png"),
                    xmin = 55, xmax = 65, 
                    ymin = 22, ymax = 32, interpolate = T)+
  ggtitle("Wendell Smallwood Location Heat Map")+
  geom_density_2d_filled(WSmallwood, mapping = aes(x,y),alpha=.80,contour_var = "ndensity")+
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5))
rb1

#Jay Ajayi was injured Week 8 and didn't play
JAjayi = wks1to8 %>% 
  filter(displayName == "Jay Ajayi")

rb2 =geom_football(league= "NFL")+
  annotation_raster(readPNG("NFL Logo.png"),
                    xmin = 55, xmax = 65, 
                    ymin = 22, ymax = 32, interpolate = T)+
  ggtitle("Jay Ajayi Location Heat Map")+
  geom_density_2d_filled(JAjayi, mapping = aes(x,y),alpha=.80,contour_var = "ndensity")+
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5))
rb2

CCLement = wks1to8 %>% 
  filter(displayName == "Corey Clement")

rb3 =geom_football(league= "NFL")+
  annotation_raster(readPNG("NFL Logo.png"),
                    xmin = 55, xmax = 65, 
                    ymin = 22, ymax = 32, interpolate = T)+
  ggtitle("Corey Clement Location Heat Map")+
  geom_density_2d_filled(CCLement, mapping = aes(x,y),alpha=.80,contour_var = "ndensity")+
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5))
rb3

JAdams = wks1to8 %>% 
  filter(displayName == "Josh Adams")

rb4 =geom_football(league= "NFL")+
  annotation_raster(readPNG("NFL Logo.png"),
                    xmin = 55, xmax = 65, 
                    ymin = 22, ymax = 32, interpolate = T)+
  ggtitle("Josh Adams Location Heat Map")+
  geom_density_2d_filled(JAdams, mapping = aes(x,y),alpha=.80,contour_var = "ndensity")+
  theme(legend.position = "none",plot.title = element_text(hjust = 0.5))
rb4
