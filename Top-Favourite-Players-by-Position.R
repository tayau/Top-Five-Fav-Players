#************************************************
#                                                    
#  Benjamin Chan, Trevor Yau
#  
#  Last Revision: 05/22/17
#                                                    
#
#*****************************************************
rm(list=ls())
library(dplyr)
library(grid)
library(dplyr)
require(grid)
#function that creates a circle taking in parameters of: 'origin', 'radius', 'number of points'
circle_points = function(center = c(0, 0), radius = 1, npoints = 360) {
  angles = seq(0, 2 * pi, length.out = npoints)
  return(data.frame(x = center[1] + radius * cos(angles),
                    y = center[2] + radius * sin(angles)))
}


bg_color='black'
#new_color='dodgerblue4'
new_color='darkorange'
theme_court = function(base_size = 16) {
  theme_bw(base_size) +
    theme(
      text = element_text(color = "black"),
      plot.background = element_rect(fill = 'white', color = 'black'),
      panel.background = element_rect(fill = new_color, color = 'black'),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks.length = unit(0, "lines"),
      #legend.background = ele
      #legend("topright", c("reading/Writing", "Speaking"), col=c("darkblue","red"), lwd=10),
      legend.background = element_rect(fill = 'blue', color = 'yellow'),
      legend.position = "topright",
      legend.key = element_blank(),
      legend.text = element_text(size = rel(1.0))
    )
}

width = 50
height = 94 / 2
key_height = 19
inner_key_width = 12
outer_key_width = 16
backboard_width = 6
backboard_offset = 4
neck_length = 0.5
hoop_radius = 0.75
hoop_center_y = backboard_offset + neck_length + hoop_radius
three_point_radius = 23.75
three_point_side_radius = 22
three_point_side_height = 14

short_three_radius = 22
short_three_seasons = c("1994-95", "1995-96", "1996-97")

court_points = data.frame(
  x = c(width / 2, width / 2, -width / 2, -width / 2, width / 2),
  y = c(height, 0, 0, height, height),
  desc = "perimeter"
)

court_points2 = data.frame(
  x = c(outer_key_width / 2 , outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2),
  y = c(0, key_height, key_height, 0),
  desc = "outer_key"
)

court_points2

court_points3 = data.frame(
  x = c(-backboard_width / 2, backboard_width / 2),
  y = c(backboard_offset, backboard_offset),
  desc = "backboard"
)

court_points4 = data.frame(
  x = c(0, 0), y = c(backboard_offset, backboard_offset + neck_length), desc = "neck"
)

foul_circle = circle_points(center = c(0, key_height), radius = inner_key_width / 2)
foul_circle_top = filter(foul_circle, y > key_height) %>% mutate(desc = "foul_circle_top")
foul_circle_bottom = filter(foul_circle, y < key_height) %>% mutate(desc = "foul_circle_bottom")

hoop = circle_points(center = c(0, hoop_center_y), radius = hoop_radius) %>% mutate(desc = "hoop")

restricted = circle_points(center = c(0, hoop_center_y), radius = 4) %>%
  filter(y >= hoop_center_y) %>%
  mutate(desc = "restricted")

three_point_circle = circle_points(center = c(0, hoop_center_y), radius = three_point_radius) %>% filter(y >= three_point_side_height)
short_three_circle = circle_points(center = c(0, hoop_center_y), radius = short_three_radius) %>% filter(y >= hoop_center_y)

three_point_line = data.frame(
  x = c(three_point_side_radius, three_point_side_radius, three_point_circle$x, -three_point_side_radius, -three_point_side_radius),
  y = c(0, three_point_side_height, three_point_circle$y, three_point_side_height, 0),
  desc = "three_point_line"
)




short_three_line = data.frame(
  x = c(three_point_side_radius, three_point_side_radius, short_three_circle$x, -three_point_side_radius, -three_point_side_radius),
  y = c(0, hoop_center_y, short_three_circle$y, hoop_center_y, 0),
  desc = "short_three_line"
)

court_without_three = rbind(court_points , foul_circle_top, foul_circle_bottom, hoop, restricted)

#court_points = rbind(court_without_three, three_point_line)
#court_points = mutate(court_points , dash = (desc == "foul_circle_bottom"))

short_three_court_points = rbind(court_without_three, short_three_line)
short_three_court_points = mutate(short_three_court_points , dash = (desc == "foul_circle_bottom"))


library(ggplot2)
require(ggplot2)
court = ggplot() +
  geom_path(data = court_points,
            aes(x = x, y = y, group = desc, linetype = 'dash'),
            color = "black") + #changes color for all lines on the court
  #Adds our line segments to the grid
  geom_path(data=court_points2,aes(x=x,y=y,group=desc,linetype='dash'), color='red')+
  geom_path(data=court_points3,aes(x=x,y=y,group=desc,linetype='dash'), color='white')+
  geom_path(data=court_points4,aes(x=x,y=y,group=desc,linetype='dash'), color='white')+
  geom_path(data=hoop,aes(x=x,y=y,group=desc,linetype='dash'), color='white')+
  geom_path(data=restricted,aes(x=x,y=y,group=desc,linetype='dash'), color='white')+
  geom_path(data=three_point_line,aes(x=x,y=y,group=desc,linetype='dash'), color='white')+
  geom_path(data=foul_circle_top,aes(x=x,y=y,group=desc,linetype='dash'), color='white')+
  geom_path(data=foul_circle_bottom,aes(x=x,y=y,group=desc,linetype='longdash'), color='white')+ 
  #scale_linetype_manual(values = c("solid", "longdash"), guide = FALSE) +
  
  #Fills in 3 point line with color
  geom_ribbon(data=three_point_line ,aes(x=x, y = y, ymin = 0, ymax = y),fill='red',alpha = 0.3)+
  
  #annotates color withing the free throw line
  annotate('rect', xmin=-8,, xmax=8, ymin=0,ymax=19, fill='orange',alpha=0)+
  scale_linetype_manual(values = c("solid", "longdash"), guide = FALSE) +
  coord_fixed(ylim = c(0, 35), xlim = c(-25, 25)) +
  ggtitle("Kawhi Leonard") +
  
  
  #Adding in player stats
  annotate('text', x=0, y=22, label="84.7%", color='black') + #free throw %
  annotate('text', x=0, y=31, label="38.8%", color='black') + #3pt%
  annotate('text', x=0, y=27, label="53.7%", color='black') + #2pt%
  annotate('text', x=20, y=30,label="3pt Area", color='white') +
  annotate('text', x=15, y=15,label="2pt Area", color='white') +
  annotate('text', x=0, y=20,label="Free Throw Line", color='white', size=3) +
  # annotate('text', x=20, y=30,label="3pt Area", color='white')
  theme_court(base_size = 22) 

court


#Creating function that creates court plots, taking in parameters: players, 3pt, 2pt and freethrow stats
court_fun = function(name,stat_3pt, stat_2pt, free_pt) {
  ggplot() +
    geom_path(data = court_points,
              aes(x = x, y = y, group = desc, linetype = 'dash'),
              color = "black") + #changes color for all lines on the court
    #Adds our line segments to the grid
    geom_path(data=court_points2,aes(x=x,y=y,group=desc,linetype='dash'), color='yellow')+
    geom_path(data=court_points3,aes(x=x,y=y,group=desc,linetype='dash'), color='white')+
    geom_path(data=court_points4,aes(x=x,y=y,group=desc,linetype='dash'), color='white')+
    geom_path(data=hoop,aes(x=x,y=y,group=desc,linetype='dash'), color='white')+
    geom_path(data=restricted,aes(x=x,y=y,group=desc,linetype='dash'), color='white')+
    geom_path(data=three_point_line,aes(x=x,y=y,group=desc,linetype='dash'), color='white')+
    geom_path(data=foul_circle_top,aes(x=x,y=y,group=desc,linetype='dash'), color='white')+
    geom_path(data=foul_circle_bottom,aes(x=x,y=y,group=desc,linetype='longdash'), color='white')+ 
    #scale_linetype_manual(values = c("solid", "longdash"), guide = FALSE) +
    
    #Fills in 3 point line with color
    geom_ribbon(data=three_point_line ,aes(x=x, y = y, ymin = 0, ymax = y),fill='red',alpha = 0.3)+
    
    #annotates color withing the free throw line
    annotate('rect', xmin=-8,, xmax=8, ymin=0,ymax=19, fill='orange',alpha=0)+
    scale_linetype_manual(values = c("solid", "longdash"), guide = FALSE) +
    coord_fixed(ylim = c(0, 35), xlim = c(-25, 25)) +
    ggtitle(name) +
    
    #Adding in player stats
    annotate('text', x=0, y=22, label=stat_3pt, color='black') + #free throw %
    annotate('text', x=0, y=31, label=stat_2pt, color='black') + #3pt%
    annotate('text', x=0, y=27, label=free_pt, color='black') + #2pt%
    annotate('text', x=20, y=30,label="3pt Area", color='white') +
    annotate('text', x=15, y=15,label="2pt Area", color='white') +
    annotate('text', x=0, y=20,label="Free Throw Line", color='white', size=3) +
    theme_court(base_size = 22) 
  
}


court_fun('Kawhi Leonard',2.5,3.5,6)

# legend.background = element_rect(fill = bg_color, color = bg_color) +
#legend.position = "bottom"+
#legend.key = element_blank()+
#legend.text = element_text(size = rel(1.0)

#setting working directory and entering data/stats of 25 players
setwd('C:/Users/Trevor/Desktop/R Programs/Personal Projects/Basketball/Top-Favourite-Players-by-Position')
data<-read.csv("Top-Favourite-Players-by-Position.csv", header=TRUE)
#colnames(data)<-c('PlayerName','Position', 'X3PT', 'FG', 'FT', 'X2PT')
data
#paste(data$FT.,"%", sep = "")
colnames(data)[1]<-'Player.Name'
data$FT. <- paste(100*(data$FT.), "%", sep = "")
data$FG. <- paste(100*(data$FG.), "%", sep = "")
data$X2P. <- paste(100*(data$X2P.), "%", sep = "")
data$X3P. <- paste(100*(data$X3P.), "%", sep = "")
data
n=nrow(data)
n
#court_fun(data$Player.Name[2],data$X3P.[2], data$X2P.[2], data$FT.[2])
court_fun(data$Player.Name[2],data$FT.[2],data$X3P.[2],data$X2P.[2])
#creating container array for our court plots
plot.mat<-list()

#Creating Court plots for all 25 players simultaneously
for(i in 1:n){
  courtplot=court_fun(data$Player.Name[i],data$FT.[i],data$X3P.[i],data$X2P.[i])
  plot.mat[[i]]=courtplot
}


plot.mat

#Saving each of the courtplots in WD
for (i in 1:n) {
  file_name = paste("courtplot", i, ".tiff", sep="")
  tiff(file_name,width = 800, height = 800, res = 120)
  print(plot.mat[[i]])
  dev.off()
}

