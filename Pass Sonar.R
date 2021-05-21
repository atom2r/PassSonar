library(tidyverse)
library(ggsoccer)
library(grid) 
library(viridis)
library(cowplot)

df <- read.csv("Chelsea vs Leicester Final.csv")

head(df)

match.data <- df %>% select(x, y, teamId,type.displayName, outcomeType.value, outcomeType.displayName, playerId, isTouch, endX, endY,
                            blockedX, blockedY)
match.data

team.data <- match.data %>% filter(teamId == 15) # earlier done as player id of mason mount
team.data

teampass.data <- team.data %>% filter(type.displayName == "Pass")
teampass.data

match.data <- match.data %>% mutate(diffx = x - endX, diffy = y - endY)

match.data <- mutate(match.data, distance = sqrt(diffx^2 + diffy^2), 
             angle = atan2(diffy, diffx))

round.angle <- 15

match.data <- match.data %>% mutate(angle.round=round(angle*180/pi/round.angle)*round.angle)

teampass.data

team.sonar <- match.data%>%
  group_by(playerId)%>%
  mutate(N=n())%>%
  ungroup()%>%
  group_by(playerId, angle.round)%>%
  mutate(n.angle=n()/N)%>%
  ungroup()%>%
  group_by(playerId)%>%
  mutate(maxN=max(n.angle),
         angle.norm=n.angle/maxN)%>%
  ungroup()%>%
  group_by(angle.round, playerId, N, teamId)%>%
  summarize(angle.norm=mean(angle.norm),
            distance=mean(distance),
            distance=ifelse(distance>30, 30,distance))

team.sonar <- team.sonar %>% mutate(playerName =
                        case_when(playerId == 25931 ~ "César Azpilicueta", 
                                  playerId == 130903 ~ "Timo Werner",
                                  playerId == 104010 ~ "Antonio Rüdiger",
                                  playerId == 113880 ~ "Kepa Arrizabalaga",
                                  playerId == 106968 ~ "Jorginho",
                                  playerId == 343346 ~ "Mason Mount",
                                  playerId == 115868 ~ "Hakim Ziyech",
                                  playerId == 84008 ~ "Marcos Alonso",
                                  playerId == 361330 ~ "Reece James",
                                  playerId == 114075 ~ "N'Golo Kanté",
                                  playerId == 28550 ~ "Thiago Silva",
                                  playerId == 302692 ~ "Christian Pulisic",
                                  playerId == 299272 ~ "Ben Chilwell",
                                  playerId == 326413 ~ "Kai Havertz",
                                  playerId == 350088 ~ "Callum Hudson-Odoi",
                                  playerId == 24444 ~ "Olivier Giroud"))

team.sonar

pass.sonar <- ggplot(team.sonar%>%filter(playerId == 106968))+geom_bar(aes(x=angle.round, y=angle.norm, fill=distance),
                                                                       stat="identity", colour = "black")+
  scale_y_continuous(limits=c(0,0.65))+ # VARIES FROM PLAYER TO PLAYER
  scale_x_continuous(breaks=seq(-180, 180, by=90), limits=c(-180,180))+
  coord_polar(start =pi*2, direction=-1)+
  
  scale_fill_viridis("Distance (Metres)", limits=c(0,30), na.value="#FDE725FF")+
  labs(x='', y='', title= "Jorginho")+
  theme(plot.title = element_text(hjust=0.5, colour = "white", family = "Comfortaa"),
        plot.background = element_rect(fill = "#011515",colour = "#011515"),
        panel.background = element_rect(fill = "#011515",colour = '#011515'),
        panel.grid = element_line(colour = "#2D4B4B"),
        legend.background = element_rect(fill = "#011515", colour = "#011515"),
        legend.key = element_rect(fill = "#011515", colour = "#011515"),
        legend.position = "bottom",
        legend.title = element_text(colour = "white", family = "Comfortaa"),
        legend.text = element_text(colour = "white", family = "Comfortaa"),
        axis.text = element_blank(),
        axis.ticks = element_blank())

pass.sonar

p2 <- ggplot() + 
  annotate("segment", x = 0.03, y = 0.35, xend = 0.03, yend = 0.45, arrow = arrow(length = unit(0.3, "cm"),
                                                                          type = "open"), colour = "white") +
  annotate("text", x = 0.1, y = 0.55, label = "Direction of play", colour = "white", family = "Comfortaa") +
  
  coord_cartesian(xlim = c(0,2), ylim =c(0,2)) +
  theme_void()

ggdraw() +
  draw_plot(pass.sonar) +
  draw_plot(p2, x = 0.22, y = 0.6)

ggsave("Jorge sonar.png", bg = "#011515", device = "png", type = "cairo")
