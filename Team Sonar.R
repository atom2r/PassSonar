# LOAD IN CREATE PITCH FUNCTION AND OTHER PACKAGES FROM PREVIOUS CODE

# Plot details
text.color="black"
background_color="white"
radar.size=27 # Dkm
ymax=80 # For coordinating player positions on pitch
xmax=120 

# Constructing vector of integers for players involved in the specific team + NOT YET SORTED BY PLAYER 
team.players <- team.data$playerId
team.players <- team.players %>% unique()
team.players

outfield.players <- team.players[-5] # Excluding Keeper

# First load in through this loop and then plot. If changes regarding size are desired afterwards then use the other loop(s)
player.plots <- list()
for (i in 1:length(team.players)){
  
  plot.data=team.sonar %>% filter(teamId==15 & playerId==team.players[i])
  
  
  player.plots[[i]]=ggplot(plot.data)+geom_bar(aes(x=angle.round, y=angle.norm, fill=distance), stat="identity", size = 12)+
    scale_y_continuous(limits=c(0,1))+
    scale_x_continuous(breaks=seq(-180,180, by=90), limits=c(-180,180))+
    coord_polar(start=pi*2, direction=-1)+
    scale_fill_viridis("Distance (metre)", limits=c(0,30), na.value="#FDE725FF")+
    labs(x='', y='',title= plot.data$playerId[2])+
    theme_void()+
    theme(plot.title = element_text(hjust=0.5, color=text.color),
          plot.background = element_rect(fill = "transparent",colour = NA),
          panel.background = element_rect(fill = "transparent",colour = NA),
          legend.position = "none")
  player.plots[[i]]=ggplotGrob(player.plots[[i]])

  if (i==length(team.players)){
    colorbar=
      ggplot(team.sonar)+geom_bar(aes(x=angle.round, y=angle.norm, fill=distance), stat="identity")+
      scale_y_continuous(limits=c(0,0))+
      scale_fill_viridis("", limits=c(0,30), na.value="#FDE725FF")+
      labs(x='', y='')+
      theme_void()+
      theme( legend.position = "bottom",
             plot.background = element_rect(fill = "transparent",colour = NA),
             panel.background = element_rect(fill = "transparent",colour = NA))
    colorbar=ggplotGrob(colorbar)
  }
}

outfield.plots <- list()
for (i in 1:length(outfield.players)){
   
   plot.data=team.sonar %>% filter(teamId==15 & playerId==outfield.players[i])

  
   outfield.plots[[i]]=ggplot(plot.data)+geom_bar(aes(x=angle.round, y=angle.norm, fill=distance), stat="identity", size = 12)+
    scale_y_continuous(limits=c(0,0.3))+ # THIS IS THE ONLY THING CHANGED HERE. THE LIMIT HAS BEEN REDUCED TO COMPENSATE FOR FEWER PASSES
    scale_x_continuous(breaks=seq(-180,180, by=90), limits=c(-180,180))+
    coord_polar(start=pi*2, direction=-1)+
    scale_fill_viridis("Distance (metre)", limits=c(0,30), na.value="#FDE725FF")+
    labs(x='', y='',title= plot.data$playerId[2])+
    theme_void()+
    theme(plot.title = element_text(hjust=0.5, color=text.color),
          plot.background = element_rect(fill = "transparent",colour = NA),
          panel.background = element_rect(fill = "transparent",colour = NA),
          legend.position = "none")
   outfield.plots[[i]]=ggplotGrob(outfield.plots[[i]])
 
}

outfield.second <- list()
for (i in 1:length(outfield.players)){
  
  plot.data=team.sonar %>% filter(teamId==15 & playerId==outfield.players[i])
  
  
  outfield.second[[i]]=ggplot(plot.data)+geom_bar(aes(x=angle.round, y=angle.norm, fill=distance), stat="identity", size = 12)+
    scale_y_continuous(limits=c(0,0.5))+ # THIS IS THE ONLY THING CHANGED HERE. THE LIMIT HAS BEEN REDUCED TO COMPENSATE FOR FEWER PASSES
    scale_x_continuous(breaks=seq(-180,180, by=90), limits=c(-180,180))+
    coord_polar(start=pi*2, direction=-1)+
    scale_fill_viridis("Distance (metre)", limits=c(0,30), na.value="#FDE725FF")+
    labs(x='', y='',title= plot.data$playerId[2])+
    theme_void()+
    theme(plot.title = element_text(hjust=0.5, color=text.color),
          plot.background = element_rect(fill = "transparent",colour = NA),
          panel.background = element_rect(fill = "transparent",colour = NA),
          legend.position = "none")
  outfield.second[[i]]=ggplotGrob(outfield.second[[i]])
  
}

outfield.third <- list()
for (i in 1:length(outfield.players)){
  
  plot.data=team.sonar %>% filter(teamId==15 & playerId==outfield.players[i])
  
  
  outfield.third[[i]]=ggplot(plot.data)+geom_bar(aes(x=angle.round, y=angle.norm, fill=distance), stat="identity", size = 12)+
    scale_y_continuous(limits=c(0,0.8))+ # THIS IS THE ONLY THING CHANGED HERE. THE LIMIT HAS BEEN REDUCED TO COMPENSATE FOR FEWER PASSES
    scale_x_continuous(breaks=seq(-180,180, by=90), limits=c(-180,180))+
    coord_polar(start=pi*2, direction=-1)+
    scale_fill_viridis("Distance (metre)", limits=c(0,30), na.value="#FDE725FF")+
    labs(x='', y='',title= plot.data$playerId[2])+
    theme_void()+
    theme(plot.title = element_text(hjust=0.5, color=text.color),
          plot.background = element_rect(fill = "transparent",colour = NA),
          panel.background = element_rect(fill = "transparent",colour = NA),
          legend.position = "none")
  outfield.third[[i]]=ggplotGrob(outfield.third[[i]])
  
}

# Team Data
team.select="Chelsea FC"
team.formation = 523

if (team.formation==523){
  team.formation='5-2-3'
  
  back.line=20
  mid.line=48
  forward.line=77
  A=createPitch(grass_colour = background_color,goal_colour=text.color, line_colour = text.color)+coord_flip(ylim=c(0,80))+
    theme(aspect.ratio = 120/80, plot.title = element_text(size=18, hjust=0.5, vjust=-2, color=text.color),
          plot.background = element_rect(fill = background_color,colour = NA),
          panel.background = element_rect(fill = background_color,colour = NA))+
    
    # Can manually sort player positions here
    annotation_custom(grob=player.plots[[5]], xmin=-9, xmax=-9+radar.size, ymax=ymax/2+radar.size/2, y=ymax/2-radar.size/2)+ #GK
    
    annotation_custom(grob=outfield.second[[2]], xmin=mid.line, xmax=mid.line+radar.size, ymax=ymax+10, y=ymax-radar.size+1)+ #RWB
    annotation_custom(grob=outfield.second[[8]], xmin=mid.line, xmax=mid.line+radar.size, ymax=-8+radar.size, y=-3)+ #LWB
    
    annotation_custom(grob=outfield.third[[4]], xmin=back.line, xmax=back.line+radar.size, ymax=ymax/2-35+radar.size, y=ymax/2-35)+ #LCB
    annotation_custom(grob=outfield.third[[11]], xmin=back.line, xmax=back.line+radar.size, ymax=ymax/2+radar.size/2, y=ymax/2-radar.size/2)+ #CB
    annotation_custom(grob=player.plots[[10]], xmin=back.line, xmax=back.line+radar.size, ymax=ymax/2+10+radar.size, y=ymax/2+10)+ #RCB
    
    annotation_custom(grob=outfield.third[[5]], xmin=mid.line, xmax=mid.line+radar.size, ymax=ymax/2-23.5+radar.size, y=ymax/2-23.5)+ #LCM
    annotation_custom(grob=outfield.third[[10]], xmin=mid.line, xmax=mid.line+radar.size, ymax=ymax/2-6+radar.size, y=ymax/2-6)+ #RCM
    
    annotation_custom(grob=outfield.plots[[7]], xmin=forward.line-80, xmax=forward.line+70+radar.size, ymax=ymax-8, y=ymax-radar.size-8)+ #RAM
    annotation_custom(grob=outfield.plots[[3]], xmin=forward.line, xmax=forward.line+radar.size, ymax=ymax/2+radar.size/2, y=ymax/2-radar.size/2)+ #ST
    annotation_custom(grob=outfield.plots[[6]], xmin=forward.line-80, xmax=forward.line+70+radar.size, ymax=8+radar.size, y=8)+ #LAM
    
    annotation_custom(grob=colorbar, xmin=3, xmax=7, ymin=1, ymax=18)+
    annotate("text", label="lmao", x=6, y=79, hjust=1,vjust=1 ,size=3.75, color=text.color)+
    annotate("text", label="Mean Pass Distance (Metres)", x=9, y=0.5, hjust=0, size=3, color=text.color)+
    annotate("text", label='Bar length = normalized pass angle frequency; Bar color = mean pass distance', color=text.color, x=-2, y=79, hjust=1, size=3)+
    annotate("text", label=paste0('Starting Formation: ', team.formation), color=text.color, x=-2, y=0, hjust=0, size=5, fontface="bold")+
    annotate("text", label=("PassSonar: Chelsea FC vs Leicester City FC"), color=text.color, x=121.5, y=0, hjust=0.5, size=7, fontface="bold")+
    guides(fill = guide_colourbar())
  
}
A
ggsave(A, file=paste0('./', team.select,' PassSonar.png'), width=9.5, height=11.5, bg=background_color)
