library(ggplot2)
library(tidyverse)

source('adams_theme.R')

bear <- read_csv(file = 'data/nbr_bear.csv', col_names = c("year","NBR"))
#names(bear)

bear_plot <- bear %>%
  ggplot(aes(x=year,y=NBR))+
  labs(title = "bear fire")+
  geom_line()+
  adams_theme

png("output/bear_fire.png")
bear_plot
dev.off()

aspects <- c("north","south","east","west")

bearNorth <- read_csv(file = 'data/nbr_bear_north.csv', col_names = c("year","NBR"))
bearSouth <- read_csv(file = 'data/nbr_bear_south.csv', col_names = c("year","NBR"))
bearEast <- read_csv(file = 'data/nbr_bear_east.csv', col_names = c("year","NBR"))
bearWest <- read_csv(file = 'data/nbr_bear_west.csv', col_names = c("year","NBR"))

aspectCol <- c()
for(a in aspects){
  temp <- rep(a,nrow(bearNorth))
  aspectCol <- append(aspectCol,temp)
}

bearAspects <- rbind(bearNorth, bearSouth, bearEast, bearWest) %>% cbind(aspectCol)


bearAspectsPlot <- bearAspects %>%
  ggplot(aes(year,NBR,col = aspectCol)) +
  labs(title = "bear fire (1994)") +
  geom_line(size = 4) +
  adams_theme

png("output/bearAspects.png")
bearAspectsPlot
dev.off()



panther <- read_csv(file = 'data/nbr_panther.csv', col_names = c("year","NBR"))

panther_plot <- panther %>%
  ggplot(aes(x=year,y=NBR))+
  geom_line()+
  labs(title = "panther fire") +
  adams_theme

png("output/panther_fire.png")
panther_plot
dev.off()



yellow <- read_csv(file = 'data/nbr_yellow.csv', col_names = c("year","NBR"))
  #names(panther)
  
yellow_fire_plot <- yellow %>%
    ggplot(aes(x=year,y=NBR))+
    geom_line()+
    labs(title = "yellow fire") +
   adams_theme
  
png("output/yellow_fire.png")
yellow_fire_plot
dev.off()





