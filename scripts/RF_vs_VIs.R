library(tidyverse)

rf <- read_csv('data/not_recovered_430.csv')
nbr <- read_csv('data/not_recovered_430_nbr.csv')
rf$year <- substring(rf$year,first = 8,last = 11)
rf <- mutate_at(.tbl = rf, .vars = "year",.funs = as.numeric)
rf <- rf %>% mutate(type = "RF")
nbr <- nbr %>% mutate(type = "NBR")
names(nbr) <- c("year","value","type")
names(rf) <- c("year","value","type")
all <- rbind(nbr,rf)


ggplot(data = filter(all,type=="NBR"), mapping = aes(year,value)) +
  geom_line(color = "red") +
  scale_y_continuous( name = "NBR", sec.axis = sec_axis(~./8, name="Probability Conifer")) +
  geom_line(data = filter(all,type=="RF"), mapping = aes(x = year, y = value*8), color = "blue") +
  theme(axis.title.y = element_text(colour = "red"),
        axis.title.y.right = element_text(color = "blue")) +
  labs(title = "Forest Regeneration Trajectory") +
  adams_theme2
  
