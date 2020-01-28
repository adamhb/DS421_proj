library(tidyverse)
library(magrittr)
library(sf)
library(raster)

setwd("C:/Users/ahanb/OneDrive/Documents/DS421/project/data/fromGEE")

klamath_fires <- st_read(dsn = "klamath_fire_perims_mtbs.shp")

(klamath_fires$Acres / 0.000247105) %>% sum()




bearD <- read_csv(file = 'bear_fire_points2.csv')
str(bearD)



bearD %>%
  filter(R5 > 0, R5 < 2) %>%
  #filter(distance > -1) %>%
  ggplot(aes(factor(burnSev),R5))+
  geom_boxplot()



bearD %>%
  filter(R10 > 0, R10 < 2) %>%
  #filter(distance > -1) %>%
  ggplot(aes(factor(burnSev),R15))+
  geom_boxplot()


bearD %>%
  filter(distance > -1) %>%
  filter(R5 > 0, R5 < 1) %>%
  ggplot(aes(distance,R5)) +
  geom_point() +
  scale_x_log10()

bearD %>%
  #filter(distance > -1) %>%
  filter(R5 > 0, R5 < 2) %>%
  ggplot(aes(slope,R5)) +
  geom_point() 
  #cale_x_log10()


attach(bearD)

test <- bearD %>%
  #filter(distance > -1) %>%
  filter(R5 > 0, R5 < 1) %>%
lm(formula = R5~burnSev+northness+distance+eastness+elevation+slope+numPriorFireLow+numPriorFireMedHigh)

summary(test)






summary(test)


bearD %>%
  ggplot(aes(burnSev))+
  geom_histogram()#+
  #scale_x_continuous(limits = c(0,2))





plot(bearD$burnSev,bearD$R5)


bearD %>%
  filter(R5 > 0, R5 < 2) %>%
  ggplot(aes(burnSev,R5)) +
  geom_boxplot()


table(bearD$R5,bearD$burnSev)


hist(bearD$R15)



setwd('C:/Users/ahanb/OneDrive/Documents/DS421/project/data')
