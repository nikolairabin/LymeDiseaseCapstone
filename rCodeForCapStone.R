library(tidyr)
library(knitr) # I recommend doing this
setwd("C:/Users/Nikolai Rabin/Documents/lymes")
#Convert all csv to data frames
precipitation = read.csv(file = "110-pcp-202001-60.csv")
tempurature = read.csv(file = "110-tavg-202001-60.csv")
amphibians = read.csv(file = "amphibians.csv")
birds = read.csv(file = "birds.csv")
populationTotal = read.csv(file = "co-est2019-alldata.csv")
popDensity = read.csv(file = "Current-Population-Density.csv")
hdi = read.csv(file = "hdi_data.csv")
income = read.csv(file = "income.csv")
mammals = read.csv(file = "Mammals.csv")
rAndD = read.csv(file = "r&d.csv")
reptiles = read.csv(file = "reptiles.csv")


#install.packages("usmap")
library(usmap) #import the package
library(mapdata)
library(ggplot2) #use ggplot2 to add layer for visualization

plot_usmap(data = hdi, values = "HDI", regions = "states") + 
  scale_fill_continuous(low = "white", high = "dark green", name = "HDI", label = scales::comma) + 
  labs(title = "U.S. States HDI",
       subtitle = "This is a blank map of Human Development Index for Each State") + 
  theme(panel.background=element_blank())

gg1 <- ggplot() + 
  geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill = "violet", color = "blue") + 
  coord_fixed(1.3)

popDensity$density <- as.numeric(as.character(popDensity$density))
plot_usmap(data = mammals[mammals$ITISscientificName == "Rattus rattus", ], values = "density", regions = "counties") + 
  scale_fill_continuous(low = "blue", high = "dark green", name = "HDI", label = scales::comma) + 
  labs(title = "U.S. counties",
       subtitle = "This is a blank map of the United States.") + 
  theme(panel.background=element_blank())


usa <- map_data("usa")
ratData = mammals[mammals$ITISscientificName == "Rattus rattus", ]
ratData = ratData[ratData$long > -130, ]
ratData = ratData[ratData$long < -75, ]
ratData = ratData[ratData$lat > -24, ]
ratData = ratData[ratData$long < -50, ]

ratData2 = mammals[mammals$ITISscientificName == "Rattus", ]
ratData2 = ratData2[ratData2$long > -130, ]
ratData2 = ratData2[ratData2$long < -75, ]
ratData2 = ratData2[ratData2$lat > -24, ]
ratData2 = ratData2[ratData2$long < -50, ]

ratData3 = mammals[mammals$ITISscientificName == "Rattus norvegicus", ]
ratData3 = ratData3[ratData3$long > -130, ]
ratData3 = ratData3[ratData3$long < -75, ]
ratData3 = ratData3[ratData3$lat > -24, ]
ratData3 = ratData3[ratData3$long < -50, ]
gg1 <- ggplot() + 
  geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill = "white", color = "black") + 
  coord_fixed(1.3)
gg1 + 
  geom_point(data = ratData, aes(x = long, y = lat), color = "red", size = 3) +
  geom_point(data = ratData2, aes(x = long, y = lat), color = "blue", size = 3) +
  geom_point(data = ratData3, aes(x = long, y = lat), color = "green", size = 3)
  



