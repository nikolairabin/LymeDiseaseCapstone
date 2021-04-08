library(tidyr)
library(knitr) # I recommend doing this
setwd("C:/Users/Nikolai Rabin/Documents/lymes")
#Convert all csv to data frames
precipitation = read.csv(file = "110-pcp-202001-60.csv") #county
temperature = read.csv(file = "110-tavg-202001-60.csv") #county
amphibians = read.csv(file = "amphibians.csv") #animal
birds = read.csv(file = "birds.csv") #animal
populationTotal = read.csv(file = "co-est2019-alldata.csv") #county
popDensity = read.csv(file = "Current-Population-Density.csv") #county
hdi = read.csv(file = "hdi_data.csv") #state
income = read.csv(file = "income.csv") #county
mammals = read.csv(file = "Mammals.csv") #animal
rAndD = read.csv(file = "r&d.csv") #state
reptiles = read.csv(file = "reptiles.csv") #animal
lymesState = read.csv(file = "lymesState.csv")
lymesCounty = read.csv(file = "lymesCounty.csv")


#install.packages("usmap")
library(usmap) #import the package
library(mapdata)
library(ggplot2) #use ggplot2 to add layer for visualization

lymesCounty$County = toupper(lymesCounty$County)
lymesCounty$State = toupper(lymesCounty$State)
income$County = toupper(income$County)
income$State = toupper(income$State)
popDensity$County = toupper(popDensity$County)
popDensity$State = toupper(popDensity$State)
populationTotal$County = toupper(populationTotal$County)
populationTotal$State = toupper(populationTotal$State)

allCountyData <- merge(lymesCounty, income, by = c('County', 'State'), all.x = TRUE)
allCountyData <- merge(allCountyData, popDensity, by = c('County', 'State'), all.x = TRUE)
allCountyData <- merge(allCountyData, populationTotal, by = c('County', 'State'), all.x = TRUE)

allCountyData = allCountyData[allCountyData$Cases2018 > 0, ]

lymes <- lymes[order(lymes$State),]
hdi <-hdi[order(hdi$state),]
rAndD <-rAndD[order(rAndD$State),]
lymes$hdi = hdi$HDI
lymes$rAndD = rAndD$Expenditures.on.R.D.per.capita.in.US..2.
lymes = lymes[lymes$X2018.Incidence > 0, ]
#hdi doesn't seem good
lymesVHDI = ggplot() + geom_point(data = as.data.frame(lymesState), aes(x = X2018.Incidence, y = hdi))
lymesVHDI
#not much here either
lymesVrAndD = ggplot() + geom_point(data = as.data.frame(lymesState), aes(x = X2018.Incidence, y = rAndD))
lymesVrAndD

lymesVincome = ggplot() + geom_point(data = as.data.frame(allCountyData), aes(x = Cases2018, y = Per.capitaincome))
lymesVincome
#density
lymesVpopDen = ggplot() + geom_point(data = as.data.frame(allCountyData), aes(x = Cases2018, y = Population))
lymesVpopDen
#total
lymesVpopDen = ggplot() + geom_point(data = as.data.frame(allCountyData), aes(x = Cases2018, y = POPESTIMATE2019))
lymesVpopDen

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
  



