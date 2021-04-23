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
lymesState = read.csv(file = "lymesState.csv") #state
lymesCounty = read.csv(file = "lymesCounty.csv") #state

#visualization libraries
library(usmap) 
library(mapdata)
library(ggplot2) 

#factor the animal data



#changing all county data to be upper case so that the variables are uniform
lymesCounty$County = toupper(lymesCounty$County)
lymesCounty$State = toupper(lymesCounty$State)

income$County = toupper(income$County)
income$State = toupper(income$State)

popDensity$County = toupper(popDensity$County)
popDensity$State = toupper(popDensity$State)

populationTotal$County = toupper(populationTotal$County)
populationTotal$State = toupper(populationTotal$State)

#merging data with county as parameter
allCountyData <- merge(lymesCounty, income, by = c('County', 'State'), all.x = TRUE)
allCountyData <- merge(allCountyData, popDensity, by = c('County', 'State'), all.x = TRUE)
allCountyData <- merge(allCountyData, populationTotal, by = c('County', 'State'), all.x = TRUE)

#Ignoring places without any cases
allCountyData = allCountyData[allCountyData$Cases2018 > 0, ]

#sorting states alphabetically to make merging easier for state data
#Also added in some 
lymesState <- lymesState[order(lymesState$State),]
hdi <-hdi[order(hdi$state),]
rAndD <-rAndD[order(rAndD$State),]
lymesState$hdi = hdi$HDI
lymesState$rAndD = rAndD$Expenditures.on.R.D.per.capita.in.US..2.
#Ignoring places without any cases
lymesState = lymesState[lymesState$X2018.Incidence > 0, ]

#Plotting the varibales in merged data
#hdi vs lyme
lymesVHDI = ggplot() + geom_text(data = as.data.frame(lymesState), aes(x = hdi, y = X2018.Incidence, label = State))+ labs(title="Lyme Disease Incidence vs HDI(state)", x ="HDI", y = "Lyme Disease Incidence") 
ggsave("plots/lymesVHDI.png", plot = last_plot())
lymesVHDI
#r and d vs lyme
lymesVrAndD = ggplot() + geom_text(data = as.data.frame(lymesState), aes(x = rAndD, y = X2018.Incidence, label = State))
ggsave("plots/lymesVrAndD.png", plot = last_plot())
lymesVrAndD
#income per county vs lyme
lymesVincome = ggplot() + geom_point(data = as.data.frame(allCountyData), aes(x = Per.capitaincome, y = Cases2018))
ggsave("plots/lymesVincome.png", plot = last_plot())
lymesVincome
#pop density vs lyme
lymesVpopDen = ggplot() + geom_point(data = as.data.frame(allCountyData), aes(x = Population, y = Cases2018))
ggsave("plots/lymesVpopDen.png", plot = last_plot())
lymesVpopDen
#total pop vs lyme
lymesVpopTotal = ggplot() + geom_point(data = as.data.frame(allCountyData), aes(x = POPESTIMATE2019/1000000, y = Cases2018)) + labs(title="Lyme Disease Cases vs Total Population(county)", x ="Total Population(millions)", y = "Lyme Disease Cases")
ggsave("plots/lymesVpopTotal.png", plot = last_plot())
lymesVpopTotal

m <- lm(lymesState$hdi ~ lymesState$X2018.Incidence)
rs <- summary(m)$r.squared

#plots for presentaiton. Not too usefful either
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
#amphibian plots
amphibians$ITISscientificName = as.factor(amphibians$ITISscientificName)
amphibians = amphibians[amphibians$decimalLongitude > -130, ]
amphibians = amphibians[amphibians$decimalLongitude < -75, ]
amphibians = amphibians[amphibians$decimalLatitude > 15, ]
amphibians = amphibians[amphibians$decimalLatitude < 55, ]
gg2 <- (ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill = "white", color = "black")
+ coord_fixed(1.3)  
+ geom_point(data = amphibians, aes(x = decimalLongitude , y = decimalLatitude), color = "blue", size = 0.5) 
+ facet_wrap(~ITISscientificName))
ggsave("amphibiansAllUsPlot.png", plot = last_plot())
#bird plots
birds$ITISscientificName = as.factor(birds$ITISscientificName)
birds = birds[birds$decimalLongitude > -130, ]
birds = birds[birds$decimalLongitude < -75, ]
birds = birds[birds$decimalLatitude > 15, ]
birds = birds[birds$decimalLatitude < 55, ]
gg2 <- (ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill = "white", color = "black")
        + coord_fixed(1.3)  
        + geom_point(data = birds, aes(x = decimalLongitude , y = decimalLatitude), color = "purple", size = 0.5) 
        + facet_wrap(~ITISscientificName))
ggsave("birdsAllUsPlot.png", plot = last_plot())

#reptile plots
reptiles$ITISscientificName = as.factor(reptiles$ITISscientificName)
reptiles = reptiles[reptiles$decimalLongitude > -130, ]
reptiles = reptiles[reptiles$decimalLongitude < -75, ]
reptiles = reptiles[reptiles$decimalLatitude > 15, ]
reptiles = reptiles[reptiles$decimalLatitude < 55, ]
gg2 <- (ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill = "white", color = "black")
        + coord_fixed(1.3)  
        + geom_point(data = reptiles, aes(x = decimalLongitude , y = decimalLatitude), color = "green", size = 0.5) 
        + facet_wrap(~ITISscientificName))
ggsave("reptilesAllUsPlot.png", plot = last_plot())

#mammal plots
mammals$ITISscientificName = as.factor(mammals$ITISscientificName)
mammals = mammals[mammals$decimalLongitude > -130, ]
mammals = mammals[mammals$decimalLongitude < -75, ]
mammals = mammals[mammals$decimalLatitude > 15, ]
mammals = mammals[mammals$decimalLatitude < 55, ]
gg2 <- (ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill = "white", color = "black")
        + coord_fixed(1.3)  
        + geom_point(data = mammals, aes(x = decimalLongitude , y = decimalLatitude), color = "red", size = 0.5) 
        + facet_wrap(~ITISscientificName))
ggsave("mammalsAllUsPlot.png", plot = last_plot())


  



