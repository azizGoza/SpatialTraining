# Practice scripts for training on spatial

## Practice reading data - We practice using two functions - read.csv() and
## read.table() - to read a CSV file as follows:

sleacData <- read.csv(file = "westPokotSLEACdata.csv")

sleacDataX <- read.table(file = "westPokotSLEACdata.csv", header = TRUE, sep = ",")

## Practice reading map data - We practice using two functions - readShapeSpatial() in maptools
## and readOGR() - to read spatial data

library(rgeos)
library(rgdal)
library(maptools)

sudan01 <- readShapeSpatial(fn = "sudanMaps/sudan01")

sudan01 <- readOGR(dsn = "sudanMaps", layer = "sudan01")

sudan02 <- readOGR(dsn = "sudanMaps", layer = "sudan02")


## Manipulation and plotting of spatial objects

### Plot sudan01
plot(sudan01)

### Plot sudan01 with each state having a different colour (qualitative colours)

### Load library RColorBrewer
library(RColorBrewer)

### Create 9 colour qualitative palette using RColorBrewer
baseColours <- brewer.pal(n = 9, name = "Pastel1")

### Using sudanColours, create 18 unique colours using colorRampPalette function;
### Creates a function that can be used to create 18 colours
sudanColours <- colorRampPalette(colors = baseColours, space = "Lab")

plot(sudan01, col = sudanColours(n = 18))

### Plot state map and add label of states on map
plot(sudan01, col = sudanColours(n = 18))
text(x = coordinates(sudan01), labels = sudan01$State_En, cex = 0.5)

### Plot a red dot on Norht Kourfodan
plot(sudan01, col = sudanColours(n = 18))
points(coordinates(subset(sudan01, State_En == "North Kordofan")), pch = 19, col = "red", cex = 1)
text(x = coordinates(sudan01), labels = sudan01$State_En, cex = 0.5)

### 

### Plot villages on map


