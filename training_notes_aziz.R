
# read data with read.csv() pnly for csv and read.table() for all type of tabular file
sleacData <- read.csv(file = "westpokotSLEACdata.csv")
sleacDataX <- read.table(file = "westpokotSLEACdata.csv",header= TRUE, sep =",")


# how to inatall more than one package
install.packages((c("rgeos","rgdal","raster")))
# package to use, can take time to install
library(rgeos)
library(rgdal)
library(raster)

# library(sf) new package better than the 3 others but can be diffuclt to use at first

sudan01 <- readShapeSpatial (fn= "sudanMaps/sudan01") ## old approach ; not recommanded

sudan01 <- readOGR( dsn= "sudanMaps", layer = "sudan01")  ## better to use readOGr

# ESRI shapefiles, files always plurial as several shapes, 
# write the name of file and layer the name of the doc without extension

# read west pokot shapefiles into R as a spatialpolygonedataframe

westPokot <- readOGR(dsn= "westPokotMaps", layer = "westPokot")  

# plot west pokot

plot(westPokot)

# label polygone or adding text function (text (coordinates(dataframe), labels = dataframe$column))
# (text (coordinates(dataframe), labels = dataframe$column)) put the text at the center of the polygones
# coordinates (dataframe), give center of polygone
text(coordinates(westPokot), labels =westPokot$NAME_4)

## subest westpost with only polygone name akoret or SP_ID 1032


akoret <- subset(westPokot, NAME_4 =="Akoret")

plot(akoret)


## keep the full map but focus one subcounty

plot(akoret,lty = 8)
plot(westPokot,add = TRUE)

###to high light one area with border of different color # lwd line widness by default lnd= 0.5


plot(westPokot)
plot(akoret,border ="red", lwd =2, add= TRUE)

###to high light one area  different color # col="xx"


plot(westPokot)
plot(akoret, col = "blue", add= TRUE)

###to high light one area with border of different color and color of different color

plot(westPokot)
plot(akoret,border ="red", col= "light blue", lwd =2, add= TRUE)




### plot the westpokot map based on their coverage
## result (chloropleth map)

# step 1 : compute point coverage
sam_pt_coverage <- sleacData$sam.in / sleacData$sam.total

sam_pt_coverage <- with (sleacData, sam.in/sam.total)

# step 1a : compute coverage
sam_pd_coverage <- 

sam_pd_coverage <- 

# step 2 : match the point coverage data with the pokot map, add new column with matchin data "subcounty"

# 1037 =  west pokpot
# 1036 = central pokot
# 1043 = south pokot
# 1032 north pokot


westPokot@data$sub_county <- c("Central Pokot", "West Pokot", "South Pokot","North Pokot")


westPokot@data$sub_county <- ifelse (westPokot@data$SP_ID =="1037","Central Pokot",
                                ifelse(westPokot@data$SP_ID =="1036","West Pokot",
                                   ifelse(westPokot@data$SP_ID =="1043","South Pokot","North Pokot")))



# merge sleadData with westPkot data

## merge sleadData with westPokot data
westPokot@data <- merge(x = westPokot@data, y = sleacData, 
                        by.x = "sub_county", by.y = "subcounty")
