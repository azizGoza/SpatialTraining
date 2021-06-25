
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
mam_pt_coverage <- sleacData$mam.in / sleacData$mam.total
mam_pt_coverage <- with(sleacData, mam.in /mam.total)

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

##
## Because this is a SLEAC survey, we need to classify point and period coverage
## based on set standards. For this exercise we used two standards classifier
## of 20% and 50%
##

classify_coverage <- function(n, cases_in, std = c(0.2, 0.5)) {
  d1 <- std[1] * n
  d2 <- std[2] * n
  
  classification <- ifelse(cases_in <= d1, "low",
                           ifelse(cases_in > d2, "high", "moderate"))
  
   classification
}

sam_pt_class <- classify_coverage(n = sleacData$sam.total, 
                                  cases_in = sleacData$sam.in)
sam_pd_class <- classify_coverage(n = sleacData$sam.total + sleacData$sam.rec,
                                  cases_in = sleacData$sam.in + sleacData$sam.rec)


sam_pd_coverage <- with(sleacData, (sam.in + sam.rec) / (sam.total + sam.rec))
mam_pd_coverage <- with(sleacData, (mam.in + mam.rec) / (mam.total + mam.rec))

## How do I add the calculated coverage values to the sleacData object?
sleacData$sam_pt_coverage <- sam_pt_coverage
sleacData$sam_pd_coverage <- sam_pd_coverage
sleacData$sam_pt_class <- sam_pt_class
sleacData$sam_pd_class <- sam_pd_class

## Second step: Match the coverage values to the westPokot map

plot(westPokot)
text(x = coordinates(westPokot), labels = westPokot$SP_ID)
## We know already that 1032 matches with North Pokot
## 1032 = North Pokot
## 1036 = Central Pokot
## 1043 = South Pokot

westPokot@data$sub_county <- ifelse(westPokot@data$SP_ID == "1032", "North Pokot",
                                    ifelse(westPokot@data$SP_ID == "1036", "Central Pokot",
                                           ifelse(westPokot@data$SP_ID == "1043", "South Pokot", "West Pokot")))

## merge sleadData with westPokot map
westPokot <- merge(x = westPokot, y = sleacData, 
                   by.x = "sub_county", by.y = "subcounty")

## Plot a SAM coverage map (point and period) for West Pokot County
## Create a choropleth map - map a specific numeric value from data associated
## with the regions within an area

## Step 1: Colours - specify a scale and groupings
## For proportion values such as coverage indicators, the natural scale is from
## 0-1 (or 1 - 100 if converted to percentages)
##
## 11 groupings - 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1 - 11 colours
## 6 groupings - 0, 0.2, 0.4, 0.6, 0.8, 1 - 6 colours
## 3 groupings - 0, 0.5, 1 - 3 colours
## 5 groupings - 0, 0.25, 0.5, 0.75, 1 - 5 colours
## https://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3
##
## Slocum et al. (2008) Thematic Cartography and Visualization. Pentice Hall
##
## ColorBrewer: An online tool for selecting color schemes for maps. The 
## Cartographic Journal 40(1): 27-37.
##
## For this example, we will use 6 classes
##

## Step 2: Get colours based on your chosen scale and groupings
## For 6 groupings, we need 6 colours and we will use a divergent colour
## scheme using the Red-Yellow-Green (RdYlGn) colour scheme
##
RdYlGn <- c("#d73027", "#fc8d59", "#fee08b", "#d9ef8b", "#91cf60", "#1a9850")

### Load library RColorBrewer
#install.packages("RColorBrewer")
library(RColorBrewer)

## Get RdYlGn colours using RColorBrewer
RdYlGn <- brewer.pal(n = 6, name = "RdYlGn")

## Step 3: Classify the coverage data based on the colour scale groupings

## Use ifelse to classify point coverage values
sam_pt_class <- with(westPokot@data, 
                     ifelse(sam_pt_coverage == 0, 1,
                            ifelse(sam_pt_coverage > 0 & sam_pt_coverage <= 0.2, 2,
                                   ifelse(sam_pt_coverage > 0.2 & sam_pt_coverage <= 0.4, 3,
                                          ifelse(sam_pt_coverage > 0.4 & sam_pt_coverage <= 0.6, 4,
                                                 ifelse(sam_pt_coverage > 0.6 & sam_pt_coverage <= 0.8, 5, 6)))))
)

## Use cut to classify point coverage values
sam_pt_class <- cut(x = westPokot@data$sam_pt_coverage,
                    breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                    include.lowest = FALSE, right = TRUE, label = FALSE) + 1
sam_pt_class <- ifelse(is.na(sam_pt_class), 1, sam_pt_class)



## Step 4: Plot the point coverage choropleth map
## This plot will produce wrong colours for the coverage values of each subcounty
plot(westPokot,
     col = RdYlGn[sam_pt_class], 
     border = "gray50", lwd = 3)
text(coordinates(westPokot), labels = westPokot@data$sub_county)

## Step 5: add a legend
## This step is to learn how to add a map legend
legend(
  title = "Point Coverage",
  x = "topright", inset = 0.002,
  legend = c("0%", "> 0 and <= 20%", "> 20% and <= 40", 
             "> 40% and <= 60%", ">60% and <= 80%", "> 80% and <= 100%"),
  pch = 22, pt.cex = 2,
  col = RdYlGn, pt.bg = RdYlGn,
  bty = "n", cex = 0.75
)

## Mapping classifications rather than numbers
RdYlGn <- brewer.pal(n = 3, name = "RdYlGn")

plot(westPokot,
     col = ifelse(westPokot@data$sam_pt_class == "low", RdYlGn[1],
                  ifelse(westPokot@data$sam_pd_class == "moderate", RdYlGn[2], RdYlGn[3])), 
     border = "gray50", lwd = 3)
text(coordinates(westPokot), labels = westPokot@data$sub_county)

## Step 5: add a legend
## This step is to learn how to add a map legend
legend(
  title = "Point Coverage Classification",
  x = "topright", inset = 0.002,
  legend = c("Low (0-20%)", "Moderate (20%-50%)", "High (50%-100%"),
  pch = 22, pt.cex = 2,
  col = RdYlGn, pt.bg = RdYlGn,
  bty = "n", cex = 0.75
)





