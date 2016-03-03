install.packages("rworldmap")
install.packages("XML")
install.packages("maptools")
install.packages("sp")
install.packages("ffbase")
install.packages("ff")
install.packages("RCurl")
install.packages("RColorBrewer")
install.packages("raster")
install.packages("ggplot2")

library(rworldmap)
library(XML)
library(maptools)
library(sp)
library(ffbase)
library(ff)
library(RCurl)
library(RColorBrewer)
library(raster)
library(ggplot2)

gf <- read.csv("GlobalFloodsRecord_for_bubbles.csv", header = TRUE, sep=",", nrows = 1000)
gf$Centroid.Y <- as.numeric(gf$Centroid.Y)
gf$Centroid.X <- as.numeric(gf$Centroid.X)
gf$Affected <- as.numeric(gf$Affected)
gf$Displaced <- as.numeric(gf$Displaced)
gf$Dead <- as.numeric(gf$Dead)
gf$Damage..USD. <- as.numeric(gf$Damage..USD.)
gf$Duration.in.Days <- as.numeric(gf$Duration.in.Days)
gf$Severity.. <- as.numeric(gf$Severity..)
gf$Affected.sq.km <- as.numeric(gf$Affected.sq.km)

mapDevice()
#Generating table to work with map data

spdf <- joinCountryData2Map(gf, joinCode="NAME", nameJoinColumn="Country")

head(spdf)

mapCountryData(spdf, nameColumnToPlot="Duration.in.Days", catMethod = "categorical", mapRegion = "world", 
                       colourPalette = "white2Black", addLegend = TRUE,  borderCol = "grey", 
                       mapTitle = "Duration of Floods by Country",aspect = 1, missingCountryCol = NA,
                       lwd = 1.5, addLegend=TRUE)

deaths <- subset(x=gf, Dead >0)
mapBubbles(deaths, nameZSize="Affected.sq.km", nameZColour="Dead",symbolSize=1,nameX = "Centroid.X",
           nameY = "Centroid.Y", fill=FALSE, add=TRUE)


