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

# Magnitude fill, and same bubbles
mag_fill <- mapCountryData(spdf, nameColumnToPlot="Magnitude..M...", catMethod = "categorical", numCats = 10, mapRegion = "latin america", 
                           colourPalette = "white2Black", addLegend = FALSE,  borderCol = "grey", 
                           mapTitle = "Deaths and displacement over flood magnitude",aspect = 1, missingCountryCol = NA,
                           lwd = 1.5)
death_displaced_bubbles <- mapBubbles(deaths, nameZSize="Displaced", nameZColour="Dead",symbolSize=1.4,nameX = "Centroid.X", legendHoriz = TRUE, legendPos="topright",
                                      nameY = "Centroid.Y", fill=TRUE, add=TRUE, colourPalette=adjustcolor(palette(heat.colors(5)), alpha.f=0.8), addColourLegend=FALSE, addLegend=TRUE)

# Add legends
do.call(addMapLegend, c(mag_fill, legendLabels="all", legendWidth=0.5, legendMar=6))
do.call(addMapLegend, c(death_displaced_bubbles, legendLabels="limits", legendWidth=0.5, legendMar=2))
