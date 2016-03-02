library(rasterVis)
library(ggplot2)
library(maps)
setwd("c:/Stat4701-Group10")  

# read csv file (replace all ' with '', remove #)
tbl <- read.table('GlobalFloodsRecord.csv',header=TRUE,sep='~') 

# centroids of all floods
latX = tbl$Centroid.X
lonY = tbl$Centroid.Y

# read GeoTiff
map <- raster("worldpop.tif")

latX <- latX[which(latX!='')] 
latX <- latX[which(latX!='N/A')] 

lonY <- lonY[which(lonY!='')] 
lonY <- lonY[which(lonY!='N/A')] 

austin = data.frame(x = as.numeric(as.character(latX)), y = as.numeric(as.character(lonY)))

gplot(map, maxpixels = 5e5) + 
  geom_tile(aes(fill = value)) +
  facet_wrap(~ variable) +
  scale_fill_gradient(low = 'white', high = 'black') +
  coord_equal()+geom_point(data = austin, aes(x = x, y = y), colour = "red", size = 1, alpha = 0.1)





