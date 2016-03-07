library(ncdf4)
library(maps)
setwd("C:/Users/SONY/Desktop/Data Visualization/Project 2")

filename <- 'NOAA_Daily_phi_500mb.nc' 
ncid <- nc_open( filename ) 
#names(ncid$var)
#print (ncid)
i <- 5000
data <- ncvar_get( ncid, 'phi', start = c(1, 1, 1, 1), count = c(144, 15, 1,5000))
data_set <- mat.or.vec(1, 2160)

makeVector <- function(data){
  time = dim(data)[3]
  data_set <- mat.or.vec(1, 2160)
    
  for (i in seq(1,time)){
    data_set <- rbind(data_set,as.vector(data[,,i]))
  }
  data_set <- data_set[-1,]
  data_set <- data.frame(data_set)
  data_set$time <- 1:time
  
  return(data_set)
}

test2 <- makeVector(data)
test.pca <- prcomp((test2[,1:(length(names(test2))-1)]),center=TRUE, scale=TRUE)

#g <- ggbiplot(test.pca, obs.scale = 1, var.scale = 1, ellipse = TRUE, 
              circle = TRUE)
#g <- g + scale_color_discrete(name = '')
#g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
#print(g)
mat_pca <- matrix(test.pca$rotation[,1],ncol = 15)
mat_pca2 <- matrix(test.pca$rotation[,2],ncol = 15)
mat_pca3 <- matrix(test.pca$rotation[,3],ncol = 15)
mat_pca4 <- matrix(test.pca$rotation[,4],ncol = 15)

plot(test.pca, type = "l")

#Run John's code before this, if it hasnt been already run, reusng his variables

par(mfrow=c(2,1))
int=seq(min(mat_pca),max(mat_pca),length.out=30)
filled.contour(lon2, lat, mat_pca[,], color.palette=rgb.palette, levels=int, asp=1, ylim=c(-90,90),
               plot.title= { title(main='PCA 1 Explaining 62.71 Percent variance', ylab='Latitude [°]'); points(x = as.vector(latX), y = as.vector(lonY), pch = 19, col = "red")}, 
               plot.axes = { axis(1); axis(2, seq(-90, 90, by = 45)); map('world', add=TRUE);grid()})


int=seq(min(mat_pca2),max(mat_pca2),length.out=30)
filled.contour(lon2, lat, mat_pca2[,], color.palette=rgb.palette, levels=int, asp=1, ylim=c(-90,90),
               plot.title= { title(main='PCA 2 Explaining 4.41 Percent variance', ylab='Latitude [°]'); points(x = as.vector(latX), y = as.vector(lonY), pch = 19, col = "red")}, 
               plot.axes = { axis(1); axis(2, seq(-90, 90, by = 45)); map('world', add=TRUE);grid()})


int=seq(min(mat_pca3),max(mat_pca3),length.out=30)
filled.contour(lon2, lat, mat_pca3[,], color.palette=rgb.palette, levels=int, asp=1, ylim=c(-90,90),
               plot.title= {title(main= 'PCA 3 Explaining 3.24 Percent variance', ylab='Latitude [°]'); points(x = as.vector(latX), y = as.vector(lonY), pch = 19, col = "red")}, 
plot.axes = { axis(1); axis(2, seq(-90, 90, by = 45)); map('world', add=TRUE);grid()})



int=seq(min(mat_pca4),max(mat_pca4),length.out=30)
filled.contour(lon2, lat, mat_pca4[,], color.palette=rgb.palette, levels=int, asp=1, ylim=c(-90,90),
               plot.title= { title(main='PCA 4 Explaining 2.90 Percent variance', ylab='Latitude [°]'); points(x = as.vector(latX), y = as.vector(lonY), pch = 19, col = "red")}, 
plot.axes = { axis(1); axis(2, seq(-90, 90, by = 45)); map('world', add=TRUE);grid()})

