## 
temp <- read.csv("Global_temp.csv")
attach(temp)

##SOURCE http://data.giss.nasa.gov/gistemp/
par(mfrow = c(1,1), mar = c(4,4,3,1))
plot(Year,Glob, type = 'l', col = "red",xlim=c(1880,2016), ylab = "Degrees", main = "Global Annual Temperature mean")
abline(a=0,b=0, lty = 2)
text(1885,4,"1951-80 Average", cex = .8)
text(2010,-45,"Source: http://data.giss.nasa.gov/gistemp/", cex = .5)
abline(a=0,b=0, lty = 2)
abline(v=1985, lty = 2)
text(1988,80,"1985", cex = .8)


par(mfrow = c(1,2),mar =c(4,4,3,1))
plot(Year,NHem,type = 'l', col = "red",xlim=c(1880,2016), ylab = "Degrees", main = "Northern Hemisphere",ylim=c(-50,110))
abline(v=1985, lty = 2)
par(mar =c(4,3,3,2))
plot(Year,SHem,type = 'l', col = "red",xlim=c(1880,2016),yaxt = "n", ylab = "", main = "Southern Hemisphere",ylim=c(-50,110))
abline(v=1985, lty = 2)


##Just in case we want more detailed temperature changes (by latitude)
if(FALSE){
par(mfrow = c(2,2))
plot(Year,X64N.90N, type = 'l',ylim = c(-200,200))
plot(Year,X44N.64N, type = 'l',ylim = c(-200,200))
plot(Year,X24N.44N, type = 'l',ylim = c(-200,200))
plot(Year,EQU.24N, type = 'l',ylim = c(-200,200))

par(mfrow = c(2,2))
plot(Year,X24S.EQU, type = 'l',ylim = c(-200,200))
plot(Year,X44S.24S, type = 'l',ylim = c(-200,200))
plot(Year,X64S.44S, type = 'l',ylim = c(-200,200))
plot(Year,X90S.64S, type = 'l',ylim = c(-200,200))


plot(Year,SHem, type = 'l',ylim=c(-50,110))
plot(Year,SHem, type = 'l',ylim=c(-50,110))
plot(Year,SHem, type = 'l',ylim=c(-50,110))
plot(Year,SHem, type = 'l',ylim=c(-50,110))
plot(Year,SHem, type = 'l',ylim=c(-50,110))
}
