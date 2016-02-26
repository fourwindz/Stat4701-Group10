require(stringr)
require(dplyr)
require(sqldf)

tbl <- read.csv('GlobalFloodsRecord.csv',header=TRUE)
countries_names <-  read.csv('country_mapping.csv',header=TRUE)
names(tbl)
cntrys <- sqldf("select tbl.*, New as New_country
                   from tbl 
                   join countries_names on countries_names.Orig = tbl.Country
                   ")
summary(tbl$Country)
cntrys$Country <- cntrys$New_country

tbl<-cntrys
tbl$Country <-tbl$New_country

tbl$Began[tbl$Began==""] <-NA
tbl$year_beg <- as.character(tbl$Began)
tbl$year_beg <- str_sub(tbl$year_beg,-2,-1)
tbl$M6 <- tbl$M.6

tbl2 <- as.data.frame(cbind(as.character(tbl$year_beg),tbl$M6, as.character(tbl$Country)))
names(tbl2)<-c("year","M6", "Country")
tbl2$year[tbl2$year=="/A"]<-NA
tbl2 <- na.omit(tbl2)
tbl2$year <- as.character(tbl2$year)
tbl2$year[as.numeric(str_sub((tbl2$year),1,1))<2] <- 2000+as.numeric(tbl2$year[as.numeric(str_sub((tbl2$year),1,1))<2])
tbl2$year[as.numeric(str_sub((tbl2$year),1,1))>2] <- 1900+as.numeric(tbl2$year[as.numeric(str_sub((tbl2$year),1,1))>2])
tbl2$year <- as.numeric(tbl2$year)


floods_M6 <- sqldf("select year, M6, count(*) as count
                     from tbl2
                     group by year, M6 
                     ")
floods_M6_0 <- sqldf("select year, M6, count(*) as count
                     from tbl2
                     where M6 =0
                     group by year, M6 
                     ")

floods_M6_1 <- sqldf("select year, M6, count(*) as count
                     from tbl2
                     where M6 =1
                     group by year, M6 
                     ")

par(mfrow= c(1,1), mar = c(2,4,3,1))

plot(floods_M6$year,floods_M6$count, col = floods_M6$M6,xlab ="",ylab= "Number of floods", pch =16, main = "Number of floods 1985-2015")
lm1 <- lm(count~as.numeric(year), data = floods_M6_0)
abline(a= lm1$coefficients[1],b = lm1$coefficients[2], col = 1,lwd= 2)
lm2 <- lm(count~as.numeric(year), data = floods_M6_1)
abline(a= lm2$coefficients[1],b = lm2$coefficients[2], col = "red", lwd = 2)
text(2012,20,paste("M > 6: slope = ",round(lm2$coefficients[2],1)), col  = "red")
text(2012,135,paste("M < 6: slope = ",round(lm1$coefficients[2],1)), col  = "black")


floods_cy <- sqldf("select distinct New as Country, year, count(*) as cnt
                    from tbl2 
                    join countries_names on countries_names.Orig = tbl2.Country
                    where M6= 1
                    group by Country ,year
                    order by 1 desc 
                    ")


countries <- data.frame(sqldf("select distinct Country, sum(cnt) 
                              from floods_cy 
                              group by country 
                              order by 2 desc")[,1])


lm_cy = list();
dim(floods_cy)
floods_cy<- na.omit(floods_cy)

for (i in 1:dim(countries)[1]){
  lm_cy[[i]] = lm(floods_cy$cnt[floods_cy$Country==(countries[i,1])] ~ floods_cy$year[floods_cy$Country==(countries[i,1])])

}
par(mfrow =c(3,4), mar = c(1,2,3,2), bg="white")
for (i in 1:12)
  {
  par(mar = c(1,2,4,2), bg="white")
  x = floods_cy$year[floods_cy$Country==(countries[i,1])]
  y = floods_cy$cnt[floods_cy$Country==(countries[i,1])]
  a =(lm_cy[[i]]$coefficients)[1]
  b =(lm_cy[[i]]$coefficients)[2]
  color = (2*(b<0)+2)
  if (i<9)
    plot(x,y, main = countries[i,], ylim = c(0,12),xlim=c(1985,2015),lwd = 4, pch = 16,col = color, xaxt ="n")
  else
  {par( mar = c(2,2,3,2), bg="white")
  plot(x,y, main = countries[i,], ylim = c(0,12),xlim=c(1985,2015), pch=16, lwd = 4,col = color)
  }
  text(x= 2010,y= 11, paste("trend =",round(b,2)),col = (sign(b)+7)/4)
  abline(a,b, lwd = 2)
}
par(mfrow = c(1,1))
title(main = "Floods by country")

##this is just in case we want to see countries 13-24
if(FALSE){
for (i in 13:24){
  x = floods_cy$year[floods_cy$Country==(countries[i,1])]
  y = floods_cy$cnt[floods_cy$Country==(countries[i,1])]
  if (i<7)
    plot(x,y, main = countries[i,], ylim = c(0,4),xlim=c(1985,2015),col = "blue", xaxt ="n")
  else
    plot(x,y, main = countries[i,], ylim = c(0,4),xlim=c(1985,2015), col = "blue")
  a =(lm_cy[[i]]$coefficients)[1]
  b =(lm_cy[[i]]$coefficients)[2]
  text(x= 2005,y= 3.5, paste("trend =",round(b,2)),col = (sign(b)+7)/4)
  abline(a,b)
}
}
