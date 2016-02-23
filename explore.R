install.packages("lubridate")
install.packages("stringr")
require(stringr)
require(lubridate)
tbl <- read.csv('GlobalFloodsRecord.csv',header=TRUE)
tbl$Began[tbl$Began==""] <-NA

tbl$Began
tbl$year_beg <- as.character(tbl$Began)
tbl$year_beg <- tbl$year_beg[-2]

tbl$year_beg <- str_sub(tbl$year_beg,-2,-1)

tbl$M.6



class(tbl$year_beg)
tbl$Began
