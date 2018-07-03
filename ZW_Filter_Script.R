require(ggplot2)
require(dplyr)
require(plyr)
require(readr)
require(ggthemes)
library(readxl)
require(doBy)
require(xlsReadWrite)
require(tcltk) #use this for tkchoosedirectory. This opens last used folder (base r package opens my computer every time)
require(signal)

rm(list=ls())

#Get name of folder containing the data. Define some properties of the current foil
dir <- "C:/Users/zane/Documents/Research/Projects/DuoPneufish_Project/Data_for_Filter_Comp/"
setwd(dir)
#finPos <- readline(prompt="What fin position? (lower case)")
#material <- readline(prompt="What material? (lower case)")

# file.list <- list.files(pattern='*.xlsx')
# df.list <- lapply(file.list, read_excel)

#Read the names of the .csv files in the current folder
fileNames <- list.files(pattern="*.xls", all.files=TRUE, no..=TRUE)
# 

df <- read_xlsx('Data_Summary.xlsx')

#choose one column to practice on
headers <- colnames(df)

foo <- select(df, headers[1])
data <- do.call(rbind, lapply(foo, as.numeric))
typeof(data)
allDataDF <- data.frame()

foo2 <- select(df, headers[200])
data2 <- do.call(rbind, lapply(foo2, as.numeric))
typeof(data)
allDataDF <- data.frame()


time <- seq(1, 10000, by=1)
x <- 1:10000
df2 <- data.frame(x=time, y=foo)
df22 <- data.frame(x=time, y=foo2)
plot(df2)

plot(df2)
bf <- butter(2, 1/150, type="low")
b <- filter(bf, data)
points(x, b, col="red", pch=20)

plot(df22)
bf <- butter(1, 1/150, type="low")
b <- filter(bf, data)
points(x, b, col="red", pch=20)
# 
# bf <- butter(2, 1/25, type="high")
# b <- filter(bf, data)
# points(x, b, col="black", pch=20)

#Create find_peaks function
find_peaks <- function (x, m = 100){
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks <- unlist(pks)
  pks
}

