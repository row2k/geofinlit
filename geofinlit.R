library("foreign")
library("ggplot2")
library("ggmap")
library("lattice")
library("xtable")
library("dplyr")
library("reshape2")
library("Hmisc")
library("stringr")

options(max.print=26000)

load("./geofinlit.rdata")
#edit(geofinlit)

citygdpcoord <- read.csv("./citygdpcoord.csv")

#make progress bar
#pb <- winProgressBar(title="progress bar", label="0% done", min=0, max=100, initial=0)

#Generate the distance from each respondent to every top50 US city
dat <- lapply(1:50, function(j) { 
  tmp <- matrix(0, nrow = 25509, ncol = 1) 
  for (i in 1:25509) {
    radius <- 6371.393
    lat1 <- geofinlit$latitude[i]*pi/180
    lon1 <- geofinlit$longitude[i]*pi/180
    lat2 <- citygdpcoord$latitude[j]*pi/180
    lon2 <- citygdpcoord$longitude[j]*pi/180
    diflat <- abs(lat2-lat1)
    diflon <- abs(lon2-lon1)
    orthodromic <- radius*acos(cos(lat1)*cos(lat2)*cos(lon1-lon2)+sin(lat1)*sin(lat2))
    tmp[i, ] <- c(orthodromic)
  }
  return(as.data.frame(tmp)) 
})

#make dataset and name the variables
dist.all <- as.data.frame(dat)
names(dist.all) <- paste("dist",citygdpcoord$citycode[1:50], sep = ".")

#Get the minimum distance and indicate the city
dist.minimum <- sapply(seq(nrow(dist.all)), function(i) {
  j <- which.min(dist.all[i,])
  c(paste("", colnames(dist.all)[j], sep=''), dist.all[i,j])
})
minimum <- as.data.frame(t(as.data.frame(dist.minimum)))
row.names(minimum) <- NULL
colnames(minimum) <- c('city', 'temp.min')
temp <- str_split_fixed(minimum$city, "dist.", 2)
minimum <- bind_cols(minimum, as.data.frame(temp))
colnames(minimum) <- c('aa1', 'dist.min','aa2','dist.min.city')
minimum$aa1 <- NULL
minimum$aa2 <- NULL
rm(temp)
#combine data sets
spatial.finlit <- bind_cols(geofinlit,minimum)
#save data
write.csv(spatial.finlit, file = "./spatial.finlit.csv")
#clean up
rm(list=ls(all=TRUE))

