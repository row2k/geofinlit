# geofinlit
mean distance calculation from zipcode

This is a snippet from a deprecated R project. The data here is from FINRA Foundation's National Financial Capability Study and 2014 Census top 50 U.S. cities by gdp. The useful bit here is mapping the respondent to the closest major city by calculating the orthodromic distance (great-circle distance) to each city via longitude and latitude, selecting the minimum and then merging the result and corresponding city name to the data row.

Calculation here, details in file.
```
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
```

Next Step: revive the project and and see if anything significant comes out of regression.
