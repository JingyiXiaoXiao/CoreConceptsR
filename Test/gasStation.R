# Testing Core Concepts in R using Gas Station Data

# non core conputation
library(rgdal)
library(rgeos)
setwd("Dropbox/gas5")
gas <- readOGR(dsn = "sbc_gas",layer = "sbc_gas")
block <- readOGR(dsn = "block2010_sbc",layer = "block2010_sbc")
school <- readOGR(dsn = "all_schools", layer = "all_schools")
shop <- readOGR(dsn = "shopping_centers", layer = "shopping_centers")

# calculate the number of gas station within 3000 meters
len <- length(gas)
numNei <- integer(len)

for(i in 1:len){
  for(j in 1:len){
    dis <- gDistance(gas[i,], gas[j,])
  if(dis <=  3000)
    numNei[i] <- numNei[i]+1
  }
}

numNei <- numNei-1
gas$numNei <- numNei

# gDistance can calculate distance using ID
distan <- gDistance(gas, byid=TRUE)
distan2 <- gWithinDistance(gas, 3000, byid = TRUE)
distan3 <- gDistance(gas, shop, byid = TRUE)

di <- t(distan3)
nearShop <- integer(len)
for(i in 1:len){
  nearShop[i] <- sum(di[i,]<3000)
}

gas$nearShop <- nearShop
  

spTransform(gas, CRS("+init=epsg:3395"))
buf <- gBuffer(repro, width = 39355, capStyle="ROUND")
return (spTransform(buf, CRS("+proj=longlat +datum=WGS84")))


# core computations
setwd("Documents/Programming/R/CoreConceptsR")
source('R/field.R')
source('R/object.R')

setwd("Dropbox")

# load data
gas <- object("sbc_gas","sbc_gas")
