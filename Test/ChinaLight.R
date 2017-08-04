# Testing Core Concepts in R using China Light data


setwd("Documents/Programming/R/CoreConceptsR")
source('R/field.R')
source('R/object.R')

setwd("~")
setwd('Dropbox/ChinaLight')

# load data
light1 <- field('nightLights.tif')
light2 <- field('nightLights2.tif')

ChinaBoundary <- object('CHN_adm','CHN_adm0')
gasFlare <- object("Flares_China_1","Flares_China_1")
road <- object("CHN_rds","CHN_rds")

# get buffer of road
roadBuffer <- buffer(road, 0.5, "degree")

# China road buffer without gas flare within China boundary
domain <- gIntersection(ChinaBoundary,gDifference(roadBuffer, gasFlare))

# 
light <- setDomain((light1+light2)/2, domain)
plot(light, col= grey(1:256/256), axes=F, box= F)
writeRaster(light, filename = "light.tif",format="GTiff")
light <- setGranularity(light, xres = 0.1)
writeRaster(light, filename = "light_corsen.tif",format="GTiff")
