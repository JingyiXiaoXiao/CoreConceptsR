# Core Concepts of Spatial Information
# Object computations
# Jingyi Xiao
# 08/2017

# library 
library(rgdal)
library(rgeos)


# Object constructor
# Arguments
## foldfername: data source name (folder name)
## filename: file name, e.g., 'road.shp' file name is road.
# Reurn: a spatial object
object <- function(foldername, filename){
  return (readOGR(dsn = foldername,layer = filename))
}

# buffer: Object operation
# Arguments
## object: spatial object
## num: buffer distance, could be 0.1 degree, or 1000 meters
## unit: buffer distance unit, support degree and meter, kilometer, feet, mile for now.
# Return: buffered object
buffer <- function(object, num, unit){
  repro <- spTransform(object, CRS("+init=epsg:3395"))
  if(unit == "degree")
    ####  very imprecise, depends on latitude. I used equator as standard now.
    buf <- gBuffer(repro, width = num*111320, capStyle="ROUND")
  else if(unit == "meter")
    buf <- gBuffer(repro, width = num, capStyle="ROUND")
  else if(unit == "kilometer")
    buf <- gBuffer(repro, width = num*1000, capStyle="ROUND")
  else if(unit == "feet")
    buf <- gBuffer(repro, width = num*0.3048, capStyle="ROUND")
  else if(unit == "mile")
    buf <- gBuffer(repro, width = num*1609.34, capStyle="ROUND")
  else
    stop("Invalid unit.")
  return (spTransform(buf, CRS("+proj=longlat +datum=WGS84")))
}

# bounds: return bounds of an object
# Arguments
## object: spatial object\
## type: bounding box or convex hull
# Return: bounding box of the object
bound <- function(object, type = "bb"){
  # return an extent object, like a boundingbox
  if(type == "bb")
    return(gEnvelope(object))
  else if(type == 'ch')
    return(gConvexHull(object))
  else
    stop("Invalid type.")
}

# property: return the value of object's property
# Arguments
## object: spatial object
## prop: property
# Return: property value
property <- function(object, prop){
  return(object$prop)
}

# identity: check if two objects are idenitical
# Arguments
## object1, object2: spatial object
# Return: TRUE or FALSE
identity <- function(object1, object2){
  gEqualsExact(object1, object2)
}


# relation: check two objects' relation
# Arguments
## object1, object2: spatial object
# Return: relation, including 
# have to check relations many times
relation <- function(object1, object2){
  if(gIntersects(object1,object2)){
    if(gTouches(object1,object2)){
      return("Touch")
    }
    else if(gCrosses(object1, object2)){
      if(gOverlaps(object1,object2)){
        if(gContains(object1,object2)){
          if(gWithin(object1,object2))
            return("Equal")
          else return("Contain")
        }
        else if(gContains(object2,object1))
          return("Within")
        else return("Overlap")
      }
      else return("Cross")
    }
  }
  else return("Disjoint")
}



