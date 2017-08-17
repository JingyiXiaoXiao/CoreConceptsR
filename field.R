# Core Concepts of Spatial Information
# Field computations
# Jingyi Xiao
# 08/2017


library(raster)

# constructor of field
# x could be image, matrix
# domain and granularity are optional
field <- function(x, domain = NULL, sn = NULL, granularity = NULL){
  if(is.null(x))
    stop("Input is needed for field data.")
  else 
    fld <- raster(x)
  
  if(! (is.null(domain) | is.null(sn)) )
    fld <- setDomain(fld,domain,sn)
  if(!is.null(granularity))
    fld <- setGranularity(fld,granularity)
  
  return(fld)
}


# get the value of a location in the field
# support index, row/col, # lat/lon query
valueAt <- function(field, qm = "index", var1, var2 = NULL){
  if(class(field) != "RasterLayer")
    stop("Invalid field data.")
  else if(qm == "index" & (var1%%1 == 0))
    return(field[var1])
  else if(qm == "rowcol" & (var1%%1 == 0) & (var2%%1 == 0))
    # var1, var2 should be in the range of [0,nrow], [0, ncol], 
    # which is checked in raster package
    return(field[var1,var2])
  else stop("Invalid location.")
}

# set the value of a location in the field
setValue <- function(field, qm = "index", var1, var2 = NULL, value){
  if(class(field) != "RasterLayer")
    stop("Invalid field data")
  else if(!is.numeric(value))
    stop("Invalid value for this location.")
  else if(qm == "index" & (var1%%1 == 0)){
    field[var1] <- value
    return(field)
  }
  else if(qm == "rowcol" & (var1%%1 == 0) & (var2%%1 == 0)){
    # var1, var2 should be in the range of [0,nrow], [0, ncol], 
    # which is checked in raster package
    field[var1,var2] <- value
    return(field)
  }
  # else if(qm == "latlon")
  #   return(field[var1,var2])
  else stop("Invalid location.")
}

# get a field's domain
# what kind of value should reture
# for now
domain <- function(field){
  return (rasterToPolygons(field))
}

# restrict the domain
# domain could be raster or features
setDomain <- function(field, domain, sn = "in"){
  if(class(field) != "RasterLayer")
    stop("Invalid field data")
  else{
    if(sn == "in"){
      fld <- crop(field,extent(domain),snap=sn)
      fld <- mask(x=fld , mask = domain)
    }
    else if(sn == "out"){
      # !!!!! extent needs to be the same. figure out
      fld <- mask(x=field, mask = domain, inverse = TRUE)
    }
    else 
      stop("Invalid domain data.")
  }
  return (fld) 
}


# map algebra
# local operations: operations on every single cell
# field1, field2... are input field data

local <- function(field1, fun){
  if(class(field1) != "RasterLayer")
    stop("Invalid input field data.")
  else if(is.null(fun)){
    return(calc(field1, fun))
  }
}
# local arithmetic operations for two or more fields are avaliable
# use +,-,*,/ directly. e.g., field1s + field2 


# focal operations: operations on not just one cell but also neighbor cells
# focal operations have already been implemented in raster package

# focal(field, weightMtatrix, fun)
# focalWeight(field, d, type=c('circle', 'Gauss', 'rectangle')) 
# is the window used for focal operations

# zonal operations
# zonal operations have already been implemented in raster package
# zonal(field, zone, fun)
# functions could be mean, sd, min, max, sum.


## get granularity of fields 
getGranularity <- function(field){
  if(class(field) == "RasterLayer"){
    return (res(field))
  }
  else 
    stop("can't get granularity")
}

# set granularity for fields 
# could be finer or corse granularity
setGranularity <- function(field, x, y = NULL, func = mean){
  if(class(field) == "RasterLayer"){
    xr <- xres(field)
   yr <- yres(field)
    if(is.null(y)){
      if(x >= xr)
        n <- aggregate(field, fact = x/xr, fun = func)
      else
        n <- disaggregate(field, fact = xr/x, fun = func)
    }
    else{
      if(x >= xr && y >= y)
        n <- aggregate(field, fact = c(x/xr,y/yr), fun = func)
      else if(x <= xr && y <= yr)
        n <- disaggregate(field, fact = c(xr/x, yr/y), fun = func)
      else
        stop("Invalid granularity.")
    }
 return(n)
 }
 else stop("Invalid field data.")
}



# more operations
# field to object

