load("C:/Test/Tests-for-GeoSpatial-Data-Scientist-master/Tests-for-GeoSpatial-Data-Scientist-master/inputData/2020-07-21 - input data for test GeoSpatial Data Scientist.RData")

#Loading the required libraries
library(raster)
library(gstat)
library(sp)
library(rgdal)
library(dplyr)
library(sf)
library(RPostgreSQL)
library(dbplyr)
library(rpostgis)

              # Preliminaries:
#Plotting the data

plot(gadm, border="red")
plot(po, cex = 0.25, pch = 20, add = TRUE, col = "blue")


#Checking the projection information

proj4string(gadm)
proj4string(po)

#Checking the the point data

head(po)
tail(po)

# Data cleaning to clip SpatialPoints to the boundaries of SpatialPolygon (removing points outside gadm) 
# subsetting the data

po_subset=po[gadm,]

#assigning projection information to restore the discarded datum occasioned by subsetting

po_subset <- spTransform(po_subset, CRS(proj4string(gadm))) # transform CRS
plot(gadm)
plot(po_subset)

                #Connecting to the PostgreSQL database:

# Connect to local PostgreSQL via dbplyr

dbcon <- dbConnect(RPostgreSQL::PostgreSQL(), 
                   host="localhost", 
                   dbname="JacobTest",
                   user=rstudioapi::askForPassword("Database username"),
                   password=rstudioapi::askForPassword("Database password"))

#Check and create PostGIS extension:

pgPostGIS(dbcon, topology = FALSE, tiger = FALSE, sfcgal = FALSE,
          display = TRUE, exec = TRUE)

#Setting CRS for the database:
crs <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
pgSRID(dbcon, crs)

#Loading SpatialPointDataFrame into the Postgresql database:

pgInsert(dbcon, "soildata", po_subset, new.id = "gid")

#Reading out the data
Soil_data=pgGetGeom(dbcon, "soildata")


                        #IDW Analysis

#Generating the extent of the study area
extent.studyArea=extent(gadm)
extent(gadm)

#defining the extent
x.range =as.numeric(c(34.36346,35.15555))# min/max longitude of the interpolation area
y.range =as.numeric(c(0.425188,1.15124))# min/max latitude of the interpolation area

#Generating extent dataframe for the interpolation grid
grid <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.02), y = seq(from = y.range[1], to = y.range[2], by = 0.02)) # expand points to grid
head(grid)

#Casting the interpolation grid into a spatial points object:
coordinates(grid) <- ~x + y 
class(grid)

# Assigning projection to the interpolation grid:
gadm <- spTransform(gadm, CRS(proj4string(Soil_data))) # transform CRS

## Aligning the projections

projection(grid) <- CRS("+init=epsg:4326")  
projection(Soil_data) <-  CRS("+init=epsg:4326")
projection(gadm) <- CRS("+init=epsg:4326")

# Casting the SpatialPoints to a SpatialPixels object
gridded(grid) <- TRUE
class(grid)


          #Inverse distance weighted interpolation using intercept only model

# Building the model
neighbors = length(gadm)
beta = 2

idw_pH = gstat(formula = pH ~ 1,data = Soil_data, 
                 nmax = neighbors,set = list(idp = beta))

# Using the predict function to interpolate the predictor variable (pH)
pH_Grid <- predict(object = idw_pH, newdata = grid)

#Plotting the model outputs:

plot(pH_Grid, main = "Soil PH")
plot(gadm, add = TRUE, border = "white")
plot(Soil_data, add = TRUE, pch = 19, cex = 0.5, col = "red")

#Clipping the prediction layer to the boundaries of the spatial polygon:

PHLayer <- mask(raster(pH_Grid), gadm)

#Plotting the clipped prediction raster:

main <- expression(paste("Soil PH"))
plot(pHLayer, main = main)
plot(gadm, add = TRUE, border = "Black")
#plot(Soil_data, add = TRUE, pch = 19, cex = 0.5, col = "red")

           

