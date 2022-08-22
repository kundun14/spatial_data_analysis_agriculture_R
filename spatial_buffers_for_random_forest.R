#instll packs
install.packages("GSIF", repos=c("http://R-Forge.R-project.org"), type = "source") # install from forge
packages = c("ranger", "rgdal", 
             "raster", "geoR", "gstat",
             "intamap", "plyr", "plotKML",
             "scales", "RCurl", "parallel",
             "lattice", "gridExtra")
install.packages(setdiff(packages, rownames(installed.packages())))  

library(ranger)
library(GSIF)
library(rgdal)
library(raster)
library(geoR)
library(gstat)
library(intamap)
library(plyr)
library(plotKML)
library(scales)
library(RCurl)
library(parallel)
library(lattice)
library(gridExtra)
library(entropy)
library(snowfall)

#funciones locales #SOURCE()
source('./RF_vs_kriging/R/RFsp_functions.R')

demo(meuse, echo=FALSE) #data
#distBuffer
grid.dist0 = GSIF::buffer.dist(meuse["zinc"], meuse.grid[1], as.factor(1:nrow(meuse)))


#2D predictions
#If no other information is available, we can use buffer distances to all points as covariates to predict values of some
#continuous or categorical variable in the RFsp framework



## Soil organic matter (distance from any to all points):
grid.dist0 = buffer.dist(meuse["om"], meuse.grid[1], as.factor(1:nrow(meuse)))


