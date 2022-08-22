library(sswr)
library(ggplot2)
library(magrittr)
library(survey)
library(QuantileNPCI)
library(tidyverse)
library(sampling)
library(survey)
library(stratification)
library(sp)
library(spcosa)

#GRILLA CUADRADA
# A systematic sample can be selected with function spsample 
# of package sp with argument type = "regular" 
# (Bivand, Pebesma, and Gómez-Rubio 2013).

gridded(grdVoorst) <- ~s1 +s2
n <- 40 # N. de unidades muestreada
set.seed(777)

mySample <- spsample(x = grdVoorst, n = n, type = "regular") %>%
  as("data.frame") # puntos de muestreo

#cell size (resolucion esp. 25 m)

cell_size <- 25
n_pixels <- nrow(grdVoorst)
area <- n_pixels * cell_size^2

print(spacing <- sqrt(area/n)) # distancia entre puntos muestrales
# dada el area y la resolucion espacial se puede estimar el n estimado

area/cell_size^2
# systematic random sampling results in more precise estimates of the mean or total

#CONTROLAR EL NUMERO DE MUESTRAS

# grilla RECTANGULAR
dy <- 1000/3 # distancia vertical de area de estudio 1000m
dx <- area/(n*dy)

mySYsample_rect <- spsample(
x = grdVoorst, cellsize = c(dx, dy), type = "regular")

