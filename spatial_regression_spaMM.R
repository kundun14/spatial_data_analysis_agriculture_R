#spatial regression
#spatial LMM
# spatially autocorrelated random effect

library(geoR) # data
library(viridis) 
library(DHARMa) # residuales en modelos mixtos generalizados spaciales (temporales etc)

data(ca20) # calcio en el suelo, dem, cluster == region, posicion
data <- data.frame(x = ca20$coords[,1], # en que crs?
                  y = ca20$coords[,2], 
                  calcium = ca20$data, 
                  elevation = ca20$covariate[,1], 
                  region = factor(ca20$covariate[,2]))


# modelo no espacial

m_lm <- lm(calcium ~ elevation + region, data = data) # analisis de covarianza
summary(m_lm)

#(check para correlacion espacial
# mapa de residuales
data$resid <- resid(m_lm)

ggplot(data, aes(x = x, y = y, size = resid)) +
  geom_point(aes(colour = region)) +
  scale_size_continuous(range = c(1,10))

# test

sims <- simulateResiduals(m_lm)
testSpatialAutocorrelation(sims, data$x, data$y, plot = FALSE)
# 
# data:  sims
# observed = 0.0594338, expected = -0.0056497, sd = 0.0069140, p-value < 2.2e-16
# alternative hypothesis: Distance-based autocorrelation ----> considerar autocorrelacion de las observaciones


#estructuras 
# (Matern, Interpolated Markov Random Fields
#CAR ---> CONITIONAL AUTOREGRESIVE MODEL
#AR1----> MODELO AUTOREGRESIVO (TEMPORAL)
#non-gaussian random effects or autocorrelated random coefficien

library(spaMM)
m_spamm <- fitme(calcium ~ elevation + region + Matern(1 | x + y), # estimara los parametros de la fun matern nu y rho
                 data = data, family = "gaussian") # toma tiempo ...

#default, it uses ML rather than REM
# By default the Nugget is set to 0.

# Here the Matern(1|x+y) formula term means that the Matern correlation
# model is fit to the data

summary(m_spamm)
data %>%  dplyr::group_by(region) %>% summarise(mean = mean(calcium))
# region    mean
# 1 1       36.2
# 2 2       46.2
# 3 3       54.2
mean(data$calcium) # 50.67978

#PARAMETROS DEL MODELO ESPACIAL

#PARAMETROS DE LOS EFECTOS FIJOS

------------ Fixed effects (beta) ------------
  Estimate Cond. SE t-value
(Intercept)  34.6296   10.311  3.3585
elevation     0.8432    1.860  0.4534
region2       8.1299    5.062  1.6059
region3      14.5584    4.944  2.9447


# PARAMETROS DE EFECTOS ALEATOREOS

#PARAMETROS DE DISPERSION

#lamba --> varianza de los b´s (U)
#phi  -----> varianza de los residuales (error)

lambda = var(u) for u ~ Gaussian; 
x + y  :  105.3  
# of obs: 178; # of groups: x + y, 178 
-------------- Residual variance  ------------
  phi estimate was 0.00596171

# PARAMETROS DE CORRELACION

# CORRELACION ENTRE LOS EFECTOS ALEATORIOS v *Z = (b) A UNA DISTANCIA d

# The parameter, v, which controls the smoothness of
# the spatial process, should be determined from the
# spatial data
# rho es el parametro de escala -----> range

1.nu      1.rho 
0.43285255 0.01161407 


dist_matrix <- dist(data[, c("x", "y")]) # matriz de distancias, metodo: euclidian
matern_corr <- spaMM::MaternCorr(dist_matrix, rho = 0.01161407 , nu = 0.43285255) # calcular la funcion de correlacion de matern con los parametros estimados en el modelo
plot(as.numeric(dist_matrix), as.numeric(matern_corr),
     xlab = "Distancia m")

# POINT PREDICCION----> raster

# In general, prediction requires as input new x values, and new z values for
# each random effect (for block random effects, the grouping variable should
# thus be provided; and for spatial random effects, new spatial coordinates
# are required).

library(fields) # tools for spatial data
library(raster)

# derive a DEM

elev_m <- Tps(data[,c("x","y")], data$elevation) # Thin plate spline regression 
#lambda  =  5.669116e-0
grilla <- raster(xmn = 4950, xmx = 5970,
            ymn = 4800, ymx = 5720, 
            resolution = 10) # grilla de 10 m

elev_raster <- interpolate(grilla, elev_m) # to rasterlayer

# REGION 

# usamos los puntos de muestreo para deimitar las regiones

pp <- SpatialPolygons(list(Polygons(list(Polygon(ca20$reg1)), ID = "reg1"),
                           Polygons(list(Polygon(ca20$reg2)), ID = "reg2"), 
                           Polygons(list(Polygon(ca20$reg3)), ID = "reg3")))
#raster de region
region_raster <- rasterize(pp, grilla)

# puntos de prediccion

grilla_pred <- expand.grid(x = seq(4960, 5960, length.out = 50),
                      y = seq(4830, 5710, length.out = 50))

grilla_pred$elevation <- extract(elev_raster, grilla_pred[,1:2]) 
grilla_pred$region <- factor(extract(region_raster, grilla_pred[,1:2]))
# remove NAs
grilla_pred <- na.omit(grilla_pred)

# predeccion usando el modelo
grilla_pred$calcium <- as.numeric(predict(m_spamm, grilla_pred))

#mapa
(gg_spamm <- ggplot(grilla_pred,aes(x=x, y=y, fill = calcium)) +
    geom_raster() +
    scale_fill_viridis())