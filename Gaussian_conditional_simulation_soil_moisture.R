#alt + shif + m = %>%

library(sf)
library(sp)
data.Set4.2 <- read.csv("./data/Set4/Set4.296sample.csv",
                         header= TRUE)
Y.full.data.sf <- st_read("./Created/Set42pop.shp")
Y.full.data.sp <- as(Y.full.data.sf, "Spatial") #coerce sf to SpatialPointsDataFrame

gridded(Y.full.data.sp) <- TRUE #SpatialPointsDataFrame to SpatialPixelsDataFrame
proj4string(Y.full.data.sp) <- CRS("+proj=utm +zone=10 +ellps=WGS84")

#puntos de muestreo
sample.coords <- cbind(data.Set4.2$Easting, 
                       data.Set4.2$Northing)     


closest.point <- function(sample.pt, grid.data){
  dist.sq <- (coordinates(grid.data)[,1]-
              sample.pt[1])^2 + 
              (coordinates(grid.data)[,2] -
               sample.pt[2])^2

return(which.min(dist.sq)) }


samp.pts <- apply(sample.coords, 
                  1, 
                  closest.point,
                  grid.data = Y.full.data.sp)


data.Set4.2$ptno <- samp.pts
data.Set4.2$Yield <- Y.full.data.sp$Yield[samp.pts]

#kriging

data.Set4.2.sp <- data.Set4.2
coordinates(data.Set4.2.sp) <- c("Easting", "Northing")
proj4string(data.Set4.2.sp) <- CRS("+proj=utm +zone=10 +ellps=WGS84")

library(gstat)
#variograma experimental
data.var <- variogram(Yield~1, 
                      locations = data.Set4.2.sp,
                      cutoff = 600)
#modelo 
data.fit <- fit.variogram(object = data.var, 
                          model = vgm(psill= 150000,
                                      model = "Sph", 
                                      range= 250, 
                                      1)) 
#[using ordinary kriging]
#interpolacion usando el modelo de covarianza

Yield.krige <- krige(formula = Yield ~ 1 , #for ordinary and simple kriging use the formula z~1
                     locations = data.Set4.2.sp, # puntos de muestreo sppoints
                     newdata = Y.full.data.sp ,  # grilla de prediccion sppixels
                     model = data.fit )

# conditional Gaussian simulation algorithm

#simulacion cond gaussiana
#casos tipicos cond Gaussian sim > krging
#zones of high permeability values or zones rich in a metal concentration
#smoothing is minimal close to the data locations and increases as the location being estimated gets farther awa

#CONDICIONES
# la variable de respuesta debe ser normal
#PASOS
# tranformacion normal
# random path 
 ## loop
  ### buscar puntos en el neiborhood (muestrales y simulados previamnete)
  ### calcular distribucion condicional basado en los puntos encontrados (usando el variograma para estimar la covarianza )
  ### monte carlo para estimar un valor de la distrubucion


#Assume that the region is stationary and isotropic
# se usa kriging simple (media conocida)
# Z(x) = y(x)-mu
# E[Z] = 0

Y.mean <- mean(data.Set4.2.sp$Yield)
data.Set4.2$Z <- data.Set4.2.sp$Yield - Y.mean
#puntos de ejemplo

data.4pts <- data.Set4.2[which(data.Set4.2$Easting < 592200 & data.Set4.2$Northing > 4267750),]
h4.mat <- as.matrix(with(data.4pts, dist(cbind(Easting, Northing)))) #  matrix de distancias entre puntos 

# modelo de variograma

nugget <- data.fit$psill[1]
psill <- data.fit$psill[2]
rnge <- data.fit$range[2]

# funcion de variograma esferico
 

sph.vgm <- function(nug, psill,rnge, hh){ ## hh distancias entre puntos
  if (hh == 0) return(0)
  if (hh > rnge) return(nug+ psill)
  return(nug + psill*(3*hh)/(2*rnge)-0.5*(hh/rnge)^30) # ecuacion modelo
}

#matriz de semivarianzas
gam.mat <- matrix(0, 4, 4)

for (i in 1:4) 
  for (j in 1:4)
    gam.mat[i,j] <- sph.vgm(0, psill, rnge, h4.mat[i,j])

#matriz de covarianza

C.full <- (nugget + psill) - gam.mat # nugget + psill = varianza

#hallar phi

C.mat <- C.full[1:3, 1:3] # cov entres puntos
c.vec <- C.full[1:3, 4] # cov entre puntos y punto de prediccion
phi <- solve(C.mat, c.vec)

#prediccion
Z.pred <- sum(phi*data.4pts$Z[1:3])

# error <- Z.pred - data.4pts$Z[4]

#kriging simple como regresion 


# mormal trans
data.Yield.ord <-data.Set4.2$Yield[order(data.Set4.2$Yield)]
data.Set4.2.ord <- data.frame(Yield = data.Yield.ord ,
                              Yquant = seq(1:length(data.Yield.ord)) / length(data.Yield.ord) ,
                              Easting = data.Set4.2$Easting[order(data.Set4.2$Yield)],
                              Northing = data.Set4.2$Northing[order(data.Set4.2$Yield)],
                              ptno = data.Set4.2$ptno[order(data.Set4.2$Yield)])

data.Set4.2.ord$Z <- qnorm(data.Set4.2.ord$Yquant)
data.Set4.2.ord$Z[length(data.Set4.2.ord$Z)]
data.Set4.2.ord$Z[length(data.Set4.2.ord$Z) - 1]
data.Set4.2.ord$Z[length(data.Set4.2.ord$Z)] <- 3.0 
mean(data.Set4.2.ord$Z) #media de Z debe ser 0

mean(data.Set4.2.ord$Z <- data.Set4.2.ord$Z - mean(data.Set4.2.ord$Z) 
) # media cero
  

#Conditional Gaussian Simulation Algorithm
set.seed(123)
ptl <- sample(samp.pts,1) # muestrea solo un punto (punto inicial)
visit.order <- sample(1:length(data.Set4.2.ord$Z), replace = FALSE) # muestrea posiciones de Z
visit.order

visit.order <- c(ptl, visit.order[-which(visit.order == ptl)])
# visit.order[1:10]

Y.full.data.sf$sim <- 0 # creamos un field para guardar los resultados de la simulacion 

data.Set4.2.ord.sp <- data.Set4.2.ord
coordinates(data.Set4.2.ord.sp) <- c("Easting", "Northing")
proj4string(data.Set4.2.ord.sp) <- CRS("+proj=utm +zone=10 +ellps=WGS84")

data.varn <- variogram(object = Z ~ 1, 
                       locations = data.Set4.2.ord.sp, 
                       cutoff = 600)

data.fitn <- fit.variogram(object = data.varn, 
                           model =vgm(1,"Sph", 250, 1)) #vgm son parmaetros de inicialicion

# parametros del variograma

print(Z.nug <- data.fitn$psill[1])
print(Z.sill <- Z.nug + data.fitn$psill[2])
print(Z.range <- data.fitn$range[2])

#closest.points() to find the closest points to the current visit point

closest.points <- function(visit.pt, coord.data, known.pts, n.pts){
  c.pts <- numeric(n.pts)
  #coordenadas del punto actual
  visit.coords <- with(coord.data, c(Easting[visit.pt], 
                                     Northing[visit.pt]))
  #coordenadas de los demas puntos
  kwnon.coords <-  with(coord.data, cbind(Easting[known.pts], 
                                          Northing[known.pts]))
  # iterativamente encontrar los puntos mas ccercanos
  test.coords <- known.coords
}

#la implementacion es muy larga , REVISAR BIEN
#SOBRE TODO EL ALGORIMO PARA BUSCCAR PUNTOS CERCANOS 

#cond Gaussian sim in gstat

#INPUTS 
#SpatialPointsDataFrame para sp::variogram()

data.Set4.2.ord.sp <- data.Set4.2.ord
coordinates(data.Set4.2.ord.sp) <- c("Easting", "Northing")
proj4string(data.Set4.2.ord.sp) <- CRS("+proj=utm +zone=10 +ellps=WGS84")

data.varn <- variogram(object = Z ~ 1 , 
                       locations = data.Set4.2.ord.sp, #known data values 78 puntos de muestreo
                       cutoff = 600)

data.fitn <- fit.variogram(object = data.varn, 
                           model= vgm(1 , "Sph", 250, 1))
  
Z.sim <- krige(formula = Z ~ 1,
               locations = data.Set4.2.ord.sp,  #known data values 78 puntos de muestreo SpatialPointsDataFrame
               newdata = Y.full.data.sp, #puntos de simulacion SpatialPixelsDataFrame
               model = data.fitn, 
               nmax = 10, # neiboorhood 
               nsim = 6) # n de simulaciones

class(Z.sim) #"SpatialPixelsDataFrame"
spplot(Z.sim)

#-------- back transformation

# install_github("envirometrix/plotKML", ref = "stars")
library(plotKLM)
data("eberg")  

eberg <- eberg[runif(nrow(eberg))<0.2]


dbinom( 6,size=9,prob=0.5)
