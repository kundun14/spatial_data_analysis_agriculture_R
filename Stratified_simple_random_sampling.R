library(sswr)
library(ggplot2)
library(magrittr)
library(survey)
library(QuantileNPCI)
library(tidyverse)

#data grdVoorst

grdVoorst["stratum"] # suelo + uso de tierra

#numero de muestras por estrato proporcional a su area

# tama??o del estrato N_h

N_h <- tapply(X = grdVoorst$stratum, 
               INDEX = grdVoorst$stratum, #as.factor.
               FUN = length) # funcion  

w_h <- N_h/sum(N_h) # pesos por cada estrato (estratos mas grandes tiene mayor peso)

n <- 40 # tama??o muestral en total
print(n_h <- round(n * w_h)) # n de muestras por strato a entero
print(sum(n_h))
n_h[1] <- n_h[1] - 1 # coreccion de 41 a 40 unidades muestrales

# stratified simple random sample is selected with function strata() of package sampling
# Argument size specifies the stratum sample sizes.

library(sampling)

ord <- unique(grdVoorst$stratum) # orden de aparicion de los stratos en la data
set.seed(314)

#unidades muestrales seleccionadas mediante simple strat random sampling

units <- sampling::strata(
  data = grdVoorst, 
  stratanames = "stratum", 
  size = n_h[ord],# en orden en la que los estratos aparecen en la data original
  method = "srswr") # simple random sampling with replacement

#simple random sampling with replacement (method = "srswr")
#more than one point can be selected within a grid cell, (chp 1)

mysample <- sampling::getdata(grdVoorst, units) %>%
  mutate(s1 = s1 %>% jitter(amount = 25/2),
         s2 = s2 %>% jitter(amount = 25 / 2))

#Estimation of population parameters

#media de  por estrato
mz_h <- tapply(X = mysample$z, INDEX = mysample$stratum, FUN = "mean")

# media total es la sumatoria de las medias por estrato por los pesos por estrato

m_z <- sum(mz_h*w_h) # 86.33397 g C / Kg

# estimated variance of   z within stratum   h

S2z_h <- tapply(X = mysample$z, INDEX = mysample$stratum, FUN = "var")

#estimated sampling variance of the median per stratum h 

v_mz_h <-  S2z_h/ n_h

# standar  error of the estimator of the population mean

se_mz <- sqrt(sum(w_h^2* v_mz_h )) # 5.816711 g C / Kg

#resultados dataframe

resultados_ssrs <- data.frame(NH = N_h,
            nH = n_h,
           mediaz_h = mz_h,
           varz_h = S2z_h,
           se_h = sqrt(v_mz_h)
           )

#The population mean can also be estimated 
#directly using the basic  $B&P(Bestimator

head(mysample)

z_total <- sum(mysample$z / mysample$Prob )
mz <- z_total/sum(N_h) #86.53333

# si ignoramos que la data deriba de un muestreo estratificado simple aleatorio
# y calculamos la media como si fuera muestreo simple aleatorio


print(mean(mysample$z)) #86.11247

# ESTIMACION CON EL PAQUETE SURVEY

library("survey")

labels <- sort(unique(mysample$stratum))
lut <- data.frame(stratum = labels, pesos = w_h)
mysample <- merge(x = mysample, y = lut)


desing_stsi <- svydesign(ids = ~1, # no clysters
                         weight  = ~ pesos, #probabilidades de muestreo
                         strata = ~ stratum, 
                         data = mysample) 

#media 
svymean(~z, design = desing_stsi)
# mean     SE
# z 84.718 5.3755

# design effect DeFF

design_stsi <- svydesign(
  id = ~ 1, strata = ~ stratum, weight = ~ pesos, data = mysample)
svymean(~ z, design_stsi, deff = "replace")

#     mean      SE   DEff
# z 84.7183  5.3755 0.68

#DeEff es menor a 1, muestreo estratificado simple aleatorio es mas
# eficiente  que el muestreo simple aleatorio

# si la suma de cuadrados de las medias de los estratos en mayor 
# a la suma de cuadrados de las medias dentro de los estratos

#intervalos de confianza

#Cum-root-f (ESTRATIFICACION USANDO UNA COVARIABLE CONTINUA)
# metodos parecidos a clasificacion por breakspoints o k means (cluster) etc.

#dem como variable de estratificacion

# install.packages("stratification")
library(stratification)

grdXuancheng <- grdXuancheng  %>% 
  dplyr::arrange(dem) %>% 
  dplyr::mutate(dem_new = dem + abs(min(dem)))

#boundaries to stratification
crfstrata <- stratification::strata.cumrootf(
  x = grdXuancheng$dem_new, 
  n = 100,
  Ls = 5, # numero de estratos
  nclass = 500)
print(bh <- crfstrata$bh) # boundaries 46.728 108.324 214.524 384.444
#ajuntar numero de estrato a la data original
grdXuancheng$crfstrata <- crfstrata$stratumID

# ESTRATIFICACION USANDO VARIABLES COVARIABLES (clusters)

library(sp)
sp::gridded(grdXuancheng) <- ~ s1 + s2 # to SpatialPixelsDataFrame
# variables : slope, temperature , twi, profile.curvature

# seleccionar puntos de muestreo de los rasters (usando una grilla regular)

subgrd  <- sp::spsample(
           x = grdXuancheng, 
           type = "regular", 
           cellsize = 400, 
           offset = c(0.5,0.5))

subgrd <- data.frame(coordinates(subgrd), 
                     over(subgrd,grdXuancheng)) # ?over

head(subgrd)  # dataframe con coordenadas y valores de las covariables (REGRESSION MATRIX)
# ?spsample

x <- c("dem", "temperature", "slope", "profile.curvature", "twi") # covariables utilizadas
set.seed(314)
myClusters <- kmeans(x = scale(subgrd[, x]), # scale standariza la data
                     centers = 5, 
                     iter.max = 1000, 
                     nstart = 100)

subgrd$cluster <- myClusters$cluster
#The size of the clusters used as strata is largely different 


# ESTRATIFICACION GEOGRAFICA (sin covariables disponibles)

#Geographical stratification improves the spatial coverage spcosampling
# geostrata

# install.packages("spcosa") #(D. Walvoort, Brus, and de Gruijter (2020))
library(spcosa)
library(sp)


#equalArea = FALSE
# Optionally, the strata may be forced to be of equal size. 
# This facilitates field work in case of stratified simple 
# random sampling for composites

set.seed(314)

gridded(subgrd) <- ~ x1 + x2

mygeostrata <- spcosa::stratify(
  subgrd, 
  nStrata = 2 , # 2 stratos como prueba
  nTry = 1, # N de configuraciones iniciales
  equalArea = TRUE
) 

# tarda bastante...

#muestrear dos puntos por geostrata

mysample <- spcosa::spsample(mygeostrata, , n = 10) # 10 puntos por geostrato
str(mysample)
plot(mysample@sample@coords)

#analisis de los puntos muestreados
# funcion estimate()

mydata <- data.frame (z = rnorm(20, mean= , sd = 2)) # data simulada como ejemplo

mean <- estimate("spatial mean", mygeostrata, mysample, data = mydata) # -0.05093418 
se <- estimate("standard error",mygeostrata, mysample, data = mydata ) #0.3822978
print(se)


# MULTIWAY STRATIFICATION (categorical + continous covariates)
#MULTIVARIATE STRATIFICATION ( + 1 variable de respuesta)






