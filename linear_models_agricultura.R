library(agridat)
library(tidyverse)
library(ggplot2)
# library(ggmosaic) #mosaic plot

data <- lasrosas.corn

#varianza de yield por nivel de nitrogeno
tapply(data$yield, INDEX = data$nf, FUN = var) # r base
data %>%  dplyr::group_by(nf) %>% summarise(var = var(yield)) # dplyr
# varianza de yield por region geografica
# xy <- data[, c(2,3)] # puntos de muestreo
# data_sp <- SpatialPointsDataFrame(coords = xy, 
#                                    data = data,
#                                    proj4string = CRS("+init=epsg:4326"))
# 
# gridded(data_sp)
# 
# data_sp@bbox
# library(spcosa) # implementa  k-means algorithm en el espacio geografico
# stratification <- spcosa::stratify(data_sp, nStrata = 100, nTry = 10)

#datos balanceados
tapply(data$yield, INDEX=data$nf, FUN=length)  
# N0  N1  N2  N3  N4  N5 
# 573 577 571 575 572 575 
#anova no es aplicable

means.nf = tapply(data$yield, INDEX=data$nf, FUN=mean)  # media

std.error <-function(x){
  sd(x)/sqrt(length(x))
}

StdErr.nf = tapply(data$yield, INDEX=data$nf, FUN= std.error)  # error estandar de la media
# N0        N1        N2        N3        N4        N5 
# 0.8748421 0.7994946 0.8080915 0.8018004 0.8005357 0.8398399 

#anova
#hipotesis 
## nf afecta el rendimiento (yield) +
mod1 = aov(yield ~ nf, data = data)
## las medias del rendimiento para cada nivel de N son diferentes estadisticamente
# tukey
TukeyHSD(mod1,conf.level = 0.95)
model.tables(mod1, type="effects")  #efectos de los niveles repecto a la media global

#Nonparametric One-Way ANOVA-----> no normalidad de la data

# el nivel de ferti con N afecta el rendimiento
kruskal.test(yield ~ nf, data=data)  
# 
# Kruskal-Wallis rank sum test
# 
# data:  yield by nf
# Kruskal-Wallis chi-squared = 81.217, df = 5, p-value = 4.669e-16

# si el p valor es muy pequeño si exisste significancia


mod2 = lm(yield ~ nf, data=data)   
summary(mod2)
anova(mod2) # el algoritmo de calculo e suma de cuadrados no es adecuada pen data no balanceada

#data no balanaceada
library(car)
car::Anova(mod2, type = c("III")) 

#Two-way ANOVA (dos viables predictoras)
# nitrogen level + topo (dos var catgoricas)

means.topo = tapply(data$yield, INDEX=data$topo, FUN=mean)
StdErr.topo = tapply(data$yield, INDEX=data$topo, FUN= std.error)

#hipotesis
# Nitrogen level afecta el rendimiento
# la clase topografica afeccta el rendimiento

mod1b <- aov( yield ~ nf + topo, data = data)
summary(mod1b)
model.tables(mod1b, type="effects")
TukeyHSD(mod1b, conf.level=0.95, which=c("topo")) # que niveles son sig diferentes


# Two-Way ANOVA con Interacciones
# si en baja topografia el nivel de nitrogeno afecta mas al rendimiento que en topografia alta

means.INT = tapply(dat$yield, INDEX=list(dat$nf, dat$topo), FUN=mean)  

#medias 
data %>%  
  group_by(topo, nf) %>% 
  summarise(mean = mean(yield))

# estimacion densidad con kernels
# W_N5 <- data %>%  
#   dplyr::filter(topo == "W" &  nf == "N5") 
# W_N5_yield <- W_N5$yield # solo yield
# n = sum(!is.na(W_N5_yield)) # valores no nulos
# 
# mean_W_N5_yield = mean(W_N5_yield, na.rm = T) # 69.65933
# var_W_N5_yield = var(W_N5_yield, na.rm = T) # 236.7384
# sd_W_N5_yield = sd(W_N5_yield, na.rm = T) # 15.38631
# 
# # valores de kernel density estimates
# density_W_N5_yield = density(W_N5_yield, na.rm = T) # Bandwidth 'bw' = 4.7
# n_density_W_N5_yield = density_W_N5_yield$n # n de puntos utilizados en la estiamcion de densidad de prob
# bw_density_W_N5_yield = density_W_N5_yield$bw
# plot(density_W_N5_yield, 
#      main = "Kernel Density Estimates of yield \n" , 
#      xlab = "Yield", 
#      ylab = "Density", 
#      ylim = c(0, max(density_W_N5_yield$y, na.rm = T)), 
#      lty = 1)
# 
# #plot densities
# # variables topo nf pivot
# 
# data_wider <- data %>%  pivot_wider(names_from = c(topo, nf) , 
#                       values_from = yield) %>% 
#                       select(!(year:rep))
# 
#  # sum(!is.na(M$LO_N1))
# 
# density_fun <- function(x){
#   density(x, na.rm = T)
# }
# 
# density_list <- lapply(data_wider, density_fun) # aplicar estima densidad a cada columna


mod1c = aov(yield ~ nf * topo, data= data) # interaccion entre nf y topo
summary(mod1c)
#nf:topo       15   1993     133   0.642  0.842


#Nonparametric k-way ANOVA
# metodos robustos 
install.packages("Rfit")  
library(Rfit)  

modRAOV <- Rfit::raov(yield ~ nf*topo, data = data)
summary(modRAOV)

#ANCOVA (respuesta continua + covariables categoricas + continuas)

mod3 = lm(yield ~ nf + bv, data = data)  
summary(mod3)
#plot de diagnostico
