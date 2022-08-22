### R code from vignette source 'exB1.Rnw'

###################################################
### code chunk number 1: exB1.Rnw:20-24
###################################################
options(prompt="> ", continue="+ ", digits=5, width=70, show.signif.stars=T)
par(mfrow=c(1,1))
rm(list=ls())
setwd("~/data/edu/dgeostats/ex/")


###################################################
### code chunk number 2: exB1.Rnw:39-41 (eval = FALSE)
###################################################
chooseCRANmirror() # elegir en que servidor descargar los ficheros
install.packages("compositions") # n Boogaart and Tolosana-Delgado


###################################################
### code chunk number 3: exB1.Rnw:50-51
###################################################
require(compositions) # los mismo que library()

# DATA DE SUELOS
#The soil was sampled at three depth intervals
# (5-6 cm thick, centred on 8, 30, 65 cm depth) by augering every 10 m along
# a regular transect, resulting in 321 sites.
#variables : clay1 clay 2 clay3 silt1 silt2 sitl3

###################################################
### code chunk number 4: exB1.Rnw:72-73 (eval = FALSE)
###################################################
file.show("sandford.txt") # abre el txt


###################################################
### code chunk number 5: exB1.Rnw:97-98 (eval = FALSE)
###################################################
## ds <- read.table("sandford.txt", skip=2)


###################################################
### code chunk number 6: exB1.Rnw:101-104
###################################################

#leer el txt como dataframe

ds <- read.table("sandford.txt", skip=2) #sin las dos primeras filas
names(ds) <- c("seq","clay1","silt1","clay2","silt2","clay3","silt3") # nombres de las columnas. seq es el numero de pedon muestreado en el transecto
str(ds)


###################################################
### code chunk number 7: exB1.Rnw:121-124
###################################################

#añadir el contenido de arena para cada profundidad

ds$sand1 <- 100 - (ds$clay1 + ds$silt1)
ds$sand2 <- 100 - (ds$clay2 + ds$silt2)
ds$sand3 <- 100 - (ds$clay3 + ds$silt3)


###################################################
### code chunk number 8: exB1.Rnw:144-150
###################################################
# layer 2
plot(ds$clay2, type="l", ylim=c(0,100), xlim=c(0,360), main="Layer 2", xlab="Station on transect", ylab="weight %")
lines(ds$silt2, lty=2, col="blue")
lines(ds$sand2, lty=3, col="red")
legend(330,100, c("clay","silt","sand"), lty=1:3, col=c("black","blue","red"))
grid()


###################################################
### code chunk number 9: exB1.Rnw:183-188
###################################################

#Splitting the dataset
# we remove every third datum (i.e., the observations at sites 3, 6, 9, . . . )
# es como seleccionar aleatoriamnte 1/3 de la data para validacion

valid.ix <- seq(from=3, to=length(ds$seq), by=3)
head(valid.ix); tail(valid.ix)
ds.val <- ds[valid.ix,]  #validacion mantener los indices de valid.ix
ds.cal <- ds[-valid.ix,] # calibracion, mantener todos menos los indices de valid.iix
dim(ds.val); dim(ds.cal)


###################################################
### code chunk number 10: exB1.Rnw:203-206
###################################################
#EL MODELAMIENTO SERA PARA LA CAPA/LAYER 2 (textura a 30 cm de profunidad)

names.2 <- c("sand2","silt2","clay2")
summary(ds.cal[,names.2]) # ds.cal[,names.2] subseting variables de la layer 2
sapply(ds.cal[,names.2], sd) # desviacion estandar


###################################################
### code chunk number 11: exB1.Rnw:257-260
###################################################
# usamos la funcion acomp : for proportions and relative scale.
# la composicion textural del suelo es una proporcion y  tiene una escala realtiva (porcentual)
# Does an increase in clay content from 2% to 5% have the same significance 
# as an increase from 32% to 35% and from from 92% to 95%?

comp.2 <- acomp(ds.cal[,names.2], total=100)
# acomp convierte a clase composicional
# las fracciones de arcilla limo y arena deben sumar a 100. total=100
class(comp.2)
#str(comp.2) error


###################################################
### code chunk number 12: exB1.Rnw:290-292
###################################################

# convertir a variables composicionales
#transformacion log-ratio aditiva

str(alr2 <- alr(comp.2)) # por default el denomidor sera clay2
summary(alr2)

#alr mapea del espaio simplex al espacio real sin limites (0,100)
# permite analisis multivariable normal


###################################################
### code chunk number 13: exB1.Rnw:308-312
###################################################

#Check that the inverse transformation recovers the original data
#alrInv invierte la transformacion log-ratio aditiva

str(tmp <- alrInv(alr2, orig=comp.2))
sum((comp.2[,"sand2"]-tmp[,"sand2"]*100)
    + (comp.2[,"silt2"]-tmp[,"silt2"]*100)
    + (comp.2[,"clay2"]-tmp[,"clay2"]*100))
#data original - inv(data transformada) = 0

###################################################
### code chunk number 14: exB1.Rnw:331-332
###################################################
names(summary(comp.2))


###################################################
### code chunk number 15: exB1.Rnw:337-340
###################################################
summary(comp.2)$mean #la media geometrica es adecuada para describir rates & ratios
colMeans(ds.cal[,names.2])/100
meanCol(comp.2)/100


###################################################
### code chunk number 16: exB1.Rnw:352-354
###################################################

#To summarize the variances, we use a so-called variation matrix
variation(comp.2)
summary(comp.2)$var


###################################################
### code chunk number 17: exB1.Rnw:381-388
###################################################
#Graphical

# layer 2
xpts <- setdiff(1:length(ds$seq),seq(3,length(ds$seq),by=3))
plot(alr2[,1] ~ xpts, type="l", xlim=c(0,360), main="Layer 2 additive log-ratios", xlab="Station on transect", ylab="ratio of weight %")
lines(alr2[,2] ~ xpts, lty=2, col="blue")
abline(h=0, lty=2)
legend(300,3.5, c("sand/clay","silt/clay"), lty=1:2, col=c("black","blue"))
grid()


###################################################
### code chunk number 18: exB1.Rnw:401-402
###################################################
boxplot(comp.2, log=F)


###################################################
### code chunk number 19: exB1.Rnw:405-406
###################################################
boxplot(comp.2, log=T) # default


###################################################
### code chunk number 20: exB1.Rnw:426-432
###################################################

#Display a ternary diagram of the layer 2 particle-size distribution.

plot(comp.2, axes=T)
plot(mean(comp.2), add=T, col="red", pch=20, cex=2)             #media
straight(rcomp(c(0,0,0)), rcomp(mean(comp.2)))
ellipses(mean(comp.2), var(comp.2), col="red", r=2)         # varianza 
straight(mean(comp.2), princomp(comp.2)$Loadings[1,])       # carga PC1
straight(mean(comp.2), princomp(comp.2)$Loadings[2,], lty=2) ## carga PC2

#interpretacion
#

###################################################
### code chunk number 21: exB1.Rnw:477-482
###################################################

#Kriging compositional variables

require(sp)
require(gstat)

ds.cal.sp <- cbind(ds.cal, y=0)
coordinates(ds.cal.sp) <- ~ seq + y # las coordenadas son relativas, las observaciones no tienen ubicacion absoluta
# siempre y cuando las distancias sean coherentes los
# resultados seran los mismos cada unidad representa 10 metros
str(ds.cal.sp)


###################################################
### code chunk number 22: exB1.Rnw:499-503
###################################################

#Ordinary kriging of particle-size fractions
#estimar los variogramas separadamente para cada variable original
#We are only interested in the short-range structure
#So we limit the range to 30 units (each representing 10 m).

v.sand <- variogram(sand2 ~ 1, loc=ds.cal.sp, cutoff=36) # formula in case of absence of regressors, use e.g. z~1
v.silt <- variogram(silt2 ~ 1, loc=ds.cal.sp, cutoff=36)
v.clay <- variogram(clay2 ~ 1, loc=ds.cal.sp, cutoff=36)
sv.max <- max(v.sand$gamma, v.silt$gamma, v.clay$gamma) # semivariansas maximas de cada variograma eperimental


###################################################
### code chunk number 23: exB1.Rnw:508-514
###################################################

#plots de los semivariogramas muestrales

p1 <- plot(v.sand, plot.numbers=T, main="Sand %", ylim=c(0, sv.max))
p2 <- plot(v.silt, plot.numbers=T, main="Silt %", ylim=c(0, sv.max))
p3 <- plot(v.clay, plot.numbers=T, main="Clay %", ylim=c(0, sv.max))
print(p1, split=c(1,1,3,1), more=T)
print(p2, split=c(2,1,3,1), more=T)
print(p3, split=c(3,1,3,1), more=F)

#Describe the spatial structure (range, sill, nugget, form) of the three
#separates. Are these similar?

#The sills correspond to the total variability


###################################################
### code chunk number 24: exB1.Rnw:531-534
###################################################

#modelar el variograma para cada variable
# primero estimamos los parametros al ojo
#fit.variogram function, using the default weighted least squares
# automap da otras opciones de ajuste automatico

(vm.sand <- fit.variogram(v.sand, vgm(psill = 1400, model = "Sph", range = 100, nugget=100)))
(vm.silt <- fit.variogram(v.silt, vgm(psill = 300, model ="Sph", range = 50, nugget = 100)))
(vm.clay <- fit.variogram(v.clay, vgm(psill = 425, model ="Sph", range = 40, nugget = 100)))

# vgm(psill = NA, model, range = NA, nugget, add.to, anis, kappa = 0.5, ..., covtable,
#     Err = 0)
# vgm() describe los modelos disponibles
###################################################
### code chunk number 25: exB1.Rnw:544-553
###################################################

#Plot the empirical variograms and the fitted models on one graph

sv.max <- max(v.sand$gamma, v.silt$gamma, v.clay$gamma)*1.05 # valor maximo de la escala
plot(v.sand$gamma ~ v.sand$dist, xlim=c(0,36), ylim=c(0,sv.max), ylab="Semivariance", xlab="Separation", main="Fitted variograms, particle-size fractions", col="red", pch=20)
lines(variogramLine(vm.sand, maxdist=36), lty=3, col="red")
points(v.silt$gamma ~ v.silt$dist, ylim=c(0,sv.max), col="blue", pch=20)
lines(variogramLine(vm.silt, maxdist=36), lty=2, col="blue")
points(v.clay$gamma ~ v.clay$dist, ylim=c(0,sv.max), col="black", pch=20)
lines(variogramLine(vm.clay, maxdist=36), lty=1, col="black")
legend(2,sv.max*.9, c("sand","silt","clay"), lty=3:1, col=c("red","blue","black"))
grid()


###################################################
### code chunk number 26: exB1.Rnw:566-571
###################################################

#Predict at the evaluation locations
#usamos los puntos de validacion

ds.val.sp <- cbind(ds.val, y=0)
coordinates(ds.val.sp) <- ~ seq + y
#krigeado
#or ordinary and simple kriging use the formula z~1
#newdata = el grid de prediccion
#	for local kriging: only observations within a distance of maxdist from the prediction location are used for prediction
# utilizamos el segundo modelo y el rango para indicar la distancia maxima de observaiones a usar 
k.sand <- krige(sand2 ~ 1, locations= ds.cal.sp, newdata=ds.val.sp, model=vm.sand, maxdist=vm.sand[2,"range"])
k.silt <- krige(silt2 ~ 1, locations= ds.cal.sp, newdata=ds.val.sp, model=vm.silt, maxdist=vm.silt[2,"range"])
k.clay <- krige(clay2 ~ 1, locations= ds.cal.sp, newdata=ds.val.sp, model=vm.clay, maxdist=vm.clay[2,"range"])


###################################################
### code chunk number 27: exB1.Rnw:579-587
###################################################

#comprobar que los tres componentes sumen a 100%

summary(diff <- 100 -
          (k.sand$var1.pred + k.silt$var1.pred + k.clay$var1.pred))

plot(diff ~ coordinates(ds.val.sp)[,1], 
     ylab="100 - (sand + silt + clay)",
     xlab="station", type="h",
     main="Difference between 100% and the sum of components")
abline(h=0)
grid()


###################################################
### code chunk number 28: exB1.Rnw:606-612
###################################################

#Evaluation OK metodology
# valores observados vs predecidos
# el sesgo (bias) es la media de los errores
# el rmse es la raiz cuadrada de la media de los errores al cuadrado

summary(diff.k.sand <- (k.sand$var1.pred - ds.val.sp$sand2)) # arenna
(rmse.k.sand <- sqrt(sum(diff.k.sand^2)/length(diff.k.sand))) # rmse arena
summary(diff.k.silt <- (k.silt$var1.pred - ds.val.sp$silt2))  # limo
(rmse.k.silt <- sqrt(sum(diff.k.silt^2)/length(diff.k.silt)))
summary(diff.k.clay <- (k.clay$var1.pred - ds.val.sp$clay2))  # arcilla
(rmse.k.clay <- sqrt(sum(diff.k.clay^2)/length(diff.k.clay)))


###################################################
### code chunk number 29: exB1.Rnw:626-640
###################################################
#plots de prediccion
par(mfrow=c(1,3))
plot(ds.val[,"sand2"] ~ k.sand$var1.pred,
     ylab="Actual", xlab="Predicted", main="Sand",
     xlim=c(0,100), ylim=c(0,100))
abline(0,1); grid()
plot(ds.val[,"silt2"] ~ k.silt$var1.pred,
     ylab="Actual", xlab="Predicted", main="Silt",
     xlim=c(0,100), ylim=c(0,100))
abline(0,1); grid()
plot(ds.val[,"clay2"] ~ k.clay$var1.pred,
     ylab="Actual", xlab="Predicted", main="Clay",
     xlim=c(0,100), ylim=c(0,100))
abline(0,1); grid()
par(mfrow=c(1,1))


###################################################
### code chunk number 30: exB1.Rnw:649-653
###################################################

#prediciones vs observaciones en el transecto

plot(ds.val$sand2 ~ ds.val$seq, type="l", ylim=c(0,100), xlim=c(0,330), main="Actual vs. predicted sand proportion (OK)", xlab="Station on transect", ylab="weight %", cex=0.6, pch=20)
lines(k.sand$var1.pred  ~ coordinates(ds.val.sp)[,1], col="red", type="l", cex=0.6)
legend(250,90, c("actual","predicted"), lty=1, col=c("black","red"))
grid()

# kriging en general suaviza la señal

###################################################
### code chunk number 31: exB1.Rnw:673-678
###################################################

# Recreating a composition
# las propociones deberian suman a 100
# se reaujustan los valores 

tmp <- data.frame(sand2=k.sand$var1.pred, silt2=k.silt$var1.pred, clay2=k.clay$var1.pred)
comp.2.ssc.ok <- acomp(tmp, total=100)
head(tmp[,"sand2"])
head(comp.2.ssc.ok[,"sand2"])
rm(tmp) # remover tm porque era una variabe temporal


###################################################
### code chunk number 32: exB1.Rnw:689-695
###################################################

# estadisticos para la data composicional

summary(diff.k.sand <- (comp.2.ssc.ok[,"sand2"]- ds.val.sp$sand2))
(rmse.k.sand <- sqrt(sum(diff.k.sand^2)/length(diff.k.sand)))
summary(diff.k.silt <- (comp.2.ssc.ok[,"silt2"] - ds.val.sp$silt2))
(rmse.k.silt <- sqrt(sum(diff.k.silt^2)/length(diff.k.silt)))
summary(diff.k.clay <- (comp.2.ssc.ok[,"clay2"] - ds.val.sp$clay2))
(rmse.k.clay <- sqrt(sum(diff.k.clay^2)/length(diff.k.clay)))

#los estadisticos son algo peores que las variables independientes

###################################################
### code chunk number 33: exB1.Rnw:719-721
###################################################

#Co-krige independent variables 
# It is not possible to model all three together, since they are linearly dependent
# modelamos sand y silt y predecimos la clay por resta

(g <- gstat(NULL, id = "sand2", form = sand2 ~ 1, data=ds.cal.sp)) # modelo NULL
(g <- gstat(g, id = "silt2", form = silt2 ~ 1, data=ds.cal.sp))  # actualiza modelo


###################################################
### code chunk number 34: exB1.Rnw:731-735
###################################################

# variogramas

vm.silt[2,"range"] # rango del variograma de silt

# cross-variogram

v.cross <- variogram(g, cutoff=vm.silt[2,"range"], width=3) #cutoff , max distancia incluida en la estiamcion
str(v.cross)
print(plot(v.cross, pl=T))


###################################################
### code chunk number 35: exB1.Rnw:743-756
###################################################

#variogramas directos y cross variogramas en el mismo plot

sv.max <- max(v.cross$gamma)
sv.min <- min(v.cross$gamma)
plot(v.cross$gamma ~ v.cross$dist, ylim=c(sv.min,sv.max),
     ylab="Semivariance", xlab="Separation",
     main="Empirical direct and cross variograms, particle-size fractions",
     type="n")
for (i in 1:3) {
  tmp <- subset(v.cross, as.numeric(v.cross$id) == i)
  lines(tmp$gamma ~ tmp$dist, col=i, type="b", lty=i)
}
grid()
abline(h=0, lty=2)
legend(2,sv.max*.99, levels(v.cross$id), lty=1:3, col=1:3)


###################################################
### code chunk number 36: exB1.Rnw:778-780
###################################################

#linear model of co-regionalization  LMC
# The LMC requires a single range and structure
# primero elegimos un variograma
# en este caso el que tenga el rango mas corto que el del vm.silt

vm.silt
g <- gstat(g, id = "silt2", model = vm.silt, fill.all=T) #crea gstat objects


###################################################
### code chunk number 37: exB1.Rnw:790-792
###################################################

# ajustar silmultaneamente los variogramas

(g <- fit.lmc(v.cross, g)) #fit.lmc(v, g) v = variograma muestral multivariado
print(plot(variogram(g, cutoff=30), model=g$model))


###################################################
### code chunk number 38: exB1.Rnw:796-808
###################################################
plot(v.cross$gamma ~ v.cross$dist, xlim=c(0,30), ylim=c(sv.min,sv.max),
     ylab="Semivariance", xlab="Separation",
     main="Modelled direct and cross variograms, particle-size fractions",
     type="p", col=as.numeric(v.cross$id))
g$model
for (i in 1:3) {
  tmp <- c("sand2.silt2","silt2","sand2")[i]
  lines(variogramLine(g$model[[tmp]], maxdist=30), col=i, lty=i)
}
grid()
abline(h=0, lty=2)
legend(1,sv.max*.95, levels(v.cross$id), lty=1:3, col=1:3)


###################################################
### code chunk number 39: exB1.Rnw:822-824
###################################################

#Interpolate by co-kriging.

k.c <- predict.gstat(g, ds.val.sp)
summary(k.c)


###################################################
### code chunk number 40: exB1.Rnw:838-840
###################################################
k.c$clay2.pred <- 100 - (k.c$sand2.pred + k.c$silt2.pred)
summary(k.c$sand2.pred)


###################################################
### code chunk number 41: exB1.Rnw:856-865
###################################################
summary(diff.kc.sand <- (k.c$sand2.pred - ds.val.sp$sand2))
# (bias.kc.sand <- sum(diff.kc.sand)/length(diff.kc.sand))
(rmse.kc.sand <- sqrt(sum(diff.kc.sand^2)/length(diff.kc.sand)))
summary(diff.kc.silt <- (k.c$silt2.pred - ds.val.sp$silt2))
# (bias.kc.silt <- sum(diff.kc.silt)/length(diff.kc.silt))
(rmse.kc.silt <- sqrt(sum(diff.kc.silt^2)/length(diff.kc.silt)))
summary(diff.kc.clay <- (k.c$clay2.pred - ds.val.sp$clay2))
# (bias.kc.clay <- sum(diff.kc.clay)/length(diff.kc.clay))
(rmse.kc.clay <- sqrt(sum(diff.kc.clay^2)/length(diff.kc.clay)))


###################################################
### code chunk number 42: exB1.Rnw:875-881
###################################################
mean(diff.k.sand) - mean(diff.kc.sand)
mean(diff.k.silt) - mean(diff.kc.silt)
mean(diff.k.clay) - mean(diff.kc.clay)
rmse.k.sand - rmse.kc.sand
rmse.k.silt - rmse.kc.silt
rmse.k.clay - rmse.kc.clay


###################################################
### code chunk number 43: exB1.Rnw:889-903
###################################################
par(mfrow=c(1,3))
plot(ds.val[,"sand2"] ~ k.c$sand2.pred,
     ylab="Actual", xlab="Predicted", main="Sand",
     xlim=c(0,100), ylim=c(0,100))
abline(0,1); grid()
plot(ds.val[,"silt2"] ~ k.c$silt2.pred,
     ylab="Actual", xlab="Predicted", main="Silt",
     xlim=c(0,100), ylim=c(0,100))
abline(0,1); grid()
plot(ds.val[,"clay2"] ~ k.c$clay2.pred,
     ylab="Actual", xlab="Predicted", main="Clay",
     xlim=c(0,100), ylim=c(0,100))
abline(0,1); grid()
par(mfrow=c(1,1))


###################################################
### code chunk number 44: exB1.Rnw:912-916
###################################################
plot(ds.val$sand2 ~ ds.val$seq, type="l", ylim=c(0,100), xlim=c(0,330), main="Actual vs. predicted sand proportion (CK)", xlab="Station on transect", ylab="sand, weight %", cex=0.6)
lines(k.c$sand2.pred  ~ coordinates(ds.val.sp)[,1], col="red", type="l", cex=0.6)
grid()
legend(240,98, c("actual", "predicted CK"), lty=1, col=c("black","red"))


###################################################
### code chunk number 45: exB1.Rnw:933-934
###################################################
summary(alr2)


###################################################
### code chunk number 46: exB1.Rnw:943-945
###################################################
ds.cal.sp$alr.1 <- alr2[,1]
ds.cal.sp$alr.2 <- alr2[,2]


###################################################
### code chunk number 47: exB1.Rnw:953-955
###################################################
v.alr.1 <- variogram(alr.1 ~ 1, loc=ds.cal.sp, cutoff=30)
v.alr.2 <- variogram(alr.2 ~ 1, loc=ds.cal.sp, cutoff=30)


###################################################
### code chunk number 48: exB1.Rnw:959-968
###################################################
sv.max <- max(v.alr.1$gamma, v.alr.2$gamma)*1.05
plot(v.alr.1$gamma ~ v.alr.1$dist, ylim=c(0,sv.max),
     ylab="Semivariance", xlab="Separation",
     main="Empirical variograms, ALR variables",
     type="b", col="red")
lines(v.alr.2$gamma ~ v.alr.2$dist, type="b", lty=2, col="blue")
grid()
legend(2,sv.max*.95, c("ln(sand/clay)","ln(silt/clay)"),
       lty=2:1, col=c("red","blue"))


###################################################
### code chunk number 49: exB1.Rnw:979-981
###################################################
(vm.alr.1 <- fit.variogram(v.alr.1, vgm(2, "Sph", 25, 0.4)))
(vm.alr.2 <- fit.variogram(v.alr.2, vgm(1, "Sph", 25, 0.4)))


###################################################
### code chunk number 50: exB1.Rnw:984-990
###################################################
plot(v.alr.1$gamma ~ v.alr.1$dist, ylim=c(0,sv.max), ylab="Semivariance", xlab="Separation", xlim=c(0,30), main="Fitted variograms, ALR-transformed components", col="red", pch=20)
lines(variogramLine(vm.alr.1, maxdist=100), lty=1, col="red")
points(v.alr.2$gamma ~ v.alr.2$dist, col="blue", pch=20)
lines(variogramLine(vm.alr.2, maxdist=100), lty=2, col="blue")
legend(2,sv.max*.95, c("ln(clay/sand)","ln(silt/sand)"), lty=2:1, col=c("blue","red"))
grid()


###################################################
### code chunk number 51: exB1.Rnw:997-999
###################################################
k.alr.1 <- krige(alr.1 ~ 1, loc=ds.cal.sp, newdata=ds.val.sp, model=vm.alr.2)
k.alr.2 <- krige(alr.2 ~ 1, loc=ds.cal.sp, newdata=ds.val.sp, model=vm.alr.2)


###################################################
### code chunk number 52: exB1.Rnw:1015-1020
###################################################
k.alr.comp <- data.frame(alr.1 = k.alr.1$var1.pred, 
                         alr.2 = k.alr.2$var1.pred)
comp.2.ok <- acomp(as.data.frame(alrInv(k.alr.comp,
                                        orig=comp.2)),total=100)
str(comp.2.ok)


###################################################
### code chunk number 53: exB1.Rnw:1025-1026
###################################################
summary(apply(comp.2.ok, 1, sum))


###################################################
### code chunk number 54: exB1.Rnw:1034-1043
###################################################
summary(diff.ka.sand <- (comp.2.ok[,"sand2"] - ds.val.sp$sand2))
# (bias.ka.sand <- sum(diff.ka.sand)/length(diff.ka.sand))
(rmse.ka.sand <- sqrt(sum(diff.ka.sand^2)/length(diff.ka.sand)))
summary(diff.ka.silt <- (comp.2.ok[,"silt2"] - ds.val.sp$silt2))
# (bias.ka.silt <- sum(diff.ka.silt)/length(diff.ka.silt))
(rmse.ka.silt <- sqrt(sum(diff.ka.silt^2)/length(diff.ka.silt)))
summary(diff.ka.clay <- (comp.2.ok[,"clay2"] - ds.val.sp$clay2))
# (bias.ka.clay <- sum(diff.ka.clay)/length(diff.ka.clay))
(rmse.ka.clay <- sqrt(sum(diff.ka.clay^2)/length(diff.ka.clay)))


###################################################
### code chunk number 55: exB1.Rnw:1052-1058
###################################################
mean(diff.k.sand) - mean(diff.ka.sand)
mean(diff.k.silt) - mean(diff.ka.silt)
mean(diff.k.clay) - mean(diff.ka.clay)
rmse.k.sand - rmse.ka.sand
rmse.k.silt - rmse.ka.silt
rmse.k.clay - rmse.ka.clay


###################################################
### code chunk number 56: exB1.Rnw:1066-1080
###################################################
par(mfrow=c(1,3))
plot(ds.val[,"sand2"] ~ comp.2.ok[,"sand2"],
     ylab="Actual", xlab="Predicted", main="Sand",
     xlim=c(0,100), ylim=c(0,100))
abline(0,1); grid()
plot(ds.val[,"silt2"] ~ comp.2.ok[,"silt2"],
     ylab="Actual", xlab="Predicted", main="Silt",
     xlim=c(0,100), ylim=c(0,100))
abline(0,1); grid()
plot(ds.val[,"clay2"] ~ comp.2.ok[,"clay2"],
     ylab="Actual", xlab="Predicted", main="Clay",
     xlim=c(0,100), ylim=c(0,100))
abline(0,1); grid()
par(mfrow=c(1,1))


###################################################
### code chunk number 57: exB1.Rnw:1090-1094
###################################################
plot(ds.val$sand2 ~ ds.val$seq, type="l", ylim=c(0,100), xlim=c(0,330), main="Actual vs. predicted sand proportion (ALR OK)", xlab="Station on transect", ylab="weight %", cex=0.6)
lines(comp.2.ok[,"sand2"]  ~ coordinates(ds.val.sp)[,1], col="red", type="l", cex=0.6)
grid()
legend(200,98, c("actual","predicted ALR OK"), lty=1, col=c("black","red"))


###################################################
### code chunk number 58: exB1.Rnw:1114-1116
###################################################
(gc <- gstat(NULL, id = "sand.clay", form = alr.1 ~ 1, data=ds.cal.sp))
(gc <- gstat(gc, id = "silt.clay", form = alr.2 ~ 1, data=ds.cal.sp))


###################################################
### code chunk number 59: exB1.Rnw:1125-1126
###################################################
v.cross <- variogram(gc, cutoff=30, width=3)


###################################################
### code chunk number 60: exB1.Rnw:1135-1148
###################################################
sv.max <- max(v.cross$gamma)
sv.min <- min(v.cross$gamma)
plot(v.cross$gamma ~ v.cross$dist, ylim=c(sv.min,sv.max),
     ylab="Semivariance", xlab="Separation",
     main="Empirical direct and cross variograms, ALR-transformed fractions",
     type="n")
for (i in 1:3) {
  tmp <- subset(v.cross, as.numeric(v.cross$id) == i)
  lines(tmp$gamma ~ tmp$dist, col=i, type="b", lty=i)
}
grid()
abline(h=0, lty=2)
legend(2,sv.max*.95, levels(v.cross$id), lty=1:3, col=1:3)


###################################################
### code chunk number 61: exB1.Rnw:1161-1162
###################################################
gc <- gstat(gc, id = "silt.clay", model = vm.alr.2, fill.all=T)


###################################################
### code chunk number 62: exB1.Rnw:1169-1170
###################################################
(gc <- fit.lmc(v.cross, gc))


###################################################
### code chunk number 63: exB1.Rnw:1174-1187
###################################################
plot(v.cross$gamma ~ v.cross$dist, ylim=c(sv.min,sv.max),
     xlim=c(0,max(v.cross$dist)),
     ylab="Semivariance", xlab="Separation",
     main="Modelled direct and cross variograms, particle-size fractions",
     type="p",
     col=as.numeric(v.cross$id), pch=20)
for (i in 1:3) {
  tmp <- c("sand.clay.silt.clay","silt.clay","sand.clay")[i]
  lines(variogramLine(gc$model[[tmp]], maxdist=30), col=i, lty=i)
}
grid()
abline(h=0, lty=2)
legend(1,sv.max*.95, levels(v.cross$id), lty=1:3, col=1:3)


###################################################
### code chunk number 64: exB1.Rnw:1201-1203
###################################################
k.c.c <- predict.gstat(gc, ds.val.sp)
summary(k.c.c)


###################################################
### code chunk number 65: exB1.Rnw:1213-1218
###################################################
kc.alr.comp <- data.frame(alr.1 = k.c.c$sand.clay.pred,
                          alr.2 = k.c.c$silt.clay.pred)
comp.2.ck <- acomp(as.data.frame(alrInv(kc.alr.comp,
                                        orig=comp.2)), total=100)
str(comp.2.ck)


###################################################
### code chunk number 66: exB1.Rnw:1229-1235
###################################################
summary(diff.kca.sand <- (comp.2.ck[,"sand2"] - ds.val.sp$sand2))
(rmse.kca.sand <- sqrt(sum(diff.kca.sand^2)/length(diff.kca.sand)))
summary(diff.kca.silt <- (comp.2.ck[,"silt2"] - ds.val.sp$silt2))
(rmse.kca.silt <- sqrt(sum(diff.kca.silt^2)/length(diff.kca.silt)))
summary(diff.kca.clay <- (comp.2.ck[,"clay2"] - ds.val.sp$clay2))
(rmse.kca.clay <- sqrt(sum(diff.kca.clay^2)/length(diff.kca.clay)))


###################################################
### code chunk number 67: exB1.Rnw:1244-1258
###################################################
par(mfrow=c(1,3))
plot(ds.val[,"sand2"] ~ comp.2.ck[,"sand2"],
     ylab="Actual", xlab="Predicted", main="Sand",
     xlim=c(0,100), ylim=c(0,100))
abline(0,1); grid()
plot(ds.val[,"silt2"] ~ comp.2.ck[,"silt2"],
     ylab="Actual", xlab="Predicted", main="Silt",
     xlim=c(0,100), ylim=c(0,100))
abline(0,1); grid()
plot(ds.val[,"clay2"] ~ comp.2.ck[,"clay2"],
     ylab="Actual", xlab="Predicted", main="Clay",
     xlim=c(0,100), ylim=c(0,100))
abline(0,1); grid()
par(mfrow=c(1,1))


###################################################
### code chunk number 68: exB1.Rnw:1269-1273
###################################################
plot(ds.val$sand2 ~ ds.val$seq, type="l", ylim=c(0,100), xlim=c(0,330), main="Actual vs. predicted sand proportion (ALR CK)", xlab="Station on transect", ylab="weight %", cex=0.6)
lines(comp.2.ck[,"sand2"]  ~ coordinates(ds.val.sp)[,1], col="red", type="l", cex=0.6)
grid()
legend(200,98, c("actual","predicted ALR CK"), lty=1, col=c("black","red"))


###################################################
### code chunk number 69: exB1.Rnw:1304-1313
###################################################
compare <- data.frame(me.sand=0, me.silt=0, me.clay=0, rmse.sand=0, rmse.silt=0, rmse.clay=0)
compare[1,] <- c(mean(diff.k.sand), mean(diff.k.silt), mean(diff.k.clay), rmse.k.sand, rmse.k.silt, rmse.k.clay)
compare[2,] <- c(mean(diff.kc.sand), mean(diff.kc.silt), mean(diff.kc.clay), rmse.kc.sand, rmse.kc.silt, rmse.kc.clay)
compare[3,] <- c(mean(diff.ka.sand), mean(diff.ka.silt), mean(diff.ka.clay), rmse.ka.sand, rmse.ka.silt, rmse.ka.clay)
compare[4,] <- c(mean(diff.kca.sand), mean(diff.kca.silt), mean(diff.kca.clay), rmse.kca.sand, rmse.kca.silt, rmse.kca.clay)
compare <- cbind(compare, mean.me=round(as.vector(apply(compare[,1:3],1,sum)/3),5))
compare <- cbind(compare, mean.rmse=as.vector(apply(compare[,4:6],1,sum)/3))
rownames(compare) <- c("OK","CK","ALR-OK","ALR-CK")
print(compare)


###################################################
### code chunk number 70: exB1.Rnw:1329-1351
###################################################
plot(ds$sand2 ~ ds$seq, type="l", ylim=c(0,100),
     xlim=c(0,330),
     main="Actual vs. predicted sand proportion",
     xlab="Station on transect",
     ylab="weight %", cex=0.6)
lines(comp.2.ssc.ok[,"sand2"]   ~ 
        coordinates(ds.val.sp)[,1], col="red",
      type="l", cex=0.6)
lines(k.c$sand2.pred  ~ 
        coordinates(ds.val.sp)[,1], col="blue",
      type="l", cex=0.6)
lines(comp.2.ok[,"sand2"]  ~ 
        coordinates(ds.val.sp)[,1], col="darkgreen",
      type="l", cex=0.6)
lines(comp.2.ck[,"sand2"]  ~ 
        coordinates(ds.val.sp)[,1], col="brown",
      type="l", cex=0.6)
grid()
legend(200,100,
       c("actual","OK","CK","ALR-OK","ALR-CK"),
       lty=1,
       col=c("black","red","blue","darkgreen","brown"))


###################################################
### code chunk number 71: exB1.Rnw:1363-1370
###################################################
plot(ds$sand2 ~ ds$seq, type="l", ylim=c(0,100), xlim=c(200,260), main="Actual vs. predicted sand proportion", xlab="Station on transect", ylab="weight %", cex=0.6)
lines(comp.2.ssc.ok[,"sand2"]  ~ coordinates(ds.val.sp)[,1], col="red", type="l", cex=0.6)
lines(k.c$sand2.pred  ~ coordinates(ds.val.sp)[,1], col="blue", type="l", cex=0.6)
lines(comp.2.ok[,"sand2"]  ~ coordinates(ds.val.sp)[,1], col="darkgreen", type="l", cex=0.6)
lines(comp.2.ck[,"sand2"]  ~ coordinates(ds.val.sp)[,1], col="brown", type="l", cex=0.6)
grid()
legend(200,100, c("actual","OK","CK","ALR-OK","ALR-CK"), lty=1, col=c("black","red","blue","darkgreen","brown"))


###################################################
### code chunk number 72: exB1.Rnw:1392-1397
###################################################
class(comp.2.ok)
class(comp.2.ck)
class(comp.2.ssc.ok)
comp.2.val <- acomp(ds.val[,names.2], total=100)
comp.2.ssc.ck <- acomp(data.frame(sand=k.c$sand2.pred, silt=k.c$silt2.pred, clay=k.c$clay2.pred), total=100)


###################################################
### code chunk number 73: exB1.Rnw:1400-1408
###################################################
opar <- par(no.readonly = T)
par(pch=20, cex=.8)
plot(comp.2.val, axes=T, col="black", pch=1)
plot(acomp(comp.2.ssc.ok, total=100), col="blue", add=T)
plot(acomp(comp.2.ssc.ck, total=100), col="red", add=T)
plot(acomp(comp.2.ok, total=100), col="darkgreen", add=T)
plot(acomp(comp.2.ck, total=100), col="brown", add=T)
par(opar)


###################################################
### code chunk number 74: exB1.Rnw:1421-1449
###################################################
opar <- par(no.readonly = T)
par(mfrow=c(1,3))
par(pch=20, cex=.8)
plot(ds.val[,"sand2"] ~ k.sand$var1.pred, col="red",
     ylab="Actual", xlab="Predicted", main="Sand",
     xlim=c(0,100), ylim=c(0,100))
points(ds.val[,"sand2"] ~ k.c$sand2.pred, col="blue")
points(ds.val[,"sand2"] ~ comp.2.ok[,"sand2"], col="darkgreen")
points(ds.val[,"sand2"] ~ comp.2.ck[,"sand2"], col="brown")
#
abline(0,1); grid()
plot(ds.val[,"silt2"] ~ k.silt$var1.pred, col="red",
     ylab="Actual", xlab="Predicted", main="Silt",
     xlim=c(0,100), ylim=c(0,100))
points(ds.val[,"silt2"] ~ k.c$silt2.pred, col="blue")
points(ds.val[,"silt2"] ~ comp.2.ok[,"silt2"], col="darkgreen")
points(ds.val[,"silt2"] ~ comp.2.ck[,"silt2"], col="brown")
abline(0,1); grid()
legend(0,100, c("OK","CK","ALR-OK","ALR-CK"), pch=20,
       col=c("red","blue","darkgreen","brown"))
plot(ds.val[,"clay2"] ~ k.clay$var1.pred, col="red",
     ylab="Actual", xlab="Predicted", main="Clay",
     xlim=c(0,100), ylim=c(0,100))
points(ds.val[,"clay2"] ~ k.c$clay2.pred, col="blue")
points(ds.val[,"clay2"] ~ comp.2.ok[,"clay2"], col="darkgreen")
points(ds.val[,"clay2"] ~ comp.2.ck[,"clay2"], col="brown")
abline(0,1); grid()
par(opar)


###################################################
### code chunk number 75: exB1.Rnw:1499-1502
###################################################
c.k <- clr(comp.2.ok)
c.o <- clr(comp.2.val)
summary(c.k)-summary(c.o)


###################################################
### code chunk number 76: exB1.Rnw:1511-1513
###################################################
dk <- dist(c.k)
do <- dist(c.o)


###################################################
### code chunk number 77: exB1.Rnw:1520-1521
###################################################
sqrt(sum((do - dk)^2)/sum(do^2))


###################################################
### code chunk number 78: exB1.Rnw:1536-1541
###################################################
stress <- function(v1, v2)
{
  d1 <- dist(clr(v1)); d2 <- dist(clr(v2))
  return(sqrt(sum((d2 - d1)^2)/sum(d1^2)))
}


###################################################
### code chunk number 79: exB1.Rnw:1548-1549
###################################################
print(stress(ds.val[,names.2], comp.2.ok))


###################################################
### code chunk number 80: exB1.Rnw:1557-1561
###################################################
stress(comp.2.val, comp.2.ssc.ok)
stress(comp.2.val, comp.2.ssc.ck)
stress(comp.2.val, comp.2.ok)
stress(comp.2.val, comp.2.ck)


###################################################
### code chunk number 81: exB1.Rnw:1568-1569
###################################################
save.image(file="exB1.RData")


