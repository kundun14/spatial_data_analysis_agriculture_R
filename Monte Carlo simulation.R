#experimento moneda estacionario
set.seed(123)
n.tosses <-30 # 30 lanzamientos
head <- numeric(n.tosses)
for (i in 1:n.tosses) {
  head[i] <- rbinom(1,1,0.5) # probabilidad 0.5 cara o sello
}

head
#experimento :  cada resltado depende del anterior
## aun es estacionario
set.seed(123)
n.tosses <-30 # 30 lanzamientos
head <- numeric(n.tosses)
p <- 0.5
for (i in 1:n.tosses) {
  head[i] <- rbinom(1,1,p)
  p <- ifelse(head[i] > 0, 0.8, 0.2) # si head(i) es mayor a cero (salio cara) entonces p tomara el valor de 0.8 sino sera 0.2
}
head

#experimento la probabilidad de sacar caras aumuenta cada vez q se lanza la moneda
## no estacionario

set.seed(123)
n.tosses <- 30
head <- numeric(n.tosses)
p <- 0.3
for (i in 1:n.tosses) {
  head[i] <- rbinom(1,1,p)
  p <- p + 0.02 
  }
head

#experimento la probabilidad de sacar caras aumuenta cada vez q se lanza la moneda
## no estacionario
### con 50 obsevaciones

set.seed(123)
n.tosses <- 50
head <- numeric(n.tosses)
p <- 0.3
for (i in 1:n.tosses) {
  head[i] <- rbinom(1,1,p)
  p <- p + 0.01 
}
head

#simulacion de monte carlo

coin.toss <- function(n.tosses, p) {
  n.head <- rbinom(1, n.tosses, p)
}

set.seed(123)

n.tosses <- 20
p <- 0.5
n.reps <- 1000 

U <- replicate(n.reps, coin.toss(n.tosses, p))
head(U, 20) # cada elemnto de lvector es el numero de caras en 20 lanzamientos
mean(U)
var(U)
hist(U)


#prueba t

set.seed(123)
y <- rnorm(10)
Y.ttest <- t.test(y, alternative = "two.sided") #default value ?? of the function ttest() is 0.05
Y.ttest

#test de permutacion 

set.seed(123)
Y1 <- rnorm(5)
Y2 <- rnorm(5)
d <- mean(Y1)-mean(Y2)
d

#h0 las dos muestras independientes probienen de la misma distrubicion
##idea basica
Y = c(Y1,Y2)
Ysamp <- sample(Y, length(Y))
Yprime1 <- Ysamp[1:5]
Yprime2 <- Ysamp[6:10]

#implementacion en la funcion perm.diff

perm.diff <- function(Y1, Y2){
  Y <- c(Y1, Y2)
  Ysamp <- sample(Y, length(Y))
  Yprime1 <- Ysamp[1:5]
  Yprime2 <- Ysamp[6:10]
  dprime <- mean(Yprime1)- mean(Yprime2)
} 

# implementamos perm.diff mediante montecarlo

set.seed(123)
n.sim <- 9
U <- replicate(9, perm.diff(Y1, Y2))
sort(c(d,U))

# estiamcion del valor p 

set.seed(123)
U <- replicate(9999, perm.diff(Y1, Y2))
U <- c(U, d) # la observacion original hacen 10000 permutaciones
U.low <- U[U < d] # todas las diff menores a d
{if (d < median(U)) # d está d en la parte baja de la distribucion?
  n.tail <- length(U.low)
  else 
    n.tail <- 10000 - length(U.low)
}

print(p <- 2 * n.tail / 10000) # Two tail test
#0.6916

t.test(Y1, Y2, "two.sided")$p.value
#0.7185004

# serie de tiempo autoregresiva de orden cero
## yi = lamba*yi-1 + ei, mu =0

lambda <- 0.4 # coeficiente de autocorrelacion
set.seed(123)
Y <- numeric(20)
for (i in 2:20) {
  Y[i]= lambda * Y[i - 1] + rnorm(1) # errores normales
}
Y <- Y[11:20]


## prueba t H0 : ?? = 0.

Y.ttest <- t.test(Y, alternative = "two.sided")
Y.ttest$p.value # 0.3809946, existe un 38% de probabilidad del error tipo I (rechazar H0 siendo verdadera)

TypeI <- as.numeric(Y.ttest$p.value < 0.05) # cero si el p valor es menor a 0.05, 1 si es mayor
Ybar <- mean(Y)
Yse <- sqrt(var(Y)/10)
c(TypeI, Ybar, Yse)

### simulacion de montecarlo

set.seed(123)
lambda <- 0.4
ttest <- function(lambda){
  Y <- numeric(20)
  for (i in 2:20) Y[i]= lambda * Y[i - 1] + rnorm(1, sd= 1)
  Y <- Y[11:20]
  Y.ttest <- t.test(Y, alternative = "two.sided")
  ypeI <- as.numeric(Y.ttest$p.value < 0.05) # cero si el p valor es menor a 0.05, 1 si es mayor
  Ybar <- mean(Y)
  Yse <- sqrt(var(Y)/10)
  return (c(TypeI, Ybar, Yse))
}

U <- replicate(10000, ttest(lambda)) # simulacion 

mean(U[1,]) # Type I error rate
mean(U[2,]) # Mean value of Ybar
mean(U[3,]) # Mean est. standard error
sd(U[2,]) # Sample std. dev. of Ybar


#638284 codigo de bloqueo

# autocorrelacion espacial 

#






