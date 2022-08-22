x <-c(8.26, 6.33, 10.4, 5.27, 5.35, 5.61, 6.12, 6.19, 5.2,
      7.01, 8.74, 7.78, 7.02, 6, 6.5, 5.8, 5.12, 7.41, 6.52, 6.21,
      12.28, 5.6, 5.38, 6.6, 8.74)

#estimar el coeficiente de variacion de x , CV = ???Var/ x
#varianza de la media 

CV <- function(x) {
  sqrt(var(x))/mean(x) #var(x) returns the sample variance
}

CV(x) #0.2524712 

#boostrap sample

sample(x,replace=T) # genera una muestra del mismo tamaño muestral con reemplazo

# una estimacion de CV usando bootrap

CV(sample(x, replace = T)) # 0.2568196
CV(sample(x, replace = T)) # 0.2089048
CV(sample(x, replace = T)) # 0.2450638


# generar 1000 estimaciones de CV basada en bootstrap samples


boot <- numeric(1000)

for (i in 1:1000) {
  boot[i] <- CV(sample(x, replace = T))
}

#media y varianza de as estimaciones

mean(boot)
var(boot)

hist(boot)

# CALCULO DEL SESGO- BIAS
# Recall from the notes that the estimate of the bias is given by the difference between the
# mean of the bootstrap values and the initial estimate

bias <- mean(boot) - CV(x) #-0.01158867

#bootstrap-corrected estimate of the CV is just the original estimate minus the bias,

CV(x) - bias

#Assuming normality, the approximate 95% confidence interval is given by

CV(x) - bias - 1.96*sqrt(var(boot))
CV(x) - bias + 1.96*sqrt(var(boot))

#Jackknife

jack <- numeric(length(x)-1)

pseudo <- numeric(length(x))

#

for (j in 1:1000) {
  if(j<i)
    jack[j] <- x[i]
  else
    if(j>i)
    jack[j-1] <- x[i]
}

for (j in 1:length(x)) if(j < i) jack[j] <- x[j]
else if(j > i) jack[j-1] <- x[j]
