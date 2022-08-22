# random intercept model
library(nlme) # modelos mixtos lineales y mas
library(agridat) # dataset
library(tidyverse) # maniulacion de datos


data <- lasrosas.corn
# factorial variable rep in our dataset describe some clusters in the data
# la idea es que observaciones enn un cluster (en un campo o granja espceficai)
# tiene un efecto en el rendimiento y esto añade variacion aleatoria al modelo

lme1 <- nlme::lme(yield ~ nf + bv*topo, 
                  random = ~1|rep,
                  data = data)
summary(lme1)  

