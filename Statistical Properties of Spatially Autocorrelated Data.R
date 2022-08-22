set.seed(123)
Y <- numeric(20)
lambda <- 0.4
for (i in 2:20) Y[i] <- lambda* Y[i-1] + rnorm(1)
Y <- Y[11:20]
Y

#H0 : ?? = 0. 

Y.ttest <- t.test(Y,alternative = "two.sided") #two.sided para hipotesis con "="
Type1 <- as.numeric(Y.ttest$p.value < 0.05) # if the p value is less than critical 0.05, then creates a 0 valued vector, 1 otherwise
Ybar <- mean(Y)
Yse <- sqrt(var(Y)/length(Y))
c(Type1, Ybar, Yse)

# build the function
set.seed(123)
lambda <- 0.4
ttest <- function(lambda){
  for (i in 2:20) Y[i] <- lambda* Y[i-1] + rnorm(1)
  Y <- Y[11:20]
  Y.ttest <- t.test(Y,alternative = "two.sided") #two.sided para hipotesis con "="
  Type1 <- as.numeric(Y.ttest$p.value < 0.05) # if the p value is less than critical 0.05, then creates a 0 valued vector, 1 otherwise
  Ybar <- mean(Y)
  Yse <- sqrt(var(Y)/length(Y))
  c(Type1, Ybar, Yse)
}

#monte carlo simulation

U <- replicate(10000, ttest(lambda)) #lambda =0.4

mean(U[1,])

# modeling spatial contiguity 


library(spdep)
Y.df <- expand.grid(x = seq(0.5, 23.5),
                    y = seq(23.5, 0.5, by=-1))

lambda <- 0.4
nlist <- cell2nb(24,24)
?invIrM()

# __________________


library(raster)

Y.ras <- raster(ncol = 20, nrow = 20,
                 xmn = 0, xmx = 20,
                 ymn = 0, ymx = 20,
                 crs = NULL)

Y.sp <- as(Y.ras, "SpatialPolygons")

plot(Y.sp)
