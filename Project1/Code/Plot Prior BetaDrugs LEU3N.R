#Plot prior N(0, 1000) and beta estimates for betadrugs in LEU3N 

mean <- 0
var <- 1000
sd <- sqrt(var)

x <- seq(-4,4,length=1000)*sd + mean
hx <- dnorm(x,mean,sd)

plot(hx ~ x, pch = ".")
abline(v = -83.2590)
abline(v = -81.7680)
