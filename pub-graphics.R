



## Plot 1
fit1   <- lm(mpg ~ hp, data = mtcars)
pred.x1 <- seq(min(mtcars$hp), max(mtcars$hp), 0.1)
conf1   <- predict(fit1, newdata = data.frame(hp = pred.x1), 
    interval = "confidence")

fit2   <- lm(mpg ~ disp, data = mtcars)
pred.x2 <- seq(min(mtcars$disp), max(mtcars$disp), 0.1)
conf2   <- predict(fit2, newdata = data.frame(disp = pred.x2), 
    interval = "confidence")


par(mfrow = c(2 , 1))
plot(x = mtcars$hp, y = mtcars$mpg)
    abline(fit1)
    lines(x = pred.x1, y = conf1[ , 2], lty = 2)
    lines(x = pred.x1, y = conf1[ , 3], lty = 2)
plot(x = mtcars$disp, y = mtcars$mpg)
    abline(fit2)
    lines(x = pred.x2, y = conf2[ , 2], lty = 2)
    lines(x = pred.x2, y = conf2[ , 3], lty = 2)


## Plot 2
dat <- data.frame(
    stock = 1:30,
    coef1 = c(runif(10) + 1, runif(20) * -1),
    coef2 = c(runif(10), runif(20) * -1))

par(mfrow = c(1, 2))
    plot(x = dat$coef1, y = dat$stock)
    plot(x = dat$coef2, y = dat$stock)



## Plot 3

library(plyr)
npgo <- read.table( "http://www.o3d.org/npgo/data/NPGO.txt", header = FALSE)
names(npgo) <- c("year", "month", "npgo")
npgo <- npgo[npgo$year < 2014, ]

hist(npgo$npgo)
plot(npgo$year, npgo$npgo, type = "h")

plot(1950:2013, 1950:2013, type = "n", ylim = c(-3, 3))
for(i in 1:12) 
    lines(x = 1950:2013, y = npgo$npgo[npgo$month == i])












