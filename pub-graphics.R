



## Plot 1
fit1   <- lm(mpg ~ hp, data = mtcars)
pred.x1 <- seq(min(mtcars$hp), max(mtcars$hp), 0.1)
conf1   <- predict(fit1, newdata = data.frame(hp = pred.x1), 
    interval = "confidence")
r1 <- formatC(summary(fit1)$r.squared, 2, format = "f")

fit2   <- lm(mpg ~ disp, data = mtcars)
pred.x2 <- seq(min(mtcars$disp), max(mtcars$disp), 0.1)
conf2   <- predict(fit2, newdata = data.frame(disp = pred.x2), 
    interval = "confidence")
r2 <- formatC(summary(fit2)$r.squared, 2, format = "f")


par(mfrow = c(1 , 2), mar = c(2.3, 1.8, 0.0, 0.0), ps = 12, lend = 2,
        oma = c(2.5, 3, 0.5, 0.5), cex.axis = 1.0)
plot(x = mtcars$hp, y = mtcars$mpg,
    type = "n",
    ylab = "",
    xlab = "Horsepower",
    ylim = c(0, 40),
    xlim = c(40, 351),
    axes = FALSE)
    axis(1, lwd = 0, lwd.tick = 1)
    axis(2, las = 1, lwd = 0, lwd.ticks = 1)
    box()
    polygon(c(pred.x1, rev(pred.x1)), 
        c(conf1[ , 2], rev(conf1[ , 3])),
        col = hcl(h = 235, c = 100, l = 60, alpha = 0.25), border = NA)
    points(x = mtcars$hp, y = mtcars$mpg, pch = 19, cex = 0.8, col = "grey40")
    lines(mtcars$hp, predict(fit1, newdata = data.frame(hp = mtcars$hp)),
        lwd = 1.95, col = hcl(h = 235, c = 100, l = 30))
    mtext("Horsepower", side = 1, line = 2.75)
    mtext("Fuel efficiency (miles / gallon)", side = 2, line = 3)
    text(330, 39, labels = "(a)", cex = 1.1)
    text(300, 30, labels = bquote(paste("R"^2, " = ", .(r1))), cex = 1.1)
plot(x = mtcars$disp, y = mtcars$mpg,
    type = "n",
    ylab = "",
    xlab = "Displacement",
    ylim = c(0, 40),
    xlim = c(50, 501),
    axes = FALSE)
    axis(1, lwd = 0, lwd.tick = 1)
    axis(2, las = 1, lwd = 0, lwd.tick = 1, labels = FALSE)
    box()
    polygon(c(pred.x2, rev(pred.x2)), 
        c(conf2[ , 2], rev(conf2[ , 3])),
        col = "grey80", border = NA)
    points(x = mtcars$disp, y = mtcars$mpg, pch = 19, cex = 0.8, col = "grey40")
    lines(mtcars$disp, predict(fit2, newdata = data.frame(disp = mtcars$disp)),
        lwd = 1.5)
    mtext("Displacement", side = 1, line = 2.75)
    text(465, 39, labels = "(b)", cex = 1.1)
    text(430, 30, labels = bquote(paste("R"^2, " = ", .(r2))), cex = 1.1)

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












