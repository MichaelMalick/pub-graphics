



## Plot 1
fit1   <- lm(mpg ~ hp, data = mtcars)
pred.x1 <- seq(min(mtcars$hp), max(mtcars$hp), 0.1)
conf1   <- predict(fit1, newdata = data.frame(hp = pred.x1), 
    interval = "confidence")
r1 <- formatC(summary(fit1)$r.squared, 2, format = "f")
pred.y1 <- predict(fit1, newdata = data.frame(hp = mtcars$hp))

fit2   <- lm(mpg ~ disp, data = mtcars)
pred.x2 <- seq(min(mtcars$disp), max(mtcars$disp), 0.1)
conf2   <- predict(fit2, newdata = data.frame(disp = pred.x2), 
    interval = "confidence")
r2 <- formatC(summary(fit2)$r.squared, 2, format = "f")
pred.y2 <- predict(fit2, newdata = data.frame(disp = mtcars$disp))


lett <- c("(a)", "(b)")
xlab <- c("Horsepower", "Displacement")
r.sq <- c(r1, r2)

dev.new(width = 7, height = 3.5)
par(mfrow = c(1 , 2), mar = c(2.3, 0.3, 0.0, 0.0), ps = 12, lend = 2,
    oma = c(2.0, 4.2, 0.5, 0.5), cex.axis = 1.0)
for(i in 1:2) {
    if(i == 1) {
        x      <- mtcars$hp
        pred.x <- pred.x1
        conf   <- conf1
        pred.y <- pred.y1
        xlim   <- c(40, 351)
    } else{
        x      <- mtcars$disp
        pred.x <- pred.x2
        conf   <- conf2
        pred.y <- pred.y2
        xlim   <- c(50, 501)
    }
    plot(x = x, y = mtcars$mpg,
        type = "n",
        ylab = "",
        xlab = "",
        ylim = c(0, 40),
        xlim = xlim,
        axes = FALSE)
        usr <- par("usr")
        text(usr[2], usr[4], lett[i], adj = c(1.3, 1.6))
        text(usr[2], usr[4], bquote(paste("R"^2, " = ", .(r.sq[i]))), 
            adj = c(1.5, 4))
        if(i == 1) {
            axis(1, lwd = 0, lwd.tick = 1)
            axis(2, las = 1, lwd = 0, lwd.ticks = 1)
            mtext("Fuel efficiency (miles / gallon)", side = 2, line = 3)
        }
        if(i == 2) { 
            axis(1, lwd = 0, lwd.tick = 1)
        }
        box()
        polygon(c(pred.x, rev(pred.x)), 
            c(conf[ , 2], rev(conf[ , 3])),
            col = hcl(h = 235, c = 100, l = 60, alpha = 0.25), border = NA)
        points(x = x, y = y, pch = 19, cex = 0.8, 
            col = "grey40")
        lines(x, pred.y, lwd = 2.1, col = hcl(h = 235, c = 100, l = 30))
        mtext(xlab[i], side = 1, line = 2.75)
}





dev.new(width = 7, height = 3.5)
par(mfrow = c(1 , 2), mar = c(2.3, 0.3, 0.0, 0.0), ps = 12, lend = 2,
        oma = c(2.0, 4.2, 0.5, 0.5), cex.axis = 1.0)
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
        lwd = 2.1, col = hcl(h = 235, c = 100, l = 30))
    mtext("Horsepower", side = 1, line = 2.75)
    mtext("Fuel efficiency (miles / gallon)", side = 2, line = 3)
    text(345, 39, labels = "(a)", cex = 1.1)
    text(300, 30, labels = bquote(paste("R"^2, " = ", .(r1))), cex = 1.1)
plot(x = mtcars$disp, y = mtcars$mpg,
    type = "n",
    ylab = "",
    xlab = "Displacement",
    ylim = c(0, 40),
    xlim = c(50, 501),
    axes = FALSE)
    axis(1, lwd = 0, lwd.tick = 1)
    axis(2, las = 1, lwd = 0, labels = FALSE)
    box()
    polygon(c(pred.x2, rev(pred.x2)), 
        c(conf2[ , 2], rev(conf2[ , 3])),
        col = hcl(h = 235, c = 100, l = 60, alpha = 0.25), border = NA)
    points(x = mtcars$disp, y = mtcars$mpg, pch = 19, cex = 0.8, col = "grey40")
    lines(mtcars$disp, predict(fit2, newdata = data.frame(disp = mtcars$disp)),
        lwd = 2.1, col = hcl(h = 235, c = 100, l = 30))
    mtext("Displacement", side = 1, line = 2.75)
    text(495, 39, labels = "(b)", cex = 1.1)
    text(430, 30, labels = bquote(paste("R"^2, " = ", .(r2))), cex = 1.1)

## Plot 2
dat <- data.frame(
    stock = 1:30,
    coef1 = c(runif(10) + 1, runif(20) * -1),
    coef2 = c(runif(10), runif(20) * -1))

par(mfrow = c(2, 2))
    plot(x = dat$coef1, y = dat$stock)
    plot(x = dat$coef2, y = dat$stock)
    plot(density(dat$coef1))
    plot(density(dat$coef2))



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












