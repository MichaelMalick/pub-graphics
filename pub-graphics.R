# Pub Graphics Examples
# Michael Malick
# 2015-11-17


# ----------------------------
# Example 1: NPGO
# ----------------------------

## Prepare data
npgo <- read.table( "http://www.o3d.org/npgo/data/NPGO.txt", header = FALSE)
names(npgo) <- c("year", "month", "npgo")
npgo <- npgo[npgo$year < 2014, ]
npgo.avg <- lapply(split(npgo, npgo$year), function(x) {
    data.frame(
        year = unique(x$year),
        npgo = mean(x$npgo))
})
npgo.avg <- do.call("rbind", npgo.avg)
row.names(npgo.avg) <- NULL

## Look at data
head(npgo, 15)
head(npgo.avg)

## Set graphics dimensions (r = 1.618)
dev.new(width = 5, height = 3.09)

## Set graphical parameters
par(
    mar  = c(2.3, 0.3, 0.0, 0.0), # (bottom, left, top, right)
    oma  = c(2.0, 4.2, 0.5, 0.5),
    ps   = 12,
    lend = 2,
    cex.axis = 0.9)

## Setup empty graphic
plot(x = 1950:2013, y = npgo.avg$npgo, 
     type = "n", 
     ylim = c(-3, 3),
     axes = FALSE)

## Add axes
axis(1, lwd = 0, lwd.tick = 1, col = "grey50")
axis(2, las = 1, lwd = 0, lwd.ticks = 1, col = "grey50")
box(col = "grey50")

## Add reference line
abline(h = 0, col = "grey50", lty = 2, lwd = 0.8)

## Add monthly time series
for(i in 1:12) 
    lines(x = 1950:2013, y = npgo$npgo[npgo$month == i], col = "grey70")

## Add annual average
lines(x = 1950:2013, y = npgo.avg$npgo, col = "black", lwd = 2)

## Add axis labels
mtext("Year", side = 1, line = 2.75)
mtext("NPGO", side = 2, line = 2.75)

## Add legend
legend("topleft", legend = c("annual average", "monthly values"),
       col = c("black", "grey50"),
       lwd = c(2, 1),
       cex = 0.8,
       box.lty = 0)





# ----------------------------
# Example 2: beavers
# ----------------------------

## Prepare data
beav1 <- beaver1[beaver1$day == 346, ]
beav2 <- beaver2[beaver2$day == 307, ]

## Density of temp
dens1 <- density(beav1$temp)
dens2 <- density(beav2$temp)

## Set min and max values for density
d.xmin <- min(c(dens1$x, dens2$x))
d.xmax <- max(c(dens1$x, dens2$x))
d.ymin <- min(c(dens1$y, dens2$y))
d.ymax <- max(c(dens1$y, dens2$y))

## Function for plot labels
add_label <- function(label, xfrac = 0.00, yfrac = 0.07, pos = 4, ...) {
    u <- par("usr")
    x <- u[1] + xfrac * (u[2] - u[1])
    y <- u[4] - yfrac * (u[4] - u[3])
    text(x, y, label, pos = pos, ...)
}

## Set graphics dimensions
dev.new(width = 4, height = 5)

## Set graphical parameters
par(
    mfrow = c(2, 1),
    mar   = c(4.5, 0.3, 0.0, 0.0), # (bottom, left, top, right)
    oma   = c(0.0, 5.2, 0.5, 0.5),
    ps    = 12,
    lend  = 2,
    cex.axis = 0.8)

## Setup empty graphic for time series
plot(beav1$time, beav1$temp, 
    type = "n", 
    ylab = "",
    xlab = "",
    ylim = c(36, 38.5),
    axes = FALSE)

## Add axes
axis(1, lwd = 0, lwd.tick = 1, col = "grey50")
axis(2, las = 1, lwd = 0, lwd.ticks = 1, col = "grey50")
box(col = "grey50")

## Add time series
lines(beav1$time, beav1$temp, type = "l")
lines(beav2$time, beav2$temp, type = "l", lty = 2, col = "red3")

## Add labels
text(x = 1700, y = 36.58, labels = "Beaver #1", cex = 0.7)
text(x = 1700, y = 37.55, labels = "Beaver #2", cex = 0.7, col = "red3")
mtext("Time", side = 1, line = 2.5)
mtext(expression(paste("Temperature (", degree, "C)")), side = 2, line = 3.5)
add_label("(a)", cex = 0.8)

## Setup empty graphic for densities
plot(dens1, type = "n", 
    xlim = c(d.xmin, d.xmax), 
    ylim = c(c(d.ymin, d.ymax)),
    main = "",
    ylab = "",
    xlab = "",
    axes = FALSE)

## Add densities
lines(dens1$x, dens1$y)
lines(dens2$x, dens2$y, lty = 2, col = "red3")

## Add axes
axis(1, lwd = 0, lwd.tick = 1, col = "grey50")
axis(2, las = 1, lwd = 0, lwd.ticks = 1, col = "grey50")
box(col = "grey50")

## Add labels
mtext(expression(paste("Temperature (", degree, "C)")), side = 1, line = 2.5)
mtext("Density", side = 2, line = 3.5)
add_label("(b)", cex = 0.8)





# ----------------------------
# Example 3: regression
# ----------------------------

## Prepare data
fit1    <- lm(mpg ~ hp, data = mtcars)
pred.x1 <- seq(min(mtcars$hp), max(mtcars$hp), 0.1)
r1      <- formatC(summary(fit1)$r.squared, 2, format = "f")
pred.y1 <- predict(fit1, newdata = data.frame(hp = mtcars$hp))
conf1   <- predict(fit1, newdata = data.frame(hp = pred.x1),
                   interval = "confidence", level = 0.95)

fit2    <- lm(mpg ~ disp, data = mtcars)
pred.x2 <- seq(min(mtcars$disp), max(mtcars$disp), 0.1)
r2      <- formatC(summary(fit2)$r.squared, 2, format = "f")
pred.y2 <- predict(fit2, newdata = data.frame(disp = mtcars$disp))
conf2   <- predict(fit2, newdata = data.frame(disp = pred.x2),
                   interval = "confidence", level = 0.95)

## Function for plot labels
add_label <- function(label, xfrac = 0.00, yfrac = 0.07, pos = 4, ...) {
    u <- par("usr")
    x <- u[1] + xfrac * (u[2] - u[1])
    y <- u[4] - yfrac * (u[4] - u[3])
    text(x, y, label, pos = pos, ...)
}

## Set colors
library(RColorBrewer)
cols <- brewer.pal(8, "Blues")

## Set graphics dimensions
dev.new(width = 7, height = 3.5)

## Set graphical parameters
par(
    mfrow = c(1, 2),
    mar   = c(2.3, 0.0, 0.0, 0.0),
    ps    = 12,
    lend  = 2,
    oma   = c(2.0, 4.2, 0.5, 0.5),
    cex.axis = 1.0)

## Setup empty graphic for horsepower
plot(x = mtcars$hp, y = mtcars$mpg,
    type = "n",
    ylab = "",
    xlab = "",
    ylim = c(0, 40),
    xlim = c(40, 351),
    axes = FALSE)

## Add axes
axis(1, lwd = 0, lwd.tick = 1, col = "grey50")
axis(2, las = 1, lwd = 0, lwd.ticks = 1, col = "grey50")
box(col = "grey50")

## Add confidence region
polygon(c(pred.x1, rev(pred.x1)), 
    c(conf1[ , 2], rev(conf1[ , 3])),
    col = cols[2], border = NA)

## Add points and lines
points(x = mtcars$hp, y = mtcars$mpg, pch = 19, cex = 0.8, col = "grey40")
lines(mtcars$hp, predict(fit1, newdata = data.frame(hp = mtcars$hp)),
        lwd = 2.1, col = cols[8])

## Add labels
mtext("Horsepower", side = 1, line = 2.75)
mtext("Fuel efficiency (miles / gallon)", side = 2, line = 3)
text(310, 39, labels = bquote(paste("R"^2, " = ", .(r1))), cex = 1.1)
add_label("(a)", cex = 1.0, yfrac = 0.05)

## Setup empty graphic for displacement
plot(x = mtcars$disp, y = mtcars$mpg,
    type = "n",
    ylab = "",
    xlab = "",
    ylim = c(0, 40),
    xlim = c(50, 501),
    axes = FALSE)

## Add axes
axis(1, lwd = 0, lwd.tick = 1, col = "grey50")
axis(2, las = 1, lwd = 0, labels = FALSE, col = "grey50")
box(col = "grey50")

## Add confidence region
polygon(c(pred.x2, rev(pred.x2)), 
    c(conf2[ , 2], rev(conf2[ , 3])),
    col = cols[2], border = NA)

## Add points and lines
points(x = mtcars$disp, y = mtcars$mpg, pch = 19, cex = 0.8, col = "grey40")
lines(mtcars$disp, predict(fit2, newdata = data.frame(disp = mtcars$disp)),
    lwd = 2.1, col = cols[8])

## Add labels
mtext("Displacement", side = 1, line = 2.75)
text(440, 39, labels = bquote(paste("R"^2, " = ", .(r2))), cex = 1.1)
add_label("(b)", cex = 1.0, yfrac = 0.05)


