# Tutorial Example
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


## 1. Set graphics dimensions (w = 5, h = 3.09, r = 1.618)

## 2. Set graphical parameters (bottom, left, top, right)

## 3. Setup empty graphic

## 4. Add axes

## 5. Add reference line

## 6. Add monthly time series

## 7. Add annual average

## 8. Add axis labels

## 9. Add legend




