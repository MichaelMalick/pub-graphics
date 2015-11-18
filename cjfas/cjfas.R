# CJFAS v72 i12 Dec 2015
# Michael Malick
# 2015-11-17


counts <- read.csv("cjfas-counts.csv", stringsAsFactors = FALSE)
types  <- read.csv("cjfas-types.csv", stringsAsFactors = FALSE)

par(mar = c(4,6,1,1))
barplot(types$count,
        horiz = TRUE,
        names.arg = types$type,
        border = "white",
        las = 1,
        xlab = "Count",
        axes = FALSE)
    axis(1, lwd = 1, lwd.tick = 1, col = "grey50")


cnt <- c(sum(counts$figures),
    sum(counts$multipanel),
    sum(counts$multitype))
nme <- c("total", "multi-panel", "multi-type")
multi <- data.frame(name = nme, count = cnt)
par(mar = c(4,6,1,1))
barplot(multi$count,
        horiz = TRUE,
        names.arg = multi$name,
        border = "white",
        las = 1,
        xlim = c(0, 60),
        xlab = "Count",
        axes = FALSE)
    axis(1, lwd = 1, lwd.tick = 1, col = "grey50")


## Ratio graphics
pdf("~/Desktop/1.pdf", width = 4, height = 4)
    par(mar = c(0.2,0.2,0.2,0.2))
    beav <- beaver1[beaver1$day == 346, ]
    plot(beav$time, beav$temp, 
         type = "l",
         axes = FALSE)
    box(col = "grey50")
dev.off()


pdf("~/Desktop/golden.pdf", width = 6.472, height = 4)
    par(mar = c(0.2,0.2,0.2,0.2))
    beav <- beaver1[beaver1$day == 346, ]
    plot(beav$time, beav$temp, 
         type = "l",
         axes = FALSE)
    box(col = "grey50")
dev.off()
