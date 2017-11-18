a <- c(64, 46, 45, 37, 40, 63, 41, 32, 36, 61, 31) # Kevin De Bruyne
b <- c(25, 16, 21, 6, 10, 19, 13, 13, 8, 21, 11) # Fernando
c <- c(54, 36, 40, 24, 39, 56, 38, 42, 40, 55, 41) # David Silva

my.df <- data.frame(Kevin.De.Bruyne = a, Fernando = b, David.Silva = c)
summary(my.df)

test.aov <- aov(formula = Kevin.De.Bruyne ~ Fernando + David.Silva, data = my.df)
summary(test.aov)

plot(a,c, xlim = c(0,3), ylim = c(0, 70), type = "n", xaxt="n", yaxs = "i", main = "Touches in Final Third", xlab = "", ylab = "")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "lightskyblue1")

my.col.a <- "darkorchid3"
my.col.b <- "coral3"
my.col.c <- "green4"

points(rnorm(length(a),0.5 , 0.05), a, pch = 1, col = my.col.a)
points(rnorm(length(b),1.5 , 0.05), b, pch = 1, col = my.col.b)
points(rnorm(length(c),2.5 , 0.05), c, pch = 1, col = my.col.c)
axis(side = 1, at = c(0.5, 1.5, 2.5), labels = c("Kevin De Bruyne", "Fernando", "David Silva")) 

density.a <- density(a)

a.plot.ma <- matrix(nrow = length(density.a$x), ncol = 3)
a.plot.ma[, 1] <- density.a$x
a.plot.ma[, 2] <- 0.5 + (density.a$y * 0.3 / max(density.a$y))
a.plot.ma[, 3] <- 0.5 - (density.a$y * 0.3 / max(density.a$y))

a.plot.ma <- subset(a.plot.ma, a.plot.ma[, 1] > quantile(a)[[1]] & a.plot.ma[, 1] < quantile(a)[[5]])

lines(a.plot.ma[,2], a.plot.ma[,1], col = "darkorchid3")
lines(a.plot.ma[,3], a.plot.ma[,1], col = "darkorchid3")

segments(min(a.plot.ma[,3]), mean(a), max(a.plot.ma[,2]), mean(a), lwd = 4, col = "darkorchid")

segments(a.plot.ma[1,2], a.plot.ma[1,1], a.plot.ma[1,3], a.plot.ma[1,1], lwd = 1, col = "darkorchid")
segments(a.plot.ma[nrow(a.plot.ma),2], a.plot.ma[nrow(a.plot.ma),1], a.plot.ma[nrow(a.plot.ma),3], a.plot.ma[nrow(a.plot.ma),1], lwd = 1, col = "darkorchid")

density.b <- density(b)

b.plot.ma <- matrix(nrow = length(density.b$x), ncol = 3)
b.plot.ma[, 1] <- density.b$x
b.plot.ma[, 2] <- 1.5 + (density.b$y * 0.3 / max(density.b$y))
b.plot.ma[, 3] <- 1.5 - (density.b$y * 0.3 / max(density.b$y))

b.plot.ma <- subset(b.plot.ma, b.plot.ma[, 1] > quantile(b)[[1]] & b.plot.ma[, 1] < quantile(b)[[5]])

lines(b.plot.ma[,2], b.plot.ma[,1], col = "coral3")
lines(b.plot.ma[,3], b.plot.ma[,1], col = "coral3")

segments(min(b.plot.ma[,3]), mean(b), max(b.plot.ma[,2]), mean(b), lwd = 4, col = "coral3")

segments(b.plot.ma[1,2], b.plot.ma[1,1], b.plot.ma[1,3], b.plot.ma[1,1], lwd = 1, col = "coral3")
segments(b.plot.ma[nrow(b.plot.ma),2], b.plot.ma[nrow(b.plot.ma),1], b.plot.ma[nrow(b.plot.ma),3], b.plot.ma[nrow(b.plot.ma),1], lwd = 1, col = "coral3")

density.c <- density(c)

c.plot.ma <- matrix(nrow = length(density.c$x), ncol = 3)
c.plot.ma[, 1] <- density.c$x
c.plot.ma[, 2] <- 2.5 + (density.c$y * 0.3 / max(density.c$y))
c.plot.ma[, 3] <- 2.5 - (density.c$y * 0.3 / max(density.c$y))

c.plot.ma <- subset(c.plot.ma, c.plot.ma[, 1] > quantile(c)[[1]] & c.plot.ma[, 1] < quantile(c)[[5]])

lines(c.plot.ma[,2], c.plot.ma[,1], col = "green4")
lines(c.plot.ma[,3], c.plot.ma[,1], col = "green4")

segments(min(c.plot.ma[,3]), mean(c), max(c.plot.ma[,2]), mean(c), lwd = 4, col = "green4")

segments(c.plot.ma[1,2], c.plot.ma[1,1], c.plot.ma[1,3], c.plot.ma[1,1], lwd = 1, col = "green4")
segments(c.plot.ma[nrow(c.plot.ma),2], c.plot.ma[nrow(c.plot.ma),1], c.plot.ma[nrow(c.plot.ma),3], c.plot.ma[nrow(c.plot.ma),1], lwd = 1, col = "green4")

