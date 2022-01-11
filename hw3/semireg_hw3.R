
#### Exercise 1

# (a)
ng <- 101
xg <- seq(0, 1, length=ng)
T1g <- rep(1, ng)
T2g <- xg
T3g <- (xg - .5)*(xg - .5>0)
B1g <- (1 - 2*xg)*(1 - 2*xg>0)
B2g <- 1 - abs(2*xg - 1)
B3g <- 2*T3g
par(mfrow=c(2,1))
plot(0, type = "n", xlim=c(0,1), ylim=c(0,1), xlab="x", ylab="") #, bty="1")
lines(xg, T1g, col=1)
lines(xg, T2g, col=2)
lines(xg, T3g, col=3)
text(0.1, 0.8, expression(T[1]), col=1)
text(0.4, 0.5, expression(T[2]), col=2)
text(0.8, 0.2, expression(T[3]), col=3)
plot (0, type = "n", xlim=c(0,1), ylim=c(0,1), xlab="x", ylab="") #, bty="1")
lines(xg, B1g, col=4)
lines(xg, B2g, col=5)
lines(xg, B3g, col=6)
text(0.1, 0.9, expression(B[1]), col=4)
text(0.4, 0.9, expression(B[2]), col=5)
text(0.6, 0.6, expression(B[3]), col=6)






## e)

set.seed(1)
n <- 100
x <- sort(runif(n))
y <- cos(2*pi*x) + 0.2*rnorm(n)
plot(x,y,col='dodgerblue', bty='l')
XT <- cbind(rep(1,n), x, (2*x - 1)*(2*x - 1 > 0))
XB <- cbind((1 - 2*x)*(1 - 2*x > 0), 1 - abs(2*x - 1), (2*x - 1)*(2*x - 1 > 0))
fitT <- lm(y~XT-1)
fitB <- lm(y~XB-1)
lines(x, fitted(fitT), col = 'orange', lwd = 6)
lines(x, fitted(fitB), col = 'darkgreen', lwd = 2)
