library(HRW); data(WarsawApts)
x <-  WarsawApts$construction.date
y <- WarsawApts$areaPerMzloty
fitCubic <- lm(y~poly(x, 3, raw = TRUE))
ng <- 101
xg <- seq(1.01*min(x) - 0.01*max(x), 1.01*max(x) - 0.01*min(x), length = ng)
fHatCubicg <- as.vector(cbind(rep(1,ng), xg, xg^2, xg^3)%*%fitCubic$coef)

plot(x, y, col = 'dodgerblue')
lines(xg, fHatCubicg, col = 'darkgreen')
plot(fitted(fitCubic), residuals(fitCubic), col = 'dodgerblue')
abline(0, 0, col = 'slateblue', lwd = 2)


predict(fitCubic, newdata = data.frame(cbind(xg, xg^2, xg^3)))

m1 <- lm(areaPerMzloty ~ construction.date + n.rooms, data = WarsawApts)
nd <-  data.frame(construction.date = c(1945, 2005), n.rooms = c(2,3))
predict(m1, newdata = nd)
as.matrix(cbind(rep(1,2), nd))%*%m1$coefficients


#### b)
trLin <- function(x, kappa) return((x-kappa)*(x>kappa))
trLin(3,5)


#### c)

knots <- seq(min(x), max(x), length = 5)[-c(1,5)]
X <- cbind(1,x)
for(k in 1:3) X <- cbind(X, trLin(x, knots[k]))
fitTLQ <- lm(y ~ -1 + X)
Xg <- cbind(1, xg)
for (k in 1:3) Xg <- cbind(Xg, trLin(xg, knots[k]))
fHatTLQg <- as.vector(Xg%*%fitTLQ$coef)
plot(x, y, col = 'dodgerblue')
lines(xg, fHatTLQg, col = 'darkgreen', lwd = 2)
plot(fitted(fitTLQ), residuals(fitTLQ), col = 'dodgerblue')
abline(0, 0, col = 'slateblue', lwd = 2)





