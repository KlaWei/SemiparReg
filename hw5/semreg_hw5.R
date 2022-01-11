library(HRW)
library(nlme)

data(WarsawApts)
x <- WarsawApts$construction.date
y <- WarsawApts$areaPerMzloty
plot(x,y,xlab="construction date",
     ylab="area per million zloty", col="dodgerblue")

area.perMz <- WarsawApts$areaPerMzloty
const.date <- WarsawApts$construction.date

numObs <- length(const.date)
X <- cbind(rep(1,numObs), const.date)

# Creating the random design matrix “Z” via basis function definition with
# a user-specified number of knots and evaluated at the predictor const.date
# values
numIntKnots <- 35
intKnots <- quantile(unique(const.date),
                     seq(0, 1,
                         length = numIntKnots + 2))[-c(1,numIntKnots + 2)]
Z <- outer(const.date, intKnots, "-")
Z <- Z * (Z > 0)

# Setting up the linear mixed model defining the semiparametric model
dummyId <- factor(rep(1, numObs))
Z.sm <- list(dummyId = pdIdent(~ -1 + Z))
fit <- lme(area.perMz ~ -1 + X, random = Z.sm)

# (a) Setting up the grid values together with the fixed and 
# random design matrices
ng <- 1001
range.date <- range(const.date)
dategrid <- seq(range.date[1], range.date[2], length = ng)
Xg <- cbind(rep(1, ng), dategrid)
Zg <- outer(dategrid, intKnots, "-")
Zg <- Zg * (Zg > 0)

# (b) Extracting the model parameters
betaHat <- as.vector(fit$coef$fixed)
uHat <- as.vector(fit$coef$random[[1]])

# (c) Estimated semiparametric model fit
fhat <- Xg %*% betaHat + Zg %*% uHat

# (d) Plot of the fitted curve
plot(const.date, area.perMz,
     xlab = "Construction date (years)",
     ylab = "Area per million zloty (m2)",
     main = "Warsaw apartments: area vs. construction date")
lines(dategrid, fhat, lwd = 2, col = "darkgreen")




