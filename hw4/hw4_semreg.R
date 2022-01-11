library(HRW)
library(mgcv)
data(WarsawApts)
x <- WarsawApts$construction.date
y <- WarsawApts$areaPerMzloty
plot(x, y, bty = 'l', col = 'dodgerblue')
fitGAMcr <- gam(y~s(x, bs = 'cr', k = 30))
xg <- seq(min(x), max(x), length = 1001)
fHatgGAMcr <- predict(fitGAMcr, newdata = data.frame(x = xg))
lines(xg, fHatgGAMcr, col = 'darkgreen')


space_palette = c("#005b6e", "#04668c", "#3c6ca7", "#726eb7", "#a86bba", "#da66ac", "#ff6792")

space_palette = c("#005b6e", "#3c6ca7", "#a86bba", "#da66ac", "#ff6792")
pal = c('#716F81', '#B97A95', '#F6AE99', '#F2E1C1')
pal2 = c('#48466D',
  '#3D84A8',
  '#46CDCF',
  '#ABEDD8')

branded_colors <- list(
  "blue"   = "#00798c",
  "red"    = "#d1495b",
  "yellow" = "#edae49",
  "green"  = "#66a182",
  "navy"   = "#2e4057", 
  "grey"   = "#8d96a3"
)

branded_colors = c()

fitGAMgp <- gam(y~s(x, bs = 'gp', k = 30))
fitGAMps <- gam(y~s(x, bs = 'ps', k = 30))
fitGAMtp <- gam(y~s(x, bs = 'tp', k = 30))

fHatgGAMgp <- predict(fitGAMgp, newdata = data.frame(x = xg))
fHatgGAMps <- predict(fitGAMps, newdata = data.frame(x = xg))
fHatgGAMtp <- predict(fitGAMtp, newdata = data.frame(x = xg))


data_plot <- data.frame(cbind(xg, fHatgGAMcr, fHatgGAMgp, fHatgGAMps, fHatgGAMtp))
head(data_plot)
data_plot %>% 
  gather('fit','value', -xg) %>% 
  ggplot() +
  geom_point(data = data.frame(cbind(x,y)), aes(x = x, y = y), color = 'lightsteelblue3', alpha=0.5) + 
  geom_line(aes(x = xg, y = value, color = fit), size = 0.5) +
  scale_color_manual(values = as.vector(unlist(branded_colors)))


## b)

fitGAMcr40 <- gam(y~s(x, bs = 'cr', k = 40))
fHatgGAMcr40 <- predict(fitGAMcr40, newdata = data.frame(x = xg))

fitGAMcr50 <- gam(y~s(x, bs = 'cr', k = 50))
fHatgGAMcr50 <- predict(fitGAMcr50, newdata = data.frame(x = xg))

fitGAMcr60 <- gam(y~s(x, bs = 'cr', k = 60))
fHatgGAMcr60 <- predict(fitGAMcr60, newdata = data.frame(x = xg))

data_plot <- data.frame(cbind(xg, fHatgGAMcr, fHatgGAMcr40, fHatgGAMcr50, fHatgGAMcr60))

data_plot %>% 
  gather('fit','value', -xg) %>% 
  ggplot() +
  geom_point(data = data.frame(cbind(x,y)), aes(x = x, y = y), color = 'lightsteelblue3', alpha=0.5) + 
  geom_line(aes(x = xg, y = value, color = fit), size = 0.5) +
  scale_color_manual(values = as.vector(unlist(branded_colors)))



fitGAMcr2 <- gam(y~s(x, bs = 'cr'))


fitGAMcr10 <- gam(y~s(x, bs = 'cr', k = 10))
fitGAMcr20 <- gam(y~s(x, bs = 'cr', k = 20))

res_kcheck = rbind( k.check(fitGAMcr10),
                    k.check(fitGAMcr20),
                    k.check(fitGAMcr),
                    k.check(fitGAMcr40),
                    k.check(fitGAMcr50),
                    k.check(fitGAMcr60))
res_kcheck = cbind(seq(10,60, by=10), res_kcheck)
res_kcheck

##### ---------
test_data <-
  data.frame(
    var0 = 100 + c(0, cumsum(runif(49, -20, 20))),
    var1 = 150 + c(0, cumsum(runif(49, -10, 10))),
    date = seq(as.Date("2002-01-01"), by="1 month", length.out=100)
  )
test_data %>%
  gather(key,value, var0, var1) %>%
  ggplot(aes(x=date, y=value, colour=key)) +
  geom_line()


