library(stats)
library(forecast)
library(strucchange)
library(tseries)
library(rugarch)
library(fUnitRoots)

# get data
SENSEX <- read.csv("SENSEX.csv")
# take data from 1995 to 2010 (16 years)
yt <- ts(SENSEX[3204:7158,]$Daily.return)
# test on 2011 to 2015 (5 years)
test_y <- ts(SENSEX[7159:8388,]$Daily.return)

# visual inspection
plot(yt)
plot(test_y)

# confirm stationarity
summary(lm(yt~time(yt)+factor(time(yt)%%5)))
summary(yt)
acf(yt)
pacf(yt)
adfTest(yt, lag=1, type="ct")
urkpssTest(yt)


# fit possible ARMA(p,q) models and get residuals
arma_yt <- auto.arima(yt, ic="bic")
et <- residuals(arma_yt)
plot(et)
acf(et)
pacf(et)
jarqueberaTest(et)


# using rugarch
spec_arch_1 <- ugarchspec(variance.model = list(model = "sGARCH", 
                                              garchOrder = c(1, 0)),
                        mean.model    = list(armaOrder = c(0, 1), include.mean=TRUE),
                                             distribution.model = "std")

spec_arch_2 <- ugarchspec(variance.model = list(model = "sGARCH", 
                                              garchOrder = c(2, 0)),
                        mean.model    = list(armaOrder = c(0, 1) , include.mean=TRUE),
                                             distribution.model = "std")

spec_arch_10 <- ugarchspec(variance.model = list(model = "sGARCH", 
                                              garchOrder = c(10, 0)),
                        mean.model    = list(armaOrder = c(0, 1), include.mean=TRUE),
                                             distribution.model = "std")

spec_garch <- ugarchspec(variance.model = list(model = "sGARCH", 
                                               garchOrder = c(1, 2)),
                         mean.model    = list(armaOrder = c(0, 1), include.mean=TRUE),
                                              distribution.model = "std")

spec_aparch <- ugarchspec(variance.model = list(model = "apARCH", 
                                               garchOrder = c(2, 0)),
                         mean.model    = list(armaOrder = c(0, 1), include.mean=TRUE),
                                              distribution.model = "std")

spec_egarch <- ugarchspec(variance.model = list(model = "eGARCH", 
                                              garchOrder = c(1, 2)),
                        mean.model    = list(armaOrder = c(0, 1), include.mean=TRUE),
                                             distribution.model = "std")


arch_1 <- ugarchfit(spec=spec_arch_1, data=yt, solver="hybrid", solver.control = list(trace=0), out.sample=1000)
forc_arch_1 <- ugarchforecast(arch_1, n.ahead=1, n.roll=1000)

arch_2 <- ugarchfit(spec=spec_arch_2, data=yt, solver="hybrid", solver.control = list(trace=0), out.sample=1000)
forc_arch_2 <- ugarchforecast(arch_2, n.ahead=1, n.roll=1000)

arch_10 <- ugarchfit(spec=spec_arch_10, data=yt, solver="hybrid", solver.control = list(trace=0), out.sample=1000)
forc_arch_10 <- ugarchforecast(arch_10, n.ahead=1, n.roll=1000)

garch <- ugarchfit(spec=spec_garch, data=yt, solver="hybrid", solver.control = list(trace=0), out.sample=1000)
forc_garch <- ugarchforecast(garch, n.ahead=1, n.roll=1000)

egarch <- ugarchfit(spec=spec_egarch, data=yt, solver="hybrid", solver.control = list(trace=0), out.sample=1000)
forc_egarch <- ugarchforecast(egarch, n.ahead=1, n.roll=1000)

aparch <- ugarchfit(spec=spec_aparch, data=yt, solver="hybrid", solver.control = list(trace=0), out.sample=1000)
forc_aparch <- ugarchforecast(aparch, n.ahead=1, n.roll=1000)

fpm(forc_arch_1)
fpm(forc_arch_2)
fpm(forc_arch_10)
fpm(forc_garch)
fpm(forc_egarch)
fpm(forc_aparch)
