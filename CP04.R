library(readr)
library(forecast)
library(ggplot2)
library(xts)
library(ggfortify)
library(ggplot2)
library(tseries)





rawData <- read_delim("IngresosApple.csv", 
                      ";", escape_double = FALSE, trim_ws = TRUE)


rawVentas <- rawData$Ingresos
rawDate <- seq(as.Date("2008/04/01"),
               as.Date("2017/07/01"), by = "quarter")

xVentas <- xts(rawVentas, order.by = rawDate)
xVentas <- to.quarterly(xVentas)
zVentas <- as.zoo(xVentas$xVentas.Close)

tsVentas <- ts(coredata(zVentas), start = c(2008, 1), frequency = 4)

plot(decompose(tsVentas))
adf.test(tsVentas, alternative="stationary",k = 4)


install.packages("timsac")
decomp(co2, trade=TRUE,plot=TRUE)

autoplot(zVentas)+
  ggtitle("Ventas Trimestrales Apple")+
  xlab("Trimestres")+
  ylab("Ventas")


#Seasonal Plot
ggfreqplot(tsVentas,freq=4,nrow=1,facet.labeller=c("1T","2T","3T","4T"))+
  ggtitle("Ventas Trimestrales")

checkresiduals(tsVentas)


# TRAIN
nObs <- length(zVentas)
cOmit <- 4
oVentas <- window(zVentas,start=index(zVentas[1]),end=index(zVentas[nObs-cOmit]))




ets <- ets(oVentas)

fore_ets <- forecast(ets)

summary(fore_ets) #Multiplicative Holt-Winters’ method with multiplicative errors
# ERROR = ULTIPLICATIVO, TENENDCIA= ADITIVA, ESTACIONALIAD= NONE


plot(fore_ets)
lines(window(zVentas),type="o")

checkresiduals(ets)



#ARIMA

arima <- auto.arima(oVentas)
summary(arima)

fore_arima <- forecast(arima)
summary(fore_arima)


plot(fore_arima)
lines(window(zVentas),type="o")


checkresiduals(fore_arima)


# CV
fore_ets_time <- function(x, h) {
  forecast(ets(x), h = h)
}
fore_arima_time <- function(x, h) {
  forecast(auto.arima(x), h = h)
}


cv_ets <- tsCV(tsVentas, fore_ets_time, h=1)
cv_arima <- tsCV(tsVentas, fore_arima_time, h=1)

plot(cv_ets)


sqrt(mean(cv_ets^2, na.rm = T))
sqrt(mean(cv_arima^2, na.rm = T))
