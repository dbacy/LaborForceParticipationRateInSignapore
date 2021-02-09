library("forecast")
#https://www.gapminder.org/data/
#Signapur data
"0.765	0.77	0.775	0.781	0.787	0.793	0.796	0.799	0.797	0.8	0.805	0.809	0.808	0.814	0.817	0.821	0.826	0.83	0.846	0.848	0.851	0.856	0.86	0.861	0.869	0.875	0.876	0.877	0.878	0.879	0.88
"
#import with scan
signapur = scan()


#convertsion to time series
signapur = ts(signapur , start = 1980)

plot(signapur , ylab = "Labor Force Participation Rate 25-52")

holttrend = holt(signapur , h = 5)
summary(holttrend)
plot(holttrend)

#Phi auto generated
plot(holt(signapur , h = 15 , damped = T))
summary(holt(signapur , h = 15 , damped = T))

#manual setting of Phi 
plot(holt(signapur , h = 15 , damped = T , phi = 0.8))

# Arima auto generated
signapurarima = auto.arima(signapur)
summary(signapurarima)

plot(forecast(signapurarima , h = 5))

#exact calculation of arima parameters
auto.arima(signapur ,stepwise = F, approximation = F )

#overview plot model
holttrend = holt(signapur , h = 10)
holtdamped = holt(signapur , h = 10 , damped = T)
arimafore = forecast(auto.arima(signapur), h = 10)

library(ggplot2)
library(forecast)
# 3  Forcast Lines as Comparision

#51 is adding axis labels
#53 is adding the legend
#56 is adding the title
autoplot(signapur) +
  forecast::autolayer(holttrend$mean,
                      series = "Holt Linear Trend") +
  forecast::autolayer(holtdamped$mean, 
                      series = "Holt Damped Trend") +
  forecast::autolayer(arimafore$mean,
                      series = "ARIMA") +
  xlab("year") + ylab("Labor Force Participation Rate Age 25-54") +
  guides(color=guide_legend(title="Forecast Method")) +
  theme(legend.position = c(0.8,0.2)) +
  ggtitle("Signapur") +
  theme(plot.title = element_text(family = "Times",
                                  hjust = 0.5, color = "blue",
                                  face = "bold", size = 15))

#########################################################################
library(forecast)
library(ggplot2)
holttrend = holt(signapur , h = 10)
holtdamped = holt(signapur , h = 10 , damped = T)
arimafore = forecast(auto.arima(signapur), h = 10)

autoplot(signapur) + geom_line(size=2) +
  forecast::autoplot(holttrend$fitted,
                     series = "Holt Linear Trend", size = 1.1) +
  forecast::autolayer(holtdamped$fitted,
                      series = "Holt Damped Trend", size = 1.1) +
  forecast::autolayer(arimafore$fitted,
                      series = "ARIMA", size = 1.1) +
  xlab("year") + ylab("Labor Force Participation Rate 25-54") +
  guides(color=guide_legend(title="Forecast Method")) +
  theme(legend.position = c(0.8,0.2)) +
  ggtitle("Signapur") + theme(plot.title = element_text(family = "Times",
                                                        hjust = 0.5,
                                                        color = "blue",
                                                        face="bold",
                                                        size = 15))