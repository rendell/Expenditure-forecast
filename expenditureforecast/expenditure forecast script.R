#Tourism expenditure forecast

library(forecast)

data=read.csv("forecastdata.csv")
totalvis <- ts(data$Total, start=c(1996, 1), end=c(2016, 4), frequency=4) 
usavis <- ts(data$USA, start=c(1996,1), end=c(2016,4), frequency=4)
venvis <- ts(data$VEN, start=c(1996,1), end=c(2016,4), frequency=4)
othervis <- ts(data$OTH, start=c(1996,1), end=c(2016,4), frequency=4)

nights_tot <- ts(data$NIGHTS_TOT, start=c(1996,1), end=c(2016,4), frequency=4)
nights_usa <- ts(data$NIGHTS_USA, start=c(1996,1), end=c(2016,4), frequency=4)
nights_ven <- ts(data$NIGHTS_VEN, start=c(1996,1), end=c(2016,4), frequency=4)
nights_oth <- ts(data$NIGHTS_OTH, start=c(1996,1), end=c(2016,4), frequency=4)

tourexp_tot <- ts(data$TOUREXP_TOT, start=c(1996,1), end=c(2016,4), frequency=4)
tourrec_tot <- ts(data$TOURREC_TOT, start=c(1996,1), end=c(2016,4), frequency=4)
tourexp_usa <- ts(data$TOUREXP_USA, start=c(1996,1), end=c(2016,4), frequency=4)
tourexp_ven <- ts(data$NIGHTS_VEN, start=c(1996,1), end=c(2016,4), frequency=4)
tourexp_oth <- ts(data$NIGHTS_OTH, start=c(1996,1), end=c(2016,4), frequency=4)

ade_tot <- ts(data$ADE_TOT, start=c(1996,1), end=c(2016,4), frequency=4)
ade_usa <- ts(data$ADE_USA, start=c(1996,1), end=c(2016,4), frequency=4)
ade_ven <- ts(data$ADE_VEN, start=c(1996,1), end=c(2016,4), frequency=4)
ade_oth <- ts(data$ADE_OTH, start=c(1996,1), end=c(2016,4), frequency=4)





plot(totalvis)
plot(usavis)
plot(othervis)
plot(nights_tot)
plot(nights_usa)
plot(nights_ven)
plot(nights_oth)

plot(tourexp_tot)
plot(tourrec_tot)
plot(tourexp_usa)
plot(tourexp_ven)
plot(tourexp_oth)
plot(ade_tot)
plot(ade_usa)
plot(ade_ven)
plot(ade_oth)

tourrec_nights <- tourrec_tot / nights_tot
plot(tourrec_nights)

tourexp_tot_nights <- tourexp_tot / nights_tot
plot(forecast_tourexp_tot_night)

forecast_tourexp_tot_nights <- auto.arima(tourexp_tot_nights)
plot(forecast(forecast_tourexp_tot_nights, h=8))
