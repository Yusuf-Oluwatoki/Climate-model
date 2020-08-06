library(extRemes)
library(plyr)
library(ggplot2)
library(readr)



Ghana <- read_csv("~/Ghana.csv", col_types = 
                  cols(Date = col_date(format = "%m/%d/%Y")))

Ghana$station <- factor(Ghana$station)
Ghana$month_abbr <- factor(Ghana$month_abbr, levels = c('Jan', 'Feb', 
        'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))
Ghana$year <- factor(Ghana$year)




#Total precipitation for the two station
ggplot2::ggplot(data=Ghana, mapping=ggplot2::aes(y=total_prec, x=month_abbr)) + 
  ggplot2::geom_boxplot(outlier.colour="red") + 
  ggplot2::facet_wrap(facets = ~station, dir = 'v')
summary(Ghana)

#max temperature for the two station
ggplot2::ggplot(data=Ghana, mapping=ggplot2::aes(y=max_temp, x=month_abbr)) + 
  ggplot2::geom_boxplot(outlier.colour="red") +
  ggplot2::facet_wrap(facets = ~station, dir = 'v')



#Axim station 
Axim_all <- dplyr::filter(Ghana, station == "Axim")

ggplot2::ggplot(data=Axim_all, mapping=ggplot2::aes(y=total_prec, x=year)) + 
  ggplot2::geom_boxplot(outlier.colour="brown") +
  ggplot2::facet_wrap(facets = ~month_abbr)


ggplot2::ggplot(data=Axim_all, mapping=ggplot2::aes(y=total_prec, x=month_abbr)) + 
  ggplot2::geom_boxplot(outlier.colour="brown") + 
  ggplot2::facet_wrap(facets = ~station, dir = 'v')

#95th percentile for Axim rainfall
quantile(Axim_all$total_prec, .95)
summary(Axim_all)





#Navrongo station
Navrongo_all <- dplyr::filter(Ghana, station == "Navrongo")

ggplot2::ggplot(data=Navrongo_all, mapping=ggplot2::aes(y=max_temp, x=year)) + 
  ggplot2::geom_boxplot(outlier.colour="blue") +
  ggplot2::facet_wrap(facets = ~month_abbr)


ggplot2::ggplot(data=Navrongo_all, mapping=ggplot2::aes(y=max_temp, x=month_abbr)) + 
  ggplot2::geom_boxplot(outlier.colour="blue") + 
  ggplot2::facet_wrap(facets = ~station, dir = 'v')

#95th percentile for Navrongo temperature
quantile(Navrongo_all$max_temp, .95)
summary(Navrongo_all)

#BmAxim_all <- blockmaxxer(Axim_all, blocks = c(Axim_all$station,Axim_all$year), which="total_prec")
  
#BmGhana <- blockmaxxer(Ghana, blocks = c(Ghana$station,Ghana$year), which="total_prec")


#Ghana annual max_rainfall 
Ghana_rain=ddply(Ghana,~station+year,summarise,max_rainfall=max(total_prec))
names(Ghana_rain)

#Axim max rainfall
Axim<-Ghana_rain[Ghana_rain$station=='Axim',]

plot(Axim_all$year, Axim_all$total_prec, xlab = "Year", 
     ylab = "Precipitation (mm)", 
     cex = 1.25, cex.lab = 1.25, 
     col = "green", bg = "lightblue", pch = 21) 

points(Axim$year, Axim$max_rainfall, col="red", cex=1.5) 
abline(h=15.54246, col="purple")

summary(Axim)


#Ghana annual max_temperature
Ghana_temp=ddply(Ghana,~station+year,summarise,temp=max(max_temp))
names(Ghana_temp)

#Navrongo max temp
Navrongo<-Ghana_temp[Ghana_temp$station=='Navrongo',]

plot(Navrongo_all$year, Navrongo_all$max_temp, xlab = "Year", 
     ylab = "temperature (celcius)", 
     cex = 1.25, cex.lab = 1.25, 
     col = "green", bg = "lightblue", pch = 15) 

points(Navrongo$year, Navrongo$temp, col="red", cex=1.5) 
abline(h=39.4, col="purple")


summary(Navrongo)



#ggplot2::ggplot(data=Axim, mapping=ggplot2::aes(y=max_rainfall, x=year))+
  ggplot2::geom_point() 
# Fit a GEV distribution to annual maximum Precipitation
# in Ghana Collins, Colorado, U.S.A.

#attach(what=Axim)

#last_model <- extRemes::fevd(max_rainfall)
#last_model

#dettach(what=Axim) 
  
# Test for stationarity
plot(Axim_all$total_prec,type='l',xlab='Day Number',
            ylab='Log-Daily Return')
plot(Navrongo_all$max_temp,type='l',xlab='Day Number',
             ylab='Log-Daily Return')


  
#GP model on Axim rainfall 

#mean residual plot
mrlplot(Axim_all$total_prec)

#Selecting threshold
length(Axim_all$total_prec[Axim_all$total_prec>18])

# Select a threshold for daily data from above.
threshrange.plot(Axim_all$total_prec, c(12, 24), type = "GP")

# Fit the PP model to the daily data from above.
fitPP <- fevd(Axim_all$total_prec, data = Axim_all, threshold = 20, 
              type = "GP", units = "mm")
fitPP
plot(fitPP)

atdf(Axim_all$total_prec, 0.05)

extremalindex(Axim_all$total_prec, 0.05, blocks=Axim_all$year)

dcGhana <- decluster(Axim_all$total_prec, 0.05, r = 9)
plot(dcGhana)



#GEV model on Navrongo temperature

fitGEV <- fevd(Navrongo_all$max_temp, data = Navrongo_all)
fitGEV
plot(fitGEV)
plot(fitGEV, "trace")

