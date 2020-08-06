## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----nile, fig.width=6, fig.height=6, message=FALSE, warning=FALSE------------
library(tidyverse)
library(forecast)

data(NileMin,package='longmemo')
# we'll just set the correct start year, for display purposes.
NileMin<-ts(as.numeric(NileMin),start=622,frequency=1)

ggtsdisplay(NileMin,lag.max=350, main='NileMin time series.', theme=theme_bw())

## ----soi, fig.width=6, fig.height=4-------------------------------------------
soi <-
  read_fwf('soi.long.data',fwf_widths(c(5, rep(7,12)), c("year", 1:12)),col_types='nnnnnnnnnnnnn') %>%
  gather(mth_no,soi,-year) %>%
  mutate(mth = lubridate::make_date(year,as.numeric(mth_no),1)) %>%
  select(mth,soi) %>%
  arrange(mth)

soi_ts <- ts(soi$soi,start=c(1951,2),frequency=12)

ggtsdisplay(soi_ts, lag.max=400, main='Southern Oscillation Index', theme=theme_bw())

## ----spectrum, fig.width=7, fig.height=4--------------------------------------
spectrum_nilemin <- spectrum(NileMin, plot=FALSE)
spectrum_soi     <- spectrum(soi_ts,  plot=FALSE)

# now munge these lists together into a single dataframe.
spec_df <- rbind(data.frame(freq=spectrum_nilemin$freq,
                            spec=spectrum_nilemin$spec, 
                            process='NileMin'),
                 data.frame(freq=spectrum_soi$freq,     
                            spec=spectrum_soi$spec,     
                            process='SOI'))

# and plot
ggplot(spec_df, aes(x=freq,y=spec)) +
  geom_line() + 
  facet_wrap(.~process,scales='free_y') +
  ggtitle('Spectrum of NileMin and SOI') + 
  ylab('Intensity') + 
  xlab(bquote('Frequency (0 -' ~ pi ~')' )) + xlim(0,pi) +
  theme_bw()

## ----arima--------------------------------------------------------------------
library(garma)
data(AirPassengers)

ap  <- as.numeric(diff(AirPassengers,12))

# Arima model
arima_mdl <- arima(ap,order=c(9,1,0))
summary(arima_mdl)

# GARMA model
# Note in the below we specify k=0.
# This tells the routine is not to fit a Gegenbauer/GARMA model.
garma_mdl <- garma(ap,order=c(9,1,0),k=0)
summary(garma_mdl)


## ----sunspot1-----------------------------------------------------------------
library(garma)

data(sunspot.year)

# Next we subset the data to ensure we are using the years used in the literature.
sunspots <- ts(sunspot.year[49:224],start=1749,end=1924)

# Now as in Gray et al 1989 we fit a GARMA(1,0) model:
sunspots_garma_mdl <- garma(sunspots, order=c(1,0,0),k=1,method='CSS')

summary(sunspots_garma_mdl)


## ----sunspot2, fig.width=7, fig.height=5--------------------------------------
# fit a GARMA(8,0,0) model
sunspots_garma_mdl <- garma(sunspots, order=c(8,0,0),k=1,method='CSS')
summary(sunspots_garma_mdl)

#prepare 'future' actuals data for plotting
future_df <- data.frame(yr=1925:1935, sunspots=sunspot.year[225:235],grp='Future Actuals')

ggplot(sunspots_garma_mdl, h=11) +
  geom_line(data=future_df,aes(x=yr,y=sunspots)) +
  ggtitle('Sunspot Forecast using GARMA(8,0)')

