require(dplyr)
require(xts)
require(zoo)
require(tidyr)
require(forecast)
require(ggfortify)
require(ggplot2)


############### Forecast Methods ####################

##Seasonal Naive with Growth. Edited lagwalk function form Forecast Package
## Edited the lagwalk method from Forecast Package by Rob Hyndman
snaive_with_growth <- function(y, lag, h=2*frequency(x), growth, 
                               level=c(80,95), fan=FALSE, lambda=NULL, biasadj=FALSE, bootstrap=FALSE, npaths=5000)
{
  n <- length(y)
  m <- frequency(y)
  nn <- 1:h
  if(!is.ts(y))
    y <- ts(y)
  if(!is.null(lambda))
  {
    origy <- y
    y <- BoxCox(y,lambda)
  }
  
  b <- b.se <- 0
  s <- sd(diff(y, lag=lag),na.rm=TRUE)
  method <- "Seasonal Naive with Growth"
  
  fits <- ts((c(rep(NA,lag),head(y,-lag)) + b)*growth, start=tsp(y)[1], frequency=m)
  res <- y - fits
  fullperiods <- trunc((h-1)/lag) + 1
  if(lag==1)
    steps <- seq_len(h)
  else
    steps <- rep(1:fullperiods, rep(lag, fullperiods))[1:h]
  f <- rep(tail(y*growth,lag), fullperiods)[1:h] + steps*b
  mse <- mean(res^2, na.rm=TRUE)
  se  <- sqrt(mse*steps  + (steps*b.se)^2)
  
  if(fan)
    level <- seq(51,99,by=3)
  else
  {
    if(min(level) > 0 & max(level) < 1)
      level <- 100*level
    else if(min(level) < 0 | max(level) > 99.99)
      stop("Confidence limit out of range")
  }
  nconf <- length(level)
  if(bootstrap)
  {
    e <- na.omit(res) - mean(res, na.rm=TRUE)
    sim <- matrix(NA_real_, ncol=npaths, nrow=h)
    for(i in seq_len(npaths))
    {
      estar <- matrix(sample(e, size=lag*fullperiods, replace=TRUE), nrow=fullperiods)
      estar <- c(t(apply(estar, 2, cumsum)))
      sim[, i] <- f + estar[1L:h]
    }
    lower <- t(apply(sim, 1, quantile, prob=.5-level/200))
    upper <- t(apply(sim, 1, quantile, prob=.5+level/200))
  }
  else
  {
    lower <- upper <- matrix(NA_real_,nrow=h,ncol=nconf)
    z <- qnorm(.5 + level/200)
    for(i in 1:nconf)
    {
      lower[,i] <- f - z[i]*se
      upper[,i] <- f + z[i]*se
    }
  }
  lower <- ts(lower, start=tsp(y)[2]+1/m, frequency=m)
  upper <- ts(upper, start=tsp(y)[2]+1/m, frequency=m)
  colnames(lower) <- colnames(upper) <- paste(level,"%",sep="")
  fcast <- ts(f, start=tsp(y)[2]+1/m, frequency=m)
  if(!is.null(lambda))
  {
    y <- origy
    fcast <- InvBoxCox(fcast, lambda, biasadj, list(level = level, upper = upper, lower = lower))
    fits <- InvBoxCox(fits,lambda)
    upper <- InvBoxCox(upper,lambda)
    lower <- InvBoxCox(lower,lambda)
  }
  
  out <- list(method=method,level=level,x=y,mean=fcast,lower=lower,upper=upper,
              model=structure(list(sd=s),class='naive'),
              fitted = fits, residuals = res, lambda=lambda)
  out$model$call <- match.call()
  
  return(structure(out,class="forecast"))
}

calc_decompose_ETS <-  function(ts_series) {
  if (frequency(ts_series) < 2 || length(ts_series) <= 2 * frequency(ts_series)) {
    fit = ets(ts_series)
  } else {
    fit = stlm(ts_series, method = 'ets')
  }
  return (fit)
}

calc_decompose_ARIMA <-  function(ts_series) {
  if (frequency(ts_series) < 2 || length(ts_series) <= 2 * frequency(ts_series)) {
    fit = auto.arima(ts_series)
  } else {
    fit = stlm(ts_series, method = 'arima')
  }
  return (fit)
}

calc_nnet <-  function(ts_series) {
  if (frequency(ts_series) < 2 || length(ts_series) <= 2 * frequency(ts_series)) {
  
      if(length(ts_series)<3){
        fit = rwf(ts_series, h=12,drift=TRUE)
      } else{
        fit = auto.arima(ts_series)
      }
  } else{
    fit = nnetar(ts_series)  
    }
  return (fit)
}

calc_theta <- function(ts_series){
  m = frequency(ts_series)
  n=length(ts_series)
  r <- as.numeric(acf(ts_series, lag.max = m, plot = FALSE)$acf)[-1]
  stat <- sqrt((1 + 2 * sum(r[-m]^2))/n)
  seasonal <- (abs(r[m])/stat > qnorm(0.95))
  if (is.na(seasonal)){
    fit = rwf(ts_series, h=12,drift=TRUE)
  } else{
    fit = thetaf((ts_series))
  }
  return (fit)
}

calc_decompose_nnet <-  function(ts_series) {
  if (frequency(ts_series) < 2 || length(ts_series) <= 2 * frequency(ts_series)) {
    fit = calc_nnet(ts_series)
  } else {
    fit = stlm(ts_series, modelfunction = nnetar)
  }
  return (fit)
}

calc_decompose_theta <-  function(ts_series) {
  if (frequency(ts_series) < 2 || length(ts_series) <= 2 * frequency(ts_series)) {
    fit = calc_theta(ts_series)
  } else {
    fit = stlm(ts_series, modelfunction = calc_theta)
  }
  return (fit)
}

##### Utility method to fortify nnetar model forecast as they are not supported by regular fortify
fortify_nnet <- function(model) {
  tsp <- attr(model$mean, which = "tsp")
  dtindex <- time(model$mean)
  if (any(tsp[3] == c(4, 12))){ 
    dtindex <- zoo::as.Date.yearmon(dtindex)
  }
  forecasted <- data.frame('Point Forecast'=model$mean, Index = dtindex,check.names = FALSE)
  d <- ggplot2::fortify(model$x, is.date = NULL)
  fitted <- ggplot2::fortify(model$fitted, is.date = NULL)
  d <- dplyr::left_join(d, fitted %>% rename('Fitted'=Data), by = "Index")
  d <- ggfortify::rbind_ts(forecasted, d, ts.connect = FALSE)
  return (as.data.frame(d))
}


##### Calculates AG level Forecast for each AG
do_forecast_level <- function(df,forecast_level, start_date, current_date,method='auto'){
  ###
  ### This function expects the data to be in specified format
  ### It should be monthly data with the Month beginning dates named 'MonthDate'
  ### The items which should be forecasted should be named 'ForecastLevel'
  ### The Quantity to forecast should be named 'Qty'
  ### If Seasonal Naive method should be used the Last Year quantity should be provided under the name, 'LY.Qty'
  ### method = c('auto','snaive_growth','ets','arima', 'auto','seasonal_ets','seasonal_arima','nnet','bats','tbats','theta')
  ###

  #If forecast_level is not provided, it is assumed that the first entry in the df in the forecast_level column is the identifier for forecast level.
  if(missing(forecast_level)){
    forecast_level = head(df$forecast_level,n=1)
  }
  # If start and end date is not given, the earliest and latest date in the column MonthDate is selected
  if (missing(start_date)){
    start_date = min(as.Date(df$MonthDate),na.rm = TRUE)
  }
  if (missing(current_date)){
    current_date = max(as.Date(df$MonthDate),na.rm = TRUE)
  }

  # Creating a timeseries object
  xts_ts <- as.xts(select(df,Qty),order.by = as.POSIXct(df$MonthDate) ,frequency = 12)
  # calculating number of months between start and end date
  no_of_months=((as.yearmon(current_date, format="%Y/%m/%d")-as.yearmon(start_date,format="%Y/%m/%d"))*12)+1
  # Creating an ideal seq and merging with the time series to deal with missing months if any
  forecast_seq = seq(from=min(as.Date(df$MonthDate),na.rm = TRUE),to=max(as.Date(df$MonthDate),na.rm = TRUE) , by = "month")
  xts_ts%>% 
    merge.xts(as.xts(forecast_seq, "month"), fill = 0)-> merged_ts
  #Creating a native R timeseries object which works best with the forecast package  
  start_vector = c(year(min(as.Date(df$MonthDate),na.rm = TRUE)),month(min(as.Date(df$MonthDate),na.rm = TRUE)))
  ts_series <- ts(data = merged_ts$Qty, start = start_vector, frequency=12)
  
  #Calculating Growth for the Seasonal Naive Growth method
  if (method == 'snaive_growth'){
    if (!(length(merged_ts)<12)){
      ideal_seq <- seq(start_date, by = "month", length.out = no_of_months)
      xts_ts%>% 
        merge.xts(as.xts(ideal_seq, "month"), fill = 0) %>% 
        merge.zoo(lag.xts(merged_ts,k = 12), suffixes = c('CY','LY'), fill=0)%>%
        rollapply(FUN = sum, width = 6, align = "right")%>%
        tail(n=1)%>%
        as.data.frame()-> growth_calc
      
      growth = growth_calc$Qty/growth_calc$LY.Qty
    } else {
      growth = NA
    }
  }

  ### method = c('auto','snaive_growth','ets','arima','seasonal_ets','seasonal_arima','nnet','theta','seasonal_nnet','seasonal_theta','baggedETS)
  if (method == 'auto'){
    print('To be implemented')
  } else {
    #If there is less than 4 months of data, applying Random Walk with Drift
    if (length(ts_series)<4){
      fit = rwf(ts_series,h = 12,drift = TRUE)
    } else{
      # Switching the method based on the arg passed
      fit = switch(method,
                   snaive_growth = snaive_with_growth(ts_series,lag=12,h=12, growth = growth),
                   ets = ets(ts_series),
                   arima = auto.arima(ts_series),
                   seasonal_ets = calc_decompose_ETS(ts_series),
                   seasonal_arima = calc_decompose_ARIMA(ts_series),
                   seasonal_nnet = calc_decompose_nnet(ts_series),
                   seasonal_theta = calc_decompose_theta(ts_series),
                   nnet = calc_nnet(ts_series),
                   theta = calc_theta(ts_series),
                   baggedETS = baggedETS(ts_series),
                   
                   stop('Method unrecognized')
      )
    }
    
  }
  #Calculating acciracy and forecast based on the fit
  accuracy = accuracy(fit)
  f_fit = forecast(fit,h=12)
  #the fortify method in the forecast package does not work well for the Neural Net. Therefore custom method called for that purpose.
  if (grepl('NNAR', f_fit$method, fixed=TRUE)){ 
    forecast_df = fortify_nnet(f_fit)
  } else{
    forecast_df = fortify(f_fit)
  }
  #Combining the informataion into a master dataframe
  master_df = cbind(forecast_level,forecast_df,accuracy,method = f_fit$method, method_arg = method)
  nms <- c("forecast_level","Index","Data","Fitted","Point Forecast","Lo 80","Hi 80","Lo 95","Hi 95","ME","RMSE","MAE","MPE","MAPE","MASE","ACF1","method", "method_arg")   # Vector of columns you want in this data.frame
  #Adding missing rows if any and fill them with zero
  Missing <- setdiff(nms, names(master_df))  # Find names of missing columns
  master_df[Missing] <- 0                    # Add them, filled with '0's
  return (master_df)
}

