load("data/tsl_uni_90_mix.rdata")

len <- sapply(ts_list, length)

save(len, file = "series_length.rdata")

stationarity_test_df <- 
  function(x) {
    cat(".")
    library(forecast)
    library(urca)
    
    nd <- ndiffs(x)
    if (nd>0) {
      x <- diff(x,differences=nd)
    }
    
    x <- as.vector(x)
    
    xtest <- ur.df(x,type="none",selectlags="AIC",lags=10)
    
    tr <- unname(xtest@teststat < min(xtest@cval))[1,1]
    
    st <- ifelse(tr, F, T)
    
    st
  }

stationarity_test_wave <- 
  function(x) {
    library(locits)
    
    nd <- ndiffs(x)
    if (nd>0) {
      x <- diff(x,differences=nd)
    }
    
    xp <- trunc_timeseries(x)
    
    xtest <- hwtos2(as.vector(xp))
    
    nrej <- xtest$nreject
    
    st <- ifelse(nrej > 0, F, T)
    
    st
  }


trunc_timeseries <-
  function(x) {
    max_power <- 4096
    seq_powers <- c(max_power,
                    max_power / 2,
                    max_power / 4, 
                    max_power / 8,
                    max_power / 16)
    
    len <- length(x)
    x <- as.vector(x)
    dim_trc <- seq_powers[which(len - seq_powers + 1 > 0)[1]]
    
    truncd_ts <- x[(len - dim_trc + 1):len]
    
    truncd_ts
  }


is_st_wave <- sapply(ts_list, stationarity_test_wave)
is_st_df <- sapply(ts_list, stationarity_test_df)
is_stationary_2ensemble <- is_st_wave & is_st_df

table(is_st_wave)
table(is_st_df)
table(is_stationary_2ensemble)

save(is_st_wave,
     is_st_df,
     is_stationary_2ensemble, 
     file = "stationarity_tsdl.rdata")

