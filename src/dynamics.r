#

describe_time_series <- 
  function(x) {
    tmcols <- grep("Tm", colnames(x))
    X <- subset(x, select = tmcols)
    
    
    n <- nrow(x)
    
    Z <- ts_dynamics(X)
    Z <- apply(Z, 2, median)
    
    k <- length(tmcols)
    
    Z <- round(c(Z, k=k, nrow=n), 3)
    
    Z
  }

describe_time_series_flat <- 
  function(x) {
    tmcols <- grep("Tm", colnames(x))
    X <- subset(x, select = tmcols)
    x <- x$Tm1
    
    n <- length(x)
    
    Z <- dynamics_flat(x)
    
    k <- length(tmcols)
    
    Z <- round(unlist(c(Z, k=k, nrow=n)), 3)
    
    Z
  }



ts_dynamics <-
  function(x) {
    require(TTR)
    
    x_std <- as.data.frame(t(apply(x,1,scale)))
    colnames(x_std) <- colnames(x)
    #ts_mean <- rowMeans(x)
    
    dim_ts <- dim(x)
    #nrow_x <- dim_ts[1]
    
    cat("Computing trend ...\n")
    ts_trend <- apply(x, 1, trend)
    
    cat("Computing skewness ...\n")
    ts_skew <- apply(x, 1, moments::skewness)
    
    cat("Computing kurtosis ...\n")
    ts_kts <- apply(x, 1, moments::kurtosis)
    
    cat("acceleration .. \n")
    ts_accl <- apply(x,1,function(y) {
      mean(EMA(y,n = 2) / EMA(y,n = 5),na.rm=TRUE)
    })
    
    cat("Computing mean ...\n")
    ts_mean <- rowMeans(x)
    
    cat("Computing median ...\n")
    ts_median <- apply(x_std, 1, median)
    
    cat("Computing min ...\n")
    ts_min <- apply(x_std, 1, min)
    
    cat("Computing max ...\n")
    ts_max <- apply(x_std, 1, max)
    
    # ts_reldiff <- apply(x, 1, function(y) {
    #   percentual_difference(y[["target"]], y[["Tm1"]]) / 100
    # })
    
    cat("Computing standard deviation ...\n")
    ts_stddev <- apply(x, 1, sd)
    
    cat("Computing maximum lyapunov exponent ...\n")
    ts_mle <- apply(x, 1, function(o) {
      tryCatch(max_lyapunov_exp(o), error = function(e) NA)
    })
    
    cat("Computing hurst ...\n")
    ts_hurst <- apply(x, 1,
                      function(o) {
                        tryCatch(tsbox::HURST(o), error = function(e) NA)
                      })
    
    cat("Computing ratio sma ema ...\n")
    ts_r_sema <- tryCatch(apply(x, 1, tsbox::r_sma_ema), error = function(e) rep(1, times=nrow(x)))
    
    cat("Computing serial correlation ...\n")
    ts_serialcorr <- apply(x, 1,
                           function(j) {
                             tryCatch(Box.test(j)$p.val, error = function(e) NA)
                           })
    
    ts_dyns <-
      data.frame(ts_trend = ts_trend,
                 ts_skew = ts_skew,
                 ts_kts = ts_kts,
                 ts_median = ts_median,
                 ts_min = ts_min,
                 ts_max = ts_max,
                 ts_accl = ts_accl,
                 #ts_reldiff = ts_reldiff,
                 ts_mean = ts_mean,
                 ts_stddev = ts_stddev,
                 ts_mle = ts_mle,
                 ts_hurst = ts_hurst,
                 ts_r_sema = ts_r_sema,
                 #ts_selfsim = ts_selfsim,
                 ts_serialcorr = ts_serialcorr)
    
    ts_dyns <- tsbox::replace_inf(ts_dyns)
    
    #has_na <- DMwR::manyNAs(t(ts_dyns), .4)
    
    #if (length(has_na) > 0) {
    #  ts_dyns <- subset(ts_dyns, select = -has_na)
    #}
    
    ts_dyns <- tsensembler::soft.completion(ts_dyns)
    
    #nzv_cols <- caret::nearZeroVar(ts_dyns)
    #if (length(nzv_cols) > 0L) {
    #  ts_dyns <- subset(ts_dyns, select = -nzv_cols)
    #}
    
    rownames(ts_dyns) <- NULL
    #preproc <- caret::preProcess(dStats)
    #dStats <- predict(preproc, dStats)
    
    dplyr::as_tibble(ts_dyns)
  }


dynamics_flat <-
  function(x) {
    require(TTR)
    
    x_std <- as.vector(scale(x))
    
    
    cat("Computing trend ...\n")
    ts_trend <- trend(x)
    
    cat("Computing skewness ...\n")
    ts_skew <- moments::skewness(x)
    
    cat("Computing kurtosis ...\n")
    ts_kts <- moments::kurtosis(x)
    
    cat("acceleration .. \n")
    ts_accl <- mean(EMA(x,n = 5) / EMA(x,n = 15),na.rm=TRUE)
    
    cat("Computing mean ...\n")
    ts_mean <- mean(x)
    
    cat("Computing median ...\n")
    ts_median <- median(x)
    
    cat("Computing min ...\n")
    ts_min <- min(x_std)
    
    cat("Computing max ...\n")
    ts_max <- max(x_std)
    
    cat("Computing standard deviation ...\n")
    ts_stddev <- sd(x)
    
    cat("Computing maximum lyapunov exponent ...\n")
    ts_mle <- max_lyapunov_exp(tail(x,500))
    
    cat("Computing hurst ...\n")
    ts_hurst <- tsbox::HURST(x)
    
    cat("Computing ratio sma ema ...\n")
    ts_r_sema <- tsbox::r_sma_ema(x)
    
    cat("Computing serial correlation ...\n")
    ts_serialcorr <- tryCatch(Box.test(x)$p.val,
                              error=function(e) NA)
    
    ts_qt1 <- quantile(x_std, .1)
    
    ts_qt9 <- quantile(x_std, .9)
    
    ts_qt05 <- quantile(x_std, .05)
    
    ts_qt95 <- quantile(x_std, .95)
    
    ts_iqr <- IQR(x_std)
    
    ts_dyns <-
      data.frame(ts_trend = ts_trend,
                 ts_qt1=ts_qt1,
                 ts_qt9=ts_qt9,
                 ts_qt05=ts_qt05,
                 ts_qt95=ts_qt95,
                 ts_iqr=ts_iqr,
                 ts_skew = ts_skew,
                 ts_kts = ts_kts,
                 ts_median = ts_median,
                 ts_min = ts_min,
                 ts_max = ts_max,
                 ts_accl = ts_accl,
                 #ts_reldiff = ts_reldiff,
                 ts_mean = ts_mean,
                 ts_stddev = ts_stddev,
                 ts_mle = ts_mle,
                 ts_hurst = ts_hurst,
                 ts_r_sema = ts_r_sema,
                 #ts_selfsim = ts_selfsim,
                 ts_serialcorr = ts_serialcorr)
    
    ts_dyns <- tsbox::replace_inf(ts_dyns)
    
    #has_na <- DMwR::manyNAs(t(ts_dyns), .4)
    
    #if (length(has_na) > 0) {
    #  ts_dyns <- subset(ts_dyns, select = -has_na)
    #}
    
    #ts_dyns <- tsensembler::soft.completion(ts_dyns)
    
    #nzv_cols <- caret::nearZeroVar(ts_dyns)
    #if (length(nzv_cols) > 0L) {
    #  ts_dyns <- subset(ts_dyns, select = -nzv_cols)
    #}
    
    rownames(ts_dyns) <- NULL
    #preproc <- caret::preProcess(dStats)
    #dStats <- predict(preproc, dStats)
    
    dplyr::as_tibble(ts_dyns)
  }


trend <-
  function(x) {
    sd(x) / sd(diff(x)[-1])
  }

max_lyapunov_exp <-
  function(x) {
    require(nonlinearTseries)
    
    len <- length(x)
    Reduce(max,
           nonlinearTseries::divergence(
             nonlinearTseries::maxLyapunov(
               time.series = x,
               min.embedding.dim = ceiling(len / 4),
               max.embedding.dim = ceiling(len / 2),
               radius = ceiling(len / 6),
               do.plot = FALSE
             )
           ))
  }

#' Hurst exponent
#'
#' @param x numeric vector
HURST <-
  function(x) {
    #require(Rwave)
    
    cwtwnoise <- DOG(x, 10, 3, 1, plot = FALSE)
    mcwtwnoise <- Mod(cwtwnoise)
    mcwtwnoise <- mcwtwnoise * mcwtwnoise
    wspwnoise <- tfmean(mcwtwnoise, plot = FALSE)
    
    hurst.est(wspwnoise, 1:7, 3, plot = FALSE)[[2]]
  }