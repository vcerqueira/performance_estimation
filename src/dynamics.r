replace_inf <- function(df) {
  do.call(data.frame,
          lapply(df, function(j) {
            replace(j, is.infinite(j), NA)
          })
  )
}

dynamics_ts <-
  function(x) {
    require(TTR)
    
    x_std <- as.vector(scale(x))
    
    cat("Computing trend ...\n")
    ts_trend <- trend(x)
    
    cat("Computing skewness ...\n")
    ts_skew <- moments::skewness(x)
    
    cat("Computing kurtosis ...\n")
    ts_kts <- moments::kurtosis(x)
    
    cat("Computing maximum lyapunov exponent ...\n")
    ts_mle <- max_lyapunov_exp(tail(x,500))
    
    cat("Computing hurst ...\n")
    ts_hurst <- HURST(x)
    
    cat("Computing serial correlation ...\n")
    ts_serialcorr <- tryCatch(Box.test(x)$p.val,
                              error=function(e) NA)
    
    
    ts_qt05 <- quantile(x_std, .05)
    
    ts_qt95 <- quantile(x_std, .95)
    
    ts_iqr <- IQR(x_std)
    
    ts_dyns <-
      data.frame(ts_trend = ts_trend,
                 ts_qt05=ts_qt05,
                 ts_qt95=ts_qt95,
                 ts_iqr=ts_iqr,
                 ts_skew = ts_skew,
                 ts_kts = ts_kts,
                 ts_mle = ts_mle,
                 ts_hurst = ts_hurst,
                 ts_n = length(x),
                 ts_serialcorr = ts_serialcorr)
    
    ts_dyns <- replace_inf(ts_dyns)
    
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
    require(Rwave)
    
    cwtwnoise <- DOG(x, 10, 3, 1, plot = FALSE)
    mcwtwnoise <- Mod(cwtwnoise)
    mcwtwnoise <- mcwtwnoise * mcwtwnoise
    wspwnoise <- tfmean(mcwtwnoise, plot = FALSE)
    
    hurst.est(wspwnoise, 1:7, 3, plot = FALSE)[[2]]
  }


r_sma_ema <-
  function(x) {
    require(TTR)
    
    if (length(x) > 10)
      n <- 5
    else
      n <- 3
    
    ts_sma <- SMA(rev(x), n = n)
    ts_ema <- EMA(rev(x), n = n)
    
    ts_sma <- ts_sma[!is.na(ts_sma)]
    ts_ema <- ts_ema[!is.na(ts_ema)]
    
    sema <- ts_sma / ts_ema
    sema <- sema[!(is.infinite(sema) | is.na(sema))]
    
    mean(ts_sma / ts_ema)
  }