#' Randomly shuffle the data
#'
#' @param x data
#'
#' @export
cv.shuffle <- function(x) x[sample(NROW(x)), ]

resample_timeseries <-
  function (x,
            resamples,
            size_estimation,
            size_validation) {
    n <- nrow(x)
    tr_size <- as.integer(n * size_estimation)
    ts_size <- as.integer(n * size_validation)
    selection_range <- (tr_size + 1):(n - ts_size + 1)
    origins <- sample(selection_range, resamples)
    
    lapply(origins, function(o) {
      list(train = x[(o - tr_size):(o - 1), ], test = x[o:(o + ts_size - 1), ])
    })
  }

#' Create cross validation folds
#'
#' @param x data
#' @param nfolds no of folds
#'
#' @export
cv.folds <- function(x, nfolds) {
  cut(seq_len(NROW(x)), breaks = nfolds, labels = FALSE)
}

#' train test partitioning holdout style
#' 
#' @param x data
#' @param hat.ratio estimation ratio
#' 
#' @export
partition <- function(x, hat.ratio) {
  len <- NROW(x)

  if (class(x)[1] == "data.frame") {
    train <- x[ seq_len(hat.ratio * len),]
    test <-  x[-seq_len(hat.ratio * len),]
  } else {
    train <- x[ seq_len(hat.ratio * len)]
    test <-  x[-seq_len(hat.ratio * len)]
  }
  list(train = train, test = test)
}


rbind_l <- function(x) do.call(rbind, x)


get_y <- function(test, form) model.response(model.frame(form, test, na.action = NULL))


roll_mean_matrix <-
  function(x, lambda) {
    require(RcppRoll)
    if (class(x) != "data.frame")
      stop("x obj class in roll_mean_matrix must be a data.frame")
    dim1 <- NROW(x)
    
    MASE <-
      lapply(x,
             function(z) {
               rollm <- RcppRoll::roll_mean(z, n = lambda)
               blanks <- dim1 - length(rollm)
               aux <- numeric(blanks)
               for (y in seq_along(aux)) {
                 aux[y] <- RcppRoll::roll_mean(z, n = y)[1]
               }
               c(aux, rollm)
             })
    as.data.frame(MASE)
  }


as_positive <- 
  function(y) {
    y - min(y) + 1
  }


estimate_k <-
  function(x, m.max=20,tol=.15) {
    require(tseriesChaos)
    
    fn.out <- false.nearest(x, m.max, d=1, t=1)
    fn.out <- round(fn.out,4)
    fn.out[is.na(fn.out)] <- 0
    #plot(fn.out)
    
    fnp.tol <- fn.out["fraction",] > tol
    fnp.tol.sum <- sum(fnp.tol)
    
    m <- ifelse(fnp.tol.sum < m.max,fnp.tol.sum + 1, m.max)
    
    m
  }

