#' Randomly shuffle the data
#'
#' @param x data
#'
#' @export
cv.shuffle <- function(x) x[sample(NROW(x)), ]

#' Create cross validation folds
#'
#' @param x data
#' @param nfolds no of folds
#'
#' @export
cv.folds <- function(x, nfolds) {
  cut(seq_len(NROW(x)), breaks = nfolds, labels = FALSE)
}

#' train test partitioning
#' holdout style
#' @param x data
#' @param hat.ratio estimation ratio
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


#' Mean Rank
#' According to predictive accuracy  error
#'
#' @param x list of results
#' @param metric metrics
#'
#' @export
mean_rank <- function(x, metric) {
  ranks <- rbind_(lapply(x,
                         function(j) {
                           rank(abs(j[ ,metric]))
                         })
  )
  lapply(c(mean, sd), function(stat) {
    apply(ranks, 2, stat)
  }) -> sstats

  names(sstats) <- c("avg", "dev")

  return(sstats)
}

#' Median Predictive Error
#'
#' @param x list of results
#' @param metric metrics
#'
#' @export
median_predictive_error <- function(x, metric) {
  resm <- rbind_(lapply(x,
                        function(j) {
                          j[ ,metric]
                        })
  )

  resm <- as.data.frame(lapply(as.data.frame(resm), function(j) {
    replace(j, is.infinite(j), NA)
  }))
  resm <- resm[complete.cases(resm), ]

  lapply(c(median, IQR), function(stat) {
    apply(resm, 2, stat, na.rm = TRUE)
  }) -> sstats

  names(sstats) <- c("median", "iqr")

  return(sstats)
}


#' Estimating Embedding Dimension using False Nearest Neighbors
#'
#' @param x time series
#' @param max_k maximum embedding dimension
#'
#' @export
k_hat_fnn <- function(x, max_k) {
  k_hat <- tseriesChaos::false.nearest(series = x, m = max_k, d = 10, t = 7)
  which(k_hat[2, ] == 0)[[1]]
}

#' Time Series transformation 
#' turn values to all positive; subtracts minimum and adding 1.
#' 
#' @param y vec
#' 
#' @export
as.positive <- function(y) {
  y <- as.vector(scale(y))

  min_y <- min(y)
  y - min_y + 1
}


soft.completion <- function(x) {
  require(softImpute)

  if ("data.frame" %in% class(x)) x <- as.matrix(x)
  as.data.frame(complete(x, softImpute(x)))
}

rbind_ <- function(x) do.call(rbind, x)


get_y <- function(test, form) model.response(model.frame(form, test, na.action = NULL))


embed.timeseries <- function(timeseries, embedding.dimension) {
  require(xts)
  require(zoo)
  if (missing(timeseries)) stop("Provide a time series.", call. = FALSE)

  if (!is.xts(timeseries) && class(timeseries) == "numeric") {
    timeseries <- xts(timeseries,
                      order.by = seq.Date(from = Sys.Date(),
                                          by = 1,
                                          length.out = length(timeseries)))
  }
  ts.index  <- index(timeseries)[-(1:(embedding.dimension-1))]

  ts.embed  <- embed(timeseries, dimension = embedding.dimension)
  colnames(ts.embed) <- c('target', paste0('Tm', 1:(embedding.dimension-1)))

  df <- as.data.frame(xts(ts.embed, order.by = ts.index))

  df
}
