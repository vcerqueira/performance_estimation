#' Predictive Accuracy Error
#'
#' @param E_hat estimated error
#' @param E true error
#'
#' @export
pae <- function(E_hat, E) E_hat - E

#' Mean Predictive Accuracy Error
#'
#' @inheritParams pae
#'
#' @export
mpae <- function(E_hat, E) mean(PAE(E_hat, E))

#' Mean Absolute Predictive Accuracy Error
#'
#' @inheritParams pae
#'
#' @export
mapae <- function(E_hat, E) mean(abs(PAE(E_hat, E)))

#' regression metrics
#' 
#' @inheritParams mape
#' 
#' @export
regMetrics <- function(trues, preds) {
  m1 <- mape(trues, preds)
  m2 <- rmse(trues, preds)
  m3 <- mae(trues, preds)
  m4 <- mase(preds, trues)
  m5 <- smape(preds, trues)

  c(mape = m1, rmse = m2, mae = m3, mase= m4, smape=m5)
}


#' mape
#'
#' @param trues trues
#' @param preds preds
#' @export
mape <- function(trues, preds) {
  res <- (trues - preds) / trues
  res <- res[is.finite(res)]
  c(mean(abs(100 * res)))
}

#' Symmetric Mean Absolute Percentage Error
#'
#' @param y_hat preds
#' @param y trues
#'
#' @export
smape <- function(y_hat, y) {
  (100 / length(y)) * sum(abs(y_hat - y) / ((abs(y_hat) + abs(y)) / 2))
}

#' Mean absolute Scaled Error
#'
#' @param y_hat preds
#' @param y trues
#'
#' @export
mase <- function(y_hat, y) {
  len <- length(y)
  baseline <- c(NA_real_, y[-length(y)])
  den <- (len / (len-1)) * sum(ae(y, baseline), na.rm = TRUE)
  sum(abs(y - y_hat)) / den
}

#' rmse
#'
#' Utility function to compute Root Mean Squared Error (RMSE)
#'
#' @inheritParams se
#'
#' @export
rmse <- function(y, y_hat) sqrt(mse(y, y_hat))

#' ae
#'
#' Element-wise computation of the absolute error loss function.
#'
#' @inheritParams se
#'
#' @export
ae <- function(y, y_hat) {
  stopifnot(length(y) == length(y_hat),
            is.numeric(y),
            is.numeric(y_hat))

  abs(y - y_hat)
}

#' se
#'
#' Utility function to compute pointwise squared error (SE)
#'
#' @param y A numeric vector representing the actual values.
#' @param y_hat A numeric vector representing the forecasted values.
#'
#' @return squared error of forecasted values.
#'
#' @export
se <- function(y, y_hat) {
  stopifnot(length(y) == length(y_hat),
            is.numeric(y),
            is.numeric(y_hat))

  (y - y_hat) ^ 2
}

#' mse
#'
#' Utility function to compute mean squared error (MSE)
#'
#' @inheritParams se
#'
#' @export
mse <- function(y, y_hat) mean(se(y, y_hat), na.rm = TRUE)

#' mae
#'
#' Mean Absolute Error loss function.
#'
#' @inheritParams se
#'
#' @export
mae <- function(y, y_hat) mean(ae(y, y_hat), na.rm = TRUE)


getRes.pe <- function(x) {
  itersInf <- performanceEstimation::getIterationsInfo(x)
  err.metrics <- rbind_(
    lapply(itersInf, function(i) {
      preds <- i[["preds"]]
      trues <- i[["trues"]]

      regMetrics(trues, preds)
    })
  )
  
  err.metrics
}
