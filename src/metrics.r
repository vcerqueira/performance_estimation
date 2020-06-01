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
mpae <- function(E_hat, E) mean(pae(E_hat, E))

#' Mean Absolute Predictive Accuracy Error
#'
#' @inheritParams pae
#'
#' @export
mapae <- function(E_hat, E) mean(abs(pae(E_hat, E)))

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



mase_cal <- function(insample, outsample, forecasts, avg=TRUE){
  frq <- 1#frequency(insample)
  forecastsNaiveSD <- rep(NA,frq)
  for (j in (frq+1):length(insample)){
    forecastsNaiveSD <- c(forecastsNaiveSD, insample[j-frq])
  }
  masep<-mean(abs(insample-forecastsNaiveSD),na.rm = TRUE)
  
  outsample <- as.numeric(outsample) ; forecasts <- as.numeric(forecasts)
  
  if (avg) {
    mase <- mean((abs(outsample-forecasts)))/masep
  } else {
    mase <- (abs(outsample-forecasts))/masep
  }
  
  return(mase)
}
