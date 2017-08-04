#' performance estimation using the estimation set
#'
#' @param train estimation embedded time series
#' @param FUN learning function to apply in training/testing sets
#' @param wf learning workflow to apply in training/testing sets
#'
#' @export
performance_estimation <- function(train, FUN, wf) {
  cat("Proc:: Cross Validation...\n")
  capture.output(kcv <- kf_xval(x = train, nfolds = 10, FUN = FUN, optim_strat="cv"))
  cat("Proc:: Blocked Cross Validation...\n")
  capture.output(bcv <- blocked_kf_xval(x = train, nfolds = 10, FUN = FUN, optim_strat="cv"))
  cat("Proc:: Modified Cross Validation...\n")
  capture.output(
    mcv <- tryCatch(
    modified_xval(x = train, nfolds = 10, FUN = FUN, optim_strat="cv"),
      error = function(e) NA
      )
    )
  cat("Proc:: HV Blocked Cross Validation...\n")
  capture.output(hvbcv <- hv.block_xval(x = train, nfolds = 10, FUN = FUN, optim_strat="cv"))
  cat("Proc:: Prequential in Blocks...\n")
  capture.output(fv <- prequential_in_blocks(x = train, nfolds = 10, FUN = FUN, optim_strat="cv"))
  #cat("Proc:: Forward Validation with Gap...\n")
  #capture.output(fvg <- forward_validation_gap(x = train, nfolds = 10, FUN = FUN, optim_strat="cv"))
  #cat("Proc:: Block Bootstrap...\n")
  #capture.output(bb <- bbootstrap(x = train, nblocks = 10,nreps = 5, FUN = FUN, optim_strat="cv"))
  #cat("Proc:: Moving Block Bootstrap...\n")
  #capture.output(mbb <- mbbootstrap(x = train, nblocks = 10,nreps = 5, FUN = FUN, optim_strat="cv"))
  
  subtrain <- partition(train, .7)
  cat("Proc:: OOS Slide...\n")
  capture.output(oos_slide <- oos_rwupdt(subtrain$train, subtrain$test, FUN))
  cat("Proc:: OOS Landmark...\n")
  capture.output(oos_landm <- oos_roupdt(subtrain$train, subtrain$test, FUN))
  cat("Proc:: OOS Monte Carlo...\n")
  capture.output(oos_mc <- oos_montecarlo(target ~., train, wf))

  cat("...Finito!\n")
  eprocs <- list(kcv, bcv, mcv, hvbcv, fv, oos_slide, oos_landm, oos_mc[[1]], oos_mc[[2]], oos_mc[[3]])
  names(eprocs) <- c("CV.KF", "CV.BKF", "CV.MKF", "CV.hvBKF", "OOS.PB", 
                     "OOS.SW", "OOS.GW", "OOS.H",
                     "OOS.MC60","OOS.MC20")

  eprocs
}

#' Time series performance estimation
#'
#' @param x embedded time series
#' @param model learning model
#' @param workflow learning workflow for code{performanceEstimation}
#'
#' @export
ts_estimation <- function(x, model, workflow) {
  data <- partition(x, .7)

  estimation <- data$train
  test <- data$test

  # estimating loss
  print("Estimating")
  loss_estimate <- performance_estimation(estimation, model, workflow)

  le <- rbind_(lapply(loss_estimate, function(j) {
    if (is.list(j)) j <- do.call(rbind, j)
    colMeans(j, na.rm = TRUE)
  }))

  # computing ground true loss
  L <- model(estimation, test)

  # computing predictive accuracy error
  PAE <- t(apply(le, 1, function(o) pae(o, L)))
  PAE
}

