#' k-fold cross validation
#' The standard cross validation procedure.
#'
#' @param x data: embedded time series
#' @param nfolds no of folds
#' @param FUN function to apply to each iteration's train and test. Typically
#' \strong{FUN} is a workflow where a predictive model is applied in a training set
#' and the model is evaluated in the test set.
#' @param ... further parameters to \code{FUN}
#'
#' @export
kf_xval <- function(x, nfolds, FUN, shuffle.rows = TRUE, ...) {
  if (shuffle.rows) x <- cv.shuffle(x)
  f <- cv.folds(x, nfolds)

  cv.res <- list() 
  for (i in seq_len(nfolds)) {
    ts.id <- which(f == i)

    train <- x[-ts.id, ]
    print(dim(train))
    test  <- x[ ts.id, ]
    print(dim(test))
    cv.res[[i]] <- FUN(train, test, ...)
  }
  cv.res
}

#' Blocked k-fold cross validation
#' Standard k-fold cross validation without reshuffling rows
#'
#' @inheritParams kf_xval
#'
#' @export
blocked_kf_xval <- function(x, nfolds, FUN, ...) {
  kf_xval(x = x, nfolds = nfolds, FUN = FUN, shuffle.rows = FALSE, ...)
}

#' Modified k-fold Cross Validation
#' Standard CV, but removes rows from training that are
#' dependent with the test set.
#'
#' @inheritParams kf_xval
#'
#' @export
modified_xval <- function(x, nfolds, FUN, ...) {
  K <- ncol(x)
  x$aux <- seq_len(nrow(x))
  x <- cv.shuffle(x)

  f <- cv.folds(x, nfolds)

  cv.res <- list()
  for (i in seq_len(nfolds)) {
    ts.id <- which(f == i)
    test  <- x[ts.id, ]

    depRows <- unique(unlist(lapply(test$aux, function(z) (z-K-1):(z+K-1L))))
    depRows <- depRows[depRows > 0]

    train <- x[-c(ts.id, depRows), ]
    if (nrow(train) < 1) {
      cv.res[[i]] <- rep(NA_real_, times = 3)
    } else {
      test$aux <- NULL
      train$aux <- NULL

      cv.res[[i]] <- FUN(train, test, ...)
    }
  }
  cv.res
}

#' hv - block Cross Validation
#' Cross without reshuffling rows and removing dependent rows.
#' Since there is no reshuffling, the dependent rows are just
#' the \code{K}-adjacent ones in the adjacent folds. \code{K} denotes
#' the embedding dimension.
#'
#' @inheritParams kf_xval
#'
#' @export
hv.block_xval <- function(x, nfolds, FUN, ...) {
  K <- ncol(x)
  f <- cv.folds(x, nfolds)

  cv.res <- list()
  seq. <- seq_len(nfolds)
  lseq <- seq.[length(seq.)]
  for (i in seq.) {
    ts.id <- which(f == i)
    # upper cut
    kcuts <- integer(0L)
    if (i + 1L <= lseq) {
      upper.fold <- which(f == i + 1L)
      upper.cut <- upper.fold[1:K]
      kcuts <- c(kcuts, upper.cut)
    }
    # lower cut
    if (i - 1L >= 1L) {
      lower.fold <- which(f == i - 1L)
      len.lf <- length(lower.fold)
      lower.cut <- lower.fold[(len.lf - K + 1L):len.lf]
      kcuts <- c(kcuts, lower.cut)
    }

    train <- x[-c(ts.id, kcuts), ]
    test  <- x[ts.id, ]

    cv.res[[i]] <- FUN(train, test, ...)
  }
  cv.res
}

#' Sequential Block Evaluation
#' Trains in the the up-to-i folds and tests on i+1
#' 
#' Prequential in blocks in a growing window fashion.
#'
#' @inheritParams kf_xval
#'
#' @export
prequential_in_blocks <- function(x, nfolds, FUN, ...) {
  f <- cv.folds(x, nfolds)

  cv.res <- list()
  seq. <- seq_len(nfolds)
  for (i in seq.[-length(seq.)]) {
    tr.id <- which(f %in% seq_len(i))
    ts.id <- which(f == i + 1L)

    train <- x[tr.id, ]
    test  <- x[ts.id, ]
    cv.res[[i]] <- FUN(train, test, ...)
  }
  cv.res
}

#' Sequential Block Evaluation
#' Trains in the the up-to-i folds and tests on i+2
#' Doesn't test on i+1 to further increase independence
#'
#' @inheritParams kf_xval
#'
#' @export
prequential_in_blocks_gap <- function(x, nfolds, FUN, ...) {
  f <- cv.folds(x, nfolds)

  cv.res <- list()
  seq. <- seq_len(nfolds); len <- length(seq.)

  seq. <- seq.[-c(len:(len - 1L))]
  for (i in seq.) {
    tr.id <- which(f %in% seq_len(i))
    ts.id <- which(f == i + 2L)

    train <- x[tr.id, ]
    test  <- x[ts.id, ]
    cv.res[[i]] <- FUN(train, test, ...)
  }
  cv.res
}

#' Simple Block Bootstrap
#' Block bootstrap with non-overlapping blocks
#'
#' @param x data
#' @param nblocks no of block to divide \code{x} into
#' @param nreps no of bootstrap repetitions
#' @param FUN function to apply
#' @param ... further params to \code{FUN}
#'
#' @export
bbootstrap <- function(x, nblocks, nreps, FUN, ...) {
  K <- ncol(x)
  res.boot <- list()
  x <- unembed.timeseries(x[-nrow(x), ], x[nrow(x), ])$data
  f <- cv.folds(x = x, nfolds = nblocks)
  for (i in seq_len(nreps)) {
    seq.f <- seq_len(nblocks)
    OOB.id <- sample(seq.f, nblocks, replace = TRUE)
    train <- rbind_(
      lapply(OOB.id, function(z) {
        r.id <- which(z == f)
        x_z <- embed.timeseries(x[r.id], K)
        rownames(x_z) <- NULL

        x_z
      })
    )
    ts.id <- setdiff(seq.f, OOB.id)

    if (length(ts.id) < 1) {
      res.boot[[i]] <- rep(NA_real_, times = 3)
    } else {
      test <- rbind_(
        lapply(ts.id, function(z) {
          r.id <- which(z == f)
          x_z <- embed.timeseries(x[r.id], K)
          rownames(x_z) <- NULL

          x_z
        })
      )
      res.boot[[i]] <- FUN(train, test, ...)
    }
  }
  res.boot
}


#' Moving Block Bootstrap
#' Block bootstrap with overlapping blocks
#'
#' @inheritParams bbootstrap
#'
#' @export
mbbootstrap <- function(x, nblocks, nreps, FUN, ...) {
  res.boot <- list()
  f <- cv.folds(x, nblocks)

  for (i in seq_len(nreps)) {
    OOB.id <- sample(seq_len(nblocks), nblocks, replace = TRUE)
    tr.id <- unlist(lapply(OOB.id, function(z) which(z == f)))
    train <- x[tr.id, ]
    test  <- x[-unique(tr.id), ]

    if (nrow(test) < 1) {
      res.boot[[i]] <- rep(NA_real_, times = 3)
    } else {
      res.boot[[i]] <- FUN(train, test, ...)
    }
  }
  res.boot
}

#' OOS Rolling Window
#' Keeps same window with retraining procedures
#'
#' @param train train set
#' @param test test set
#' @param FUN function to apply
#' @param ... further params to \code{FUN}
#'
#' @export
oos_rwupdt <- function(train, test, FUN, ...) {
  seq. <- seq_len(nrow(test))
  seq.diff <- seq. - 1L

  rbind_(
    lapply(seq.diff, function(y) {
      cut.y <- seq_len(y)
      if (y < 1) {
        train.set <- train
        test.set  <- test
      } else {
        train.set <- rbind.data.frame(train[-cut.y, ], test[cut.y, ])
        test.set  <- test[-cut.y, ]
      }
      FUN(train.set, test.set, ...)
    })
  )
}

#' OOS Rolling Origin Update
#'
#' Landmark style retraining
#'
#' @inheritParams oos.rw.updt
#'
#' @export
oos_roupdt <- function(train, test, FUN, ...) {
  seq. <- seq_len(nrow(test))
  seq.diff <- seq. - 1L

  rbind_(
    lapply(seq.diff, function(y) {
      cut.y <- seq_len(y)
      if (y < 1) {
        train.set <- train
        test.set  <- test
      } else {
        train.set <- rbind.data.frame(train, test[cut.y, ])
        test.set  <- test[-cut.y, ]
      }
      FUN(train.set, test.set, ...)
    })
  )
}

#' Out-of-sample estimation in multiple testing periods
#' 
#' Using monte carlo simulations
#' 
#' @param form formula
#' @param x time series data
#' @param wf learning pipeline
#' 
#' @export
oos_montecarlo <- function(form, x, wf) {
  capture.output(perfMC_0 <- performanceEstimation(tasks = PredTask(form=form, data = x, copy = T),
                                                   workflows = wf,
                                                   estTask = EstimationTask(metrics = c("rmse", "mae", "mape"),
                                                                            method = MonteCarlo(nReps = 1,
                                                                                                szTrain = 0.7,
                                                                                                szTest = .29 ))))
  MCres_0 <- getRes.pe(perfMC_0)
  capture.output(perfMC_40 <- performanceEstimation(tasks = PredTask(form = form, data = x, copy = T),
                                                    workflows = wf,
                                                    estTask = EstimationTask(metrics = c("rmse", "mae", "mape"),
                                                                             method = MonteCarlo(nReps = 10,
                                                                                                 szTrain = 0.42,
                                                                                                 szTest = .18 ))))
  MCres_40 <- getRes.pe(perfMC_40)
  
  capture.output(perfMC_80 <- performanceEstimation(tasks = PredTask(form = form, data = x, copy = T),
                                                    workflows = wf,
                                                    estTask = EstimationTask(metrics = c("rmse", "mae", "mape"),
                                                                             method = MonteCarlo(nReps = 20,
                                                                                                 szTrain = 0.14,
                                                                                                 szTest = .06))))
  MCres_80 <- getRes.pe(perfMC_80)

  res <- list(MCres_0 = MCres_0, MCres_40 = MCres_40, MCres_80 = MCres_80)

  res
}