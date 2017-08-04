require(performanceEstimation)

M.glm <- function(train, test, optim_strat = "oos", ...) {
  require(glmnet)
  if (optim_strat == "oos") {
    est_proc <- MonteCarlo(nReps = 1, szTrain = .7, szTest = .29)
  } else {
    est_proc <- CV(nReps = 1, nFolds = 5)
  }
  form <- target ~.
  #paraOptim <- performanceEstimation(
  #  PredTask(form = target ~.,
  #           copy = TRUE,
  #           data = train,
  #           type = "regr", taskName = "param_optim"),
  #  workflows = workflowVariants(wf = 'glm_wf', alpha=c(0,.2,.4,.6,.8,1)),
  #  EstimationTask(metrics=c("rmse"), method=est_proc)
  #)
  #topM <- topPerformer(paraOptim, "rmse", "param_optim")
  train <- soft.completion(train)

  predictors <- model.matrix(form, train)
  Y <- get_y(train, form)
  M.lambda <- cv.glmnet(predictors, Y, family = "gaussian", alpha = 1)$lambda.min
  M <- glmnet(predictors, Y, alpha = 1, lambda = M.lambda)
  preds <- predict(M, model.matrix(form, test))[,1]

  res <- list(trues = test[,"target"], preds = preds)
  
  regMetrics(res[["trues"]], res[["preds"]])
}

wf.glm <- function(form, train, test, optim_strat = "oos", ...) {
  require(glmnet)

  if (optim_strat == "oos") {
    est_proc <- MonteCarlo(nReps = 1, szTrain = .7, szTest = .29)
  } else {
    est_proc <- CV(nReps = 1, nFolds = 5)
  }
  form <- target ~.

  #paraOptim <- performanceEstimation(
  #  PredTask(form = target ~.,
  #           copy = TRUE,
  #           data = train,
  #           type = "regr", taskName = "param_optim"),
  #  workflows = workflowVariants(wf = 'glm_wf', alpha=c(1)),
  #  EstimationTask(metrics=c("rmse"), method=est_proc)
  #)
  #topM <- topPerformer(paraOptim, "rmse", "param_optim")

  predictors <- model.matrix(form, train)
  Y <- get_y(train, form)
  M.lambda <- cv.glmnet(predictors, Y, family = "gaussian", alpha = 1)$lambda.min
  M <- glmnet(predictors, Y, alpha = 1, lambda = M.lambda)
  preds <- predict(M, model.matrix(form, test))[,1]

  res <- list(trues = test[,"target"], preds = preds)
  res
}


M.nnet <- function(train, test, optim_strat = "oos", ...) {
  require(nnet)
  if (optim_strat == "oos") {
    est_proc <- MonteCarlo(nReps = 1, szTrain = .7, szTest = .29)
  } else {
    est_proc <- CV(nReps = 1, nFolds = 5)
  }
  form <- target ~.
  paraOptim <- performanceEstimation(
    PredTask(form = target ~.,
             copy = TRUE,
             data = train,
             type = "regr", taskName = "param_optim"),
    workflows = workflowVariants(learner="nnet",
                                 learner.pars=list(size=c(5,7,10, 20),
                                                   decay=c(0.1,0.01, 0,0.005),
                                                   trace = FALSE,
                                                   linout = TRUE)),
    EstimationTask(metrics=c("rmse"), method=est_proc)
  )
  topM <- topPerformer(paraOptim, "rmse", "param_optim")

  lpars <- topM@pars$learner.pars

  m <- nnet(form,
            train,
            trace = FALSE,
            linout = TRUE,
            size = lpars[["size"]],
            decay = lpars[["decay"]], ...)

  preds <- predict(m, test)

  res <- list(trues = test[,"target"], preds = preds[,1])

  regMetrics(res[["trues"]], res[["preds"]])
}


wf.nnet <- function(form, train, test, optim_strat = "oos", ...) {
  require(nnet)

  if (optim_strat == "oos") {
    est_proc <- MonteCarlo(nReps = 1, szTrain = .7, szTest = .29)
  } else {
    est_proc <- CV(nReps = 1, nFolds = 5)
  }
  form <- target ~.
  print("a")
  
  paraOptim <- performanceEstimation(
    PredTask(form = target ~.,
             copy = TRUE,
             data = train,
             type = "regr", taskName = "param_optim"),
    workflows = workflowVariants(learner="nnet",
                                 learner.pars=list(size=c(5,7,10, 20),
                                                   decay=c(0.1,0.01, 0, 0.005),
                                                   trace = FALSE,
                                                   linout = TRUE)),
    EstimationTask(metrics=c("rmse"), method=est_proc)
  )

  topM <- topPerformer(paraOptim, "rmse", "param_optim")

  lpars <- topM@pars$learner.pars

  m <- nnet(form,
            train,
            trace = FALSE,
            linout = TRUE,
            size = lpars[["size"]],
            decay = lpars[["decay"]], ...)

  preds <- predict(m, test)

  list(trues = test[,"target"], preds = preds[,1])
}



wf.dt <- function(form, train, test, optim_strat = "oos", ...) {
  require(rpart)

  if (optim_strat == "oos") {
    est_proc <- MonteCarlo(nReps = 1, szTrain = .7, szTest = .29)
  } else {
    est_proc <- CV(nReps = 1, nFolds = 5)
  }
  form <- target ~.

  paraOptim <- performanceEstimation(
    PredTask(form = target ~.,
             copy = TRUE,
             data = train,
             type = "regr",
             taskName = "param_optim"),
    workflows = workflowVariants(learner="rpart",
                                 learner.pars=list(minsplit=c(10, 20),
                                                   maxdepth = c(10, 15),
                                                   cp = c(.01,.05,.001,.1))),

    EstimationTask(metrics=c("rmse"), method=est_proc)
  )

  topM <- topPerformer(paraOptim, "rmse", "param_optim")

  lpars <- topM@pars$learner.pars

  m <- rpart(form,
             train,
             control = rpart.control(minsplit = lpars$minsplit,
                                     maxdepth = lpars$maxdepth,
                                     cp = lpars$cp))

  preds <- predict(m, test)

  list(trues = test[,"target"], preds = preds)
}

M.dt <- function(train, test, optim_strat = "oos", ...) {
  require(rpart)
  if (optim_strat == "oos") {
    est_proc <- MonteCarlo(nReps = 1, szTrain = .7, szTest = .29)
  } else {
    est_proc <- CV(nReps = 1, nFolds = 5)
  }
  form <- target ~.

  paraOptim <- performanceEstimation(
    PredTask(form = target ~.,
             copy = TRUE,
             data = train,
             type = "regr",
             taskName = "param_optim"),
    workflows = workflowVariants(learner="rpart",
                                 learner.pars=list(minsplit=c(10, 20),
                                                   maxdepth = c(10, 15),
                                                   cp = c(.01,.05,.001,.1))),
    #workflows = workflowVariants(learner="rpart"),

    EstimationTask(metrics=c("rmse"), method=est_proc)
  )

  topM <- topPerformer(paraOptim, "rmse", "param_optim")

  lpars <- topM@pars

  m <- rpart(form,
             train,
             control = rpart.control(minsplit = lpars$learner.pars$minsplit,
                                     maxdepth = lpars$learner.pars$maxdepth,
                                     cp = lpars$learner.pars$cp))


  preds <- predict(m, test)

  res <- list(trues = test[,"target"], preds = preds)
  regMetrics(res[["trues"]], res[["preds"]])
}


workflow.dt  <- Workflow(wf = 'wf.dt')
workflow.nn  <- Workflow(wf = 'wf.nnet')
workflow.glm <- Workflow(wf = 'wf.glm')
