pre_split_workflow <- 
  function(x, nfolds) {
    f <- cv.folds(x, nfolds)
    
    estimation_results <- vector("list", nfolds)
    seq. <- seq_len(nfolds)
    for (i in seq.[-length(seq.)]) {
      cat(i, "\n")
      tr.id <- which(f %in% seq_len(i))
      ts.id <- which(f == i + 1L)
      
      train <- x[tr.id, ]
      test  <- x[ts.id, ]
      estimation_results[[i]] <- 
        pre_split_performance_estimation(
          train = train,
          test = test,
          form = form,
          predictive_algorithm = "rbr"
        )
    }
    estimation_results
  }


pre_split_performance_estimation <- 
  function(train,test, form, predictive_algorithm = "rbr") {
    nfolds <- 10
    
    pred_model <-
      switch(predictive_algorithm,
             "rf" = RF_loss,
             "rbr" = RBR_loss,
             "lasso" = LASSO_loss,
             "gp" = GP_loss,
             RF_loss)
    
    true_loss <-
      pred_model(train = train,
                 test = test,
                 form = form)
    
    estimated_loss <- 
      performance_estimation(
        train = train,
        form = form,
        pred_model = pred_model,
        nfolds = nfolds
      )
    
    err_estimation <- sapply(estimated_loss,
                             function(u) {
                               ((u - true_loss) / true_loss) * 100
                             })
    
    list(err_estimation=err_estimation, err=true_loss)
  }