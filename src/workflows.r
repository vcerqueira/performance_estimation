workflow <- 
  function(ds,
           form,
           predictive_algorithm = "rf",
           nfolds,
           outer_split) {
    
    
    khat <- estimate_k(ds[1:(length(ds) * outer_split)], 
                       m.max = 30, tol = .01)
    if (khat < 8) khat <- 8
    
    x <- embed_timeseries(ds, khat)
    
    xp <- partition(x, outer_split)
    
    train <- xp$train
    test <- xp$test
    
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
    
    estimated_loss_on <- 
      online_methods_pe(train = train,
                        form = form,
                        pred_model = pred_model)
    
    estimated_loss <- 
      c(estimated_loss,
        estimated_loss_on)
    
    err_estimation <- sapply(estimated_loss,
                             function(u) {
                               ((u - true_loss) / true_loss) * 100
                             })
    
    list(err_estimation=err_estimation, 
         est_err = estimated_loss,
         err=true_loss)
  }


performance_estimation <- 
  function(train, form, pred_model, nfolds) {
    cat("Estimating loss using ...\n")
    cat("... std. x val ...\n")
    std_x_val <-
      kf_xval(
        x = train,
        nfolds = nfolds,
        FUN = pred_model,
        shuffle.rows = TRUE,
        form = form, 
        average_results = TRUE)
    
    cat("... blocked x val ...\n")
    blocked_x_val <- 
      blocked_kf_xval(
        x = train,
        nfolds = nfolds,
        FUN = pred_model,
        form = form)
    
    cat("... modified x val ...\n")
    mod_x_val <- 
      modified_xval(
        x = train,
        nfolds = nfolds,
        FUN = pred_model,
        average_results = TRUE,
        form = form)
    
    cat("... hv blocked x val ...\n")
    hvb_x_val <- 
      tryCatch(
        hv.block_xval(
          x = train,
          nfolds = nfolds,
          FUN = pred_model,
          average_results = TRUE,
          form = form
        ), error = function(e) NA)
    
    cat("... preq blocks ...\n")
    preq_b <-
      prequential_in_blocks(
        x = train,
        nfolds = nfolds,
        FUN = pred_model,
        average_results = TRUE,
        form = form
      )
    
    cat("... sliding preq blocks ...\n")
    sl_preq_b <-
      sliding_prequential_in_blocks(
        x = train,
        nfolds = nfolds,
        FUN = pred_model,
        average_results = TRUE,
        form = form
      )
    
    cat("... preq blocks w gap ...\n")
    preq_b_gap <-
      prequential_in_blocks_gap(
        x = train,
        nfolds = nfolds,
        FUN = pred_model,
        average_results = TRUE,
        form = form
      )
    
    cat("... holdout ...\n")
    hout <-
      holdout(x = train,
              FUN = pred_model,
              form = form)
    
    cat("... repeated holdout ...\n")
    rephout <-
      repeated_holdout(
        x = train,
        nreps = nfolds,
        train_size = .6,
        test_size = .1,
        average_results = TRUE,
        FUN = pred_model,
        form = form
      )
    
    loss_estimations <- 
      c(x_val = std_x_val,
        b_x_val = blocked_x_val,
        m_x_val = mod_x_val,
        hvb_x_val = hvb_x_val,
        preq_b = preq_b,
        sl_preq_b = sl_preq_b,
        preq_b_gap = preq_b_gap,
        hout = hout,
        rephout = rephout)
    
    loss_estimations
  }

online_methods_pe <- 
  function(train, form, pred_model) {
    cat("Estimating loss using ...\n")
    
    xp <- partition(train, .7)
    
    train_ <- xp$train
    validation <- xp$test
    
    cat("... preq lw ...\n")
    preq_lw <-
      PrequentialLandmark(
        train = train_,
        test = validation,
        FUN = pred_model,
        average_results = TRUE,
        form=form
      )
    
    cat("... preq sw ...\n")
    preq_sw <-
      PrequentialSliding(
        train = train_,
        test = validation,
        FUN = pred_model,
        average_results = TRUE,
        form=form
      )
    
    loss_estimations <- 
      c(preq_lw = preq_lw,
        preq_sw = preq_sw)
    
    loss_estimations
  }
