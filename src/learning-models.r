RF <- 
  function(form,train,test) {
    require(ranger)
    
    m <- ranger(formula = form, data=train, num.trees = 100)
    
    predict(m,test)$predictions
  }

RBR <- 
  function(form,train,test) {
    require(Cubist)
    
    X <- model.matrix(form, train)
    Y <- get_y(train, form)
    
    m <- cubist(X, Y, committees = 10)
    
    Xts <- model.matrix(form, test)
    
    predict(m,Xts)
  }


LASSO <- 
  function(form,train,test) {
    require(glmnet)
    
    X <- model.matrix(form, train)
    Y <- get_y(train, form)
    
    m.all <- glmnet(X, Y, alpha = 0, family = "gaussian")
    m <- glmnet(X,
                Y,
                alpha = 1,
                lambda = min(m.all$lambda),
                family = "gaussian")
    
    Xts <- model.matrix(form, test)
    
    unname(predict(m,Xts)[,1])
  }



GP <- 
  function(form,train,test) {
    require(kernlab)
    
    m <- gausspr(form,
                 train,
                 type = "regression",
                 kernel = "rbfdot",
                 tol = .001)
    
    predict(m,test)[,1]
  }

CART <- 
  function(form,train,test) {
    require(DMwR)
    
    m <- rpartXse(form,train)
    
    unname(predict(m,test))
  }

CART_loss <- 
  function(train, test, form, avg=TRUE) {
    y <- get_y(test, form)
    y_tr <- get_y(train, form)
    y_hat <- tryCatch(CART(form, train, test),
             error=function(e) {
               rep(mean(y_tr), times=length(y))
             })
    
    mase_cal(y_tr, y, y_hat, avg)
  }



GP_loss <- 
  function(train, test, form, avg=TRUE) {
    y <- get_y(test, form)
    y_tr <- get_y(train, form)
    y_hat <- GP(form, train, test)
    
    mase_cal(y_tr, y, y_hat, avg)
  }



LASSO_loss <- 
  function(train, test, form, avg=TRUE) {
    y <- get_y(test, form)
    y_tr <- get_y(train, form)
    y_hat <- 
      tryCatch(LASSO(form, train, test),
               error=function(e) {
                 rep(mean(y_tr), times=length(y))
               })
    
    mase_cal(y_tr, y, y_hat, avg)
  }

RF_loss <- 
  function(train, test, form, avg=TRUE) {
    y <- get_y(test, form)
    y_tr <- get_y(train, form)
    y_hat <- RF(form, train, test)
    
    #rmse(y, y_hat)
    mase_cal(y_tr, y, y_hat, avg)
  }

RBR_loss <- 
  function(train, test, form, avg=TRUE) {
    y <- get_y(test, form)
    y_tr <- get_y(train, form)
    y_hat <- RBR(form, train, test)
    
    mase_cal(y_tr, y, y_hat, avg)
  }

MLP <-
  function(form,train,test) {
    require(nnet)
    m <- nnet(form, train, size=50, linout=T, MaxNWts=2^20, maxit=150)

    unname(predict(m,test))[,1]
  }

MLP_loss <-
  function(train, test, form, avg=TRUE) {
    y <- get_y(test, form)
    y_tr <- get_y(train, form)
    y_hat <- MLP(form, train, test)

    mase_cal(y_tr, y, y_hat, avg)
  }

