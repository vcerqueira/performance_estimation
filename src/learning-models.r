RF <- 
  function(form,train,test) {
    require(ranger)
    
    m <- ranger(formula = form, data=train, num.trees = 100)
    
    predict(m,test)$predictions
  }

RF_loss <- 
  function(train, test, form) {
    y <- get_y(test, form)
    y_hat <- RF(form, train, test)
    
    rmse(y, y_hat)
  }

RBR <- 
  function(form,train,test) {
    require(Cubist)
    
    X <- model.matrix(form, train)
    Y <- get_y(train, form)
    
    m <- cubist(X, Y, committees = 25)
    
    Xts <- model.matrix(form, test)
    
    predict(m,Xts)
  }

RBR_loss <- 
  function(train, test, form) {
    y <- get_y(test, form)
    y_hat <- RBR(form, train, test)
    
    rmse(y, y_hat)
  }


LASSO <- 
  function(form,train,test) {
    require(glmnet)
    
    X <- model.matrix(form, train)
    Y <- get_y(train, form)
    
    m.all <- glmnet(X, Y, alpha = 1, family = "gaussian")
    m <- glmnet(X,
                Y,
                alpha = 1,
                lambda = min(m.all$lambda),
                family = "gaussian")
    
    Xts <- model.matrix(form, test)
    
    unname(predict(m,Xts)[,1])
  }

LASSO_loss <- 
  function(train, test, form) {
    y <- get_y(test, form)
    y_hat <- LASSO(form, train, test)
    
    rmse(y, y_hat)
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

GP_loss <- 
  function(train, test, form) {
    y <- get_y(test, form)
    y_hat <- GP(form, train, test)
    
    rmse(y, y_hat)
  }
