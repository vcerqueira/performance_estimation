# FROM https://github.com/cbergmeir/tsExpKit/blob/master/R/simulateTS.R

# The coefficients that are generated are chosen in a way that the resulting 
# processes are stationary (AR) and invertible (MA).
# 
# TODO: function also necessary (and present) in tsDyn
# For detailed explanation see the INS paper.
# 
# @title Generate random coefficients for an ARMA model.
# @param lags the number of lags
# @param maxRoot the root of the characteristic polynomial is chosen between 1.1 and \code{maxRoot}. 
# @export
generateRandomARMAParameters <- function(lags, maxRoot)  {
  
  if(maxRoot <= 1.1) stop("maxRoot has to be bigger than 1.1")
  
  l <- lags# + 1
  s <- sign(runif(l,-1,1))
  polyRoots <- s*runif(l,1.1,maxRoot)
  
  #calculate coefficients
  coeff <- 1
  for(root in polyRoots) coeff <- c(0,coeff) - c(root*coeff,0)
  
  nCoeff <- coeff / coeff[1]
  params <- - nCoeff[2:length(nCoeff)]  
  
  return(params)
}

##old version for parameter generation
#generateRandomARMAParameters <- function(lags)  {
#  rVals <- rnorm(lags)
#  values <- -sort(-abs(rVals))/(max(abs(rVals))+0.05)
#  factors <- rep(c(1, -1),floor(lags/2))
#  if(lags %% 2 == 1) factors <- c(factors, 1)
#  params <- factors * values
#  return(params)
#}

##old version for non-linear time series simulation
#simulateNonlinearTS <- function(length, lags=4, seed=1)  {
#  
#  set.seed(seed)
# 
#  maxlags <- lags + 10 
#  model <- "nonlin"
#  
#  w <- rnorm(length+maxlags, sd = 0.01)
#  
#  lts <- simulateLinearTS(length+maxlags, lags=lags, seed=seed)[["ts"]]
#  
#  z <- rep(0, length+maxlags)
#  for(t in (maxlags-5):(length+maxlags)) 
#    z[t] <- 0.7 * exp (-z[t - 1] / length) + 0.4 * cos (z[t - 2]) + 0.25 * atan(z[t - 3]) + w[t] +lts[t]
#  
#  ts <- z[maxlags:(length+maxlags-1)]
#  
#  return(list(lags=lags, model=model, seed=seed, ts=ts))
#  
#}

#' This function can be used to generate pure AR, pure MA, and ARMA series. The AR part will be
#' stationary and the MA part invertible. Therefore, the coefficients are not given directly,
#' but a value \code{maxRoot} which controls the interval from which the roots for the 
#' characteristic polynomials are chosen. So, all the roots of the characteristic polynomials 
#' are real-valued. For a detailed explanation see the referenced literature.
#' 
#' @title Generate AR, MA, and ARMA series 
#' @param length the length of the series to be generated
#' @param ar if \code{TRUE}, series has an AR part
#' @param ma if \code{TRUE}, series has an MA part
#' @param lags the number of lags
#' @param seed a seed used for random number generation
#' @param maxRoot the roots of the characteristic polynomials are chosen between 1.1 and \code{maxRoot} 
#' @param n.start burn-in period. if NA, calculated automatically by arima.sim
#' @param ... parameters passed to arima.sim
#' @return a list containing all the parameters, and a member \code{ts} with the generated series
#' @references
#' C. Bergmeir, J.M. Benítez, On the Use of Cross-validation for Time Series Predictor Evaluation. Information Sciences 191 (2012) 192-213.
#' @export
simulateLinearTS <- function (length, ar=TRUE, ma=TRUE, lags=4, seed=1, maxRoot=5.0, n.start = NA, ...)  {
  
  set.seed(seed)
  
  params <- generateRandomARMAParameters(lags, maxRoot)
  
  #print(params)
  
  if(ar && !ma)  {
    model <- "AR"    
    paramsAr <- params
    paramsMa <- 0     
    ts <- arima.sim(model=list(ar=params),n=length, n.start = n.start, ...)
  } else if(!ar && ma) {
    model <- "MA"    
    paramsMa <- params
    paramsAr <- 0
    ts <- arima.sim(model=list(ma=params),n=length, n.start = n.start, ...)
  } else if(ar && ma)  {
    model <- "ARMA"    
    paramsMa <- params    
    paramsAr <- generateRandomARMAParameters(lags, maxRoot)    
    ts <- arima.sim(model=list(ar=paramsAr, ma=params),n=length, n.start = n.start)
  }
  
  return(list(paramsAr=paramsAr, paramsMa=paramsMa, lags=lags, model=model, seed=seed, maxRoot=maxRoot, ts=ts))
}

#' This function can be used to generate nonlinear time series. 
#' It is similar to the function \code{\link{simulateLinearTS}}, but applies nonlinear functions to certain lags.
#' The nonlinear functions currently used are: cos, sin, tanh, atan, and exp(-x/10000).
#' For a detailed explanation see the referenced literature.
#' 
#' @title Generate nonlinear time series 
#' @param length the length of the series to be generated
#' @param lags the number of lags
#' @param seed a seed used for random number generation
#' @param maxRoot the roots of the characteristic polynomials are chosen between 1.1 and \code{maxRoot} 
#' @return a list containing all the parameters, and a member \code{ts} with the generated series
#' @references
#' C. Bergmeir, J.M. Benítez, On the Use of Cross-validation for Time Series Predictor Evaluation. Information Sciences 191 (2012) 192-213.
#' @export
#' @export
simulateNonlinearTS <- function(length, lags=4, seed=1, maxRoot=5.0)  {
  
  #  length=10000
  #  lags=20
  #  seed=3
  #  maxRoot=5.0
  
  x <- NULL
  
  randNonlinX <- function(type, x)  {
    nonlinX <- switch(type, cos(x), sin(x), tanh(x), atan(x), exp(-x/10000))#, cosh(x), sinh(x))#,1/(x*x), 1/(x*x*x))#x,tan(x)),  
    return(nonlinX)
  }
  
  set.seed(seed)
  
  params <- generateRandomARMAParameters(lags, maxRoot)
  #print(params)
  
  x[1:lags] <- rnorm (lags, sd = 0.5)
  
  type <- ceiling(runif(lags,min=0, max=5))
  
  #print(type)
  
  #type[1:lags] <- 1
  
  for (i in ((lags+1):(length+2*lags))) {
    
    x[i] <- rnorm (1, sd = 0.5) 
    
    for (j in 1:lags)  {
      x[i] <- x[i] + params[j]*randNonlinX(type[j], x[i-j]) 
    }
  }
  
  model <- "nonlin"
  ts <- x[(2*lags+1):(length+2*lags)]
  
  return(list(lags=lags, model=model, seed=seed, ts=ts))
  
}

##example code
#par(mfrow = c(4, 1))
#
#ts <- simulateNonlinearTS(1000, seed=3, lags=15)
#ts
#plot(ts$ts, type="l")
#acf(ts$ts)
#tnnTest(ts$ts)
#
#ts <- simulateNonlinearTS(1000, seed=3, lags=9)
#ts
#plot(ts$ts, type="l")
#acf(ts$ts)



##-----------------------
#VAR

mcevec <- function(lambda, c, n, d) {
  
  v <- vector()
  v[1:n] <- 0
  v[(n-d+1):n] <- c
  
  for(j in (n-d):1) {
    v[j] <- lambda*v[j+d]
  }
  
  v
}

#mcgenevec <- function(lambda, c, w, n, d) {
#  
#  v <- vector()
#  v[1:n] <- 0
#  v[(n-d+1):n] <- c
#  
#  for(j in (n-d):1) {
#    v[j] <- lambda*v[j+d] + w[j+d]
#  }
#  
#  v
#}

companionMatrixFromEigenvalues <- function(q, lambda, c, d) {
  
  nchain <- length(q)
  n <- sum(q)
  
  #v <- matrix(0, nrow=n, ncol=n)
  
  v <- matrix(0, nrow=n, ncol=n)
  
  i <- 0
  for(k in 1:nchain) {
    v[, i+1] <- mcevec(lambda[k], c[, i+1], n, d)
    
    #    if(q[k] > 1) {
    #      for(j in 2:q[k]) {
    #        mcgenevec(lambda[k], c[, i+1], v[,i+j-1], n, d)
    #      }      
    #    }
    i <- i+q[k]
  }
  
  X <- v
  #X
  
  F <- X%*%diag(lambda) %*% solve(X)
  F
}


generateRandomStableCompanionMatrix <- function(dim = 2, order = 2) {
  
  lenq <- order*dim
  
  #  c <- matrix(runif(lenq), nrow=order, ncol=lenq)
  #  c[which(c>0.5)] <- 1
  #  c[which(c<=0.5)] <- 0
  
  c <- matrix(1-diag(dim),nrow=dim,ncol=lenq)
  
  lambda <- runif(lenq, min=0.01)
  
  #TODO: generation of complex eigenvalues results in complex values in F. This shouldn't be the case.
  #  ##generate eigenvalues within unit circle. 
  #  
  #  if((lenq%%2) == 1) {
  #    
  #    if(lenq==1 || lenq==3) {
  #      lambda <- runif(lenq, min=0.01)      
  #    } else {
  #      
  #      theta <- runif((lenq%/%2) - 1, min=0, max=2*pi)
  #      r <- runif((lenq%/%2) - 1, min=0.01)
  #      
  #      lambda <- complex(real=r*cos(theta), imaginary=r*sin(theta))
  #      lambda <- c(lambda, Conj(lambda))
  #      
  #      lambda <- c(lambda, runif(3, min=0.01))  
  #      
  #    }
  #  } else {
  #    
  #    theta <- runif(lenq%/%2, min=0, max=2*pi)
  #    r <- runif(lenq%/%2, min=0.01)
  #    
  #    lambda <- complex(real=r*cos(theta), imaginary=r*sin(theta))
  #    lambda <- c(lambda, Conj(lambda))
  #    
  #  }
  
  F <- companionMatrixFromEigenvalues(seq(1,1,length=lenq), lambda, c, dim)
  
  F[which(Mod(F)<1e-10)] <- 0
  
  if(sum(Mod(Im(F)))> 1e-10) {
    warning("generateRandomStableCompanionMatrix: Companion matrix is not real-valued")
    print(F)
  }
  
  F <- Re(F)
  
  list(F=F, lambda=lambda)
}

#' This function can be used to simulate data from a random, stable VAR process.
#' 
#' @title Simulate data from stable VAR process 
#' @param dim the dimension of the VAR (bivariate, trivariate,...)
#' @param order the order of the VAR (how many lags)
#' @param sd the standard deviation of the noise to be included
#' @param length the length of the series to be generated
#' @return a matrix containing the simulated data
#' @references
#' G.N. Boshnakov, B.M. Iqelan (2009). Generation of time series models with given spectral properties. Journal of Time Series Analysis 30(3):349-368. 
#' @export
simulateStableVarProcess <- function(dim=3, order=2, sd=1, length=1000) {
  
  compMat <- generateRandomStableCompanionMatrix(dim=dim, order=order)
  
  A <- list()
  for(i in 1:order) {
    
    A[[i]] <- compMat$F[1:dim, ((i-1)*dim+1):(i*dim)]
    
  }
  #A
  
  l <- length+100
  
  repeat {
    
    #------
    #generate random covariance structure and noise using this covariance structure
    cov <- matrix(runif(dim*dim, min=-1), ncol=dim)
    diag(cov) <- 1
    cov <- cov*t(cov)
    
    L <- try(chol(cov))
    if(class(L) != "try-error") break
  }  
  
  L <- chol(cov)
  
  u <- t(L) %*% matrix(rnorm(l*dim, sd=sd), nrow=dim)
  #------
  
  y <- matrix(0, ncol=l, nrow=dim)
  
  y[,1:order] <- rnorm(dim*order, sd=sd)
  
  for(i in (order+1):l) {
    
    y[,i] <- u[,i-order]
    
    for(j in 1:order) {
      y[,i] <- y[,i] + A[[j]]%*%y[,i-j]
    }
    #y[,i] <- A1%*%y[,i-1] + A2%*%y[,i-2] + u[,i-2]
  }
  
  y[,101:l]
}