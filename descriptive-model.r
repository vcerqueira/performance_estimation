load("data/tsl_uni_90_mix.rdata")
load("best_estimator.rdata")
load("series_length.rdata")
load("stationarity_tsdl.rdata")

source("src/dynamics.r")

library(rpart)
library(rattle)
library(party)
library(ranger)

Z <- lapply(ts_list, function(x) dynamics_ts(tail(x,1000)))
names(Z) <- paste0("TS_", seq_along(Z))


Z <- do.call(rbind, Z)
Z$estimator <- best_estimator
#save(Z, file = "metadata.rdata")
#load("metadata.rdata")
colnames(Z) <- c(
  "Trend","Qt05","Qt95","IQR","Skewness","Kurtosis",
  "MLE","Hurst","N","SerialCorr","estimator"
)
Z$Stationarity <- as.character(as.integer(is_stationary_2ensemble))

Z$estimator <- as.factor(ifelse(grepl("CV",Z$estimator), "CVAL","OOS"))

library(rpart)
library(rpart.plot)

m <- rpart(estimator ~., Z,
           control = list(cp=.028))
rpart.plot(m,type=2, extra=2,
           clip.right.labs = F, 
           fallen.leaves=F,cex=0.8,
           under = F)






