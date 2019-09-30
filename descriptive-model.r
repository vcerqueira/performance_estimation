load("data/timeseries.rdata")
load("best_estimator.rdata")

source("src/dynamics.r")

library(rpart)
library(rattle)
library(party)
library(ranger)

Z <- lapply(ts_list, function(x) dynamics_ts(head(x,500)))

names(Z) <- paste0("TS_", seq_along(Z))

Z <- do.call(rbind, Z)
Z$estimator <- best_estimator

m <- rpart(estimator ~., Z,
           control = list(minsplit=5,
                          cp=.01))
print(m)
fancyRpartPlot(m)
