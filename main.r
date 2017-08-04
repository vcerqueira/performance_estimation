
source("Blind_Repo_CVE/estimation-procedures.r")
source("Blind_Repo_CVE/learning-workflows.r")
source("Blind_Repo_CVE/metrics.r")
source("Blind_Repo_CVE/performance-estimation.R")
source("Blind_Repo_CVE/utils.r")

library(performanceEstimation)
library(scmamp)

S <- readRDS("Blind_Repo_CVE/synthetic_timeseries.rds")

S1 <- S[[1]]
S2 <- S[[2]]


S1e <- lapply(S1,embed.timeseries, embedding.dimension=5)
S2e <- lapply(S2,embed.timeseries, embedding.dimension=5)



RES_S1_glm <- lapply(seq_along(S1e), function(i) {
  cat(i,"\n")
  ts_estimation(S1e[[i]], M.glm, workflow.glm)
})

RES_S1_nn <- lapply(seq_along(S1e), function(i) {
  cat(i,"\n")
  ts_estimation(S1e[[i]], M.nnet, workflow.nn)
})

RES_S1_dt <- lapply(seq_along(S1e), function(i) {
  cat(i,"\n")
  ts_estimation(S1e[[i]], M.dt, workflow.dt)
})

##

RES_S2_glm <- lapply(seq_along(S2e), function(i) {
  cat(i,"\n")
  ts_estimation(S2e[[i]], M.glm, workflow.glm)
})

RES_S2_nn <- lapply(seq_along(S2e), function(i) {
  cat(i,"\n")
  ts_estimation(S2e[[i]], M.nnet, workflow.nn)
})

RES_S2_dt <- lapply(seq_along(S2e), function(i) {
  cat(i,"\n")
  ts_estimation(S2e[[i]], M.dt, workflow.dt)
})


RES_S1_glm.abs <- lapply(RES_S1_glm, abs)


task <- PredTask(t~., data.frame(t=1))
estTask <- EstimationTask(metrics = colnames(RES_S1_glm.abs[[1]]))
workflow <- Workflow(wf = "na")
iterationsInfo <- list()

CR_s1 <- lapply(seq_along(RES_S1_glm.abs), function(k) {
  task <- PredTask(t~., data.frame(t=1))
  x_er <- apply(RES_S1_glm.abs[[k]], 1, function(j) {
    EstimationResults(task, workflow, estTask, t(as.matrix(j)), list())
  })
  
  ComparisonResults(list(x_er))
})

CR_s1 <- ComparisonResults(sapply(CR_s1, c))
pc_s1 <- pairedComparisons(CR_s1)

names_methods <- c("CV.KF",
                   "CV.BKF",
                   "CV.MKF",
                   "CV.hvBFK",
                   "OOS.PB",
                   "OOS.SW",
                   "OOS.GW",
                   "OOS.H",
                   "OOS.MC60",
                   "OOS.MC20")

colnames(pc_s1$rmse$avgScores) <- names_methods
colnames(pc_s1$mae$avgScores) <- names_methods

par(mfrow=c(1,2))

plotCD(-pc_s1$rmse$avgScores)
plotCD(-pc_s1$mae$avgScores)

