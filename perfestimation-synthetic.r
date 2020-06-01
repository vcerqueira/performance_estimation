load("data/synthetic.rdata")

source("src/utils.r")
source("src/estimation-procedures.r")
source("src/workflows.r")
source("src/metrics.r")
source("src/learning-models.r")

library(tsensembler)

form <- target ~ .
nfolds <- 30
embedded_time_series <- synthetic$TS3

library(parallel)
final_results <- 
  mclapply(1:length(embedded_time_series),
         function(i) {
           cat(i, "\n\n")
           ds <- embedded_time_series[[i]]
           
           x <-
             workflow(
               ds = ds,
               form = form,
               predictive_algorithm = "rf",
               nfolds = nfolds,
               outer_split = .8, 
               is_embedded = T
             )
           
           x
         }, mc.cores = 3)

save(final_results, 
     file = "final_results_synthetic_ts3_rf.rdata")

