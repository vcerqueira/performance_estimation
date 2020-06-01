load("data/tsl_uni_90_mix.rdata")

source("src/utils.r")
source("src/estimation-procedures.r")
source("src/workflows.r")
source("src/metrics.r")
source("src/learning-models.r")

library(tsensembler)

form <- target~.
nfolds <- 30

library(parallel)
final_results <-
  mclapply(1:length(ts_list),
         function(i) {
           cat(i, "\n\n")
           ds <- ts_list[[i]]
           
           x <-
             workflow(
               ds = ds,
               form = form,
               predictive_algorithm = "rbr",
               nfolds = nfolds,
               outer_split = .7)
           
           x
         }, mc.cores = 10)

save(final_results, file = "final_results_rbr.rdata")

