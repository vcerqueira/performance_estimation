load("data/rw-datasets.rdata")

source("src/utils.r")
source("src/estimation-procedures.r")
source("src/workflows.r")
source("src/metrics.r")
source("src/learning-models.r")

library(tsensembler)

form <- target~.
nfolds <- 10

final_results <- vector("list", length(embedded_time_series))
for (i in seq_along(final_results)) {
  cat(i, "\n\n")
  ds <- embedded_time_series[[i]]
  
  x <-
    workflow(
      ds = ds,
      form = form,
      predictive_algorithm = "gp",
      nfolds = nfolds,
      outer_split = .8
    )
  
  final_results[[i]] <- x
}
save(final_results, file = "final_results_main_gp.rdata")

