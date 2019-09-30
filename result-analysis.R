load("/Users/vcerqueira/Desktop/final_results_main_rbr_2509.rdata")

source("src/bayesian-analysis.r")
source("src/plots.r")

err_estimation <- 
  lapply(final_results,
         function(x) {
           x$err_estimation
         })

fr <- do.call(rbind, err_estimation)
fr <- as.data.frame(fr)

colnames(fr) <- 
  c("CV", "CV-Bl", "CV-Mod","CV-hvBl",
    "Preq-Bls", "Preq-Sld-Bls",
    "Preq-Bls-Gap","Holdout", "Rep-Holdout",
    "Preq-Grow","Preq-Slide")
fr_abs <- abs(fr)

best_estimator <- colnames(fr_abs)[apply(fr_abs,1,which.min)]
save(best_estimator, file = "best_estimator.rdata")

fr_abs_rank <- apply(fr_abs, 1, rank)
rowMeans(fr_abs_rank)
apply(fr_abs_rank,1, sd)

avg_rank_plot(rowMeans(fr_abs_rank),apply(fr_abs_rank,1, sd))

baseline <- "Rep-Holdout"

cID <- which(colnames(fr_abs) %in% baseline)

PerfDiff <- lapply(fr_abs,
                   function(x) x-fr_abs[,cID,drop=T])

PerfDiff <- as.data.frame(PerfDiff[-cID])

baout <-
  lapply(PerfDiff,
         function(u) {
           BayesianSignTest(u,-2.5, 2.5)
         })


baout <- lapply(baout,unlist)
baout <- do.call(rbind, baout)
rownames(baout) <- 
  gsub("\\.","-",rownames(baout))
proportion_plot(baout)

percdiff_plot_log(fr)


##


