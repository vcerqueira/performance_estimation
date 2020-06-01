load("final_results_synthetic_ts1_lasso.rdata")
load("final_results_synthetic_ts2_lasso.rdata")
load("final_results_synthetic_ts3_lasso.rdata")
load("final_results_synthetic_ts1_rbr.rdata")
load("final_results_synthetic_ts2_rbr.rdata")
load("final_results_synthetic_ts3_rbr.rdata")
load("final_results_synthetic_ts1_rf.rdata")
load("final_results_synthetic_ts2_rf.rdata")
load("final_results_synthetic_ts3_rf.rdata")

source("src/plots.r")
library(scmamp)

err_estimation <- 
  lapply(final_results,
         function(x) {
           tryCatch(x$err_estimation,
                    error =function(e) {NULL})
         })

err_estimation <- err_estimation[!sapply(err_estimation, is.null)]

fr <- do.call(rbind, err_estimation)
fr <- as.data.frame(fr)
rownames(fr) <- NULL

colnames(fr) <- 
  c("CV", "CV-Bl", "CV-Mod","CV-hvBl",
    "Preq-Bls", "Preq-Sld-Bls",
    "Preq-Bls-Gap","Holdout", "Rep-Holdout",
    "Preq-Slide","Preq-Grow")

fr_abs <- abs(fr)
fr_abs_rank <- apply(fr_abs, 1, rank)

avg_rank_plot(avg = rowMeans(fr_abs_rank),
              sdev = apply(fr_abs_rank,1, sd)) + 
  theme(axis.text.x  = element_text(angle = 45,
                                    size = 12,
                                    hjust = 1))

#
