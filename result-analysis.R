load("final_results_rbr_all_nd.rdata")
load("final_results_lasso_all_nd.rdata")
load("final_results_rf_all_nd.rdata")

load("stationarity_tsdl.rdata")

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

is_stationary_final <- is_stationary_2ensemble
table(is_stationary_final)

## Subsetting ##########################################
fr_final <- fr[!is_stationary_final,]
fr_final <- fr[is_stationary_final,]
fr_final <- fr

########################################################

colnames(fr_final) <- 
  c("CV", "CV-Bl", "CV-Mod","CV-hvBl",
    "Preq-Bls", "Preq-Sld-Bls",
    "Preq-Bls-Gap","Holdout", "Rep-Holdout",
    "Preq-Slide","Preq-Grow")
fr_abs <- abs(fr_final)
rownames(fr_abs) <- NULL

# best_estimator <- colnames(fr_abs)[apply(fr_abs,1,which.min)]
# save(best_estimator, file = "best_estimator.rdata")
#table(best_estimator)

fr_abs_rank <- apply(fr_abs, 1, rank)

# bp_dist(t(fr_abs_rank)[!is_stationary_final,])
# bp_dist(t(fr_abs_rank)[is_stationary_final,])
# bp_dist(t(fr_abs_rank))

avg_rank_plot(avg = rowMeans(fr_abs_rank),
              sdev = apply(fr_abs_rank,1, sd)) + 
  theme(axis.text.x  = element_text(angle = 45,
                                    size = 12,
                                    hjust = 1))

rm(fr_final)

baseline <- "Rep-Holdout"

cID <- which(colnames(fr_abs) %in% baseline)

PerfDiff <- lapply(as.data.frame(fr_abs),
                   function(x) x-fr_abs[,cID,drop=T])

PerfDiff <- as.data.frame(PerfDiff[-cID])

rope <- 2.5
baout <-
  lapply(PerfDiff,
         function(u) {
           bSignedRankTest(u, 
                           rope=c(-rope,rope))$posterior.probabilities
         })

baout <- lapply(baout,unlist)
baout <- do.call(rbind, baout)
rownames(baout) <- gsub("\\.","-",rownames(baout))
colnames(baout) <- c("probLeft","probRope","probRight")
proportion_plot(baout) + scale_fill_brewer(palette="Set2")


#
colnames(fr) <- colnames(fr_abs)
percdiff_plot_log(fr)

