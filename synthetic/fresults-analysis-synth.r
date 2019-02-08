load("/Users/vcerqueira/Desktop/final_results_synthetic_ts1_preq_rbr.rdata")
final_results2<-final_results
load("final_results_synthetic_ts1_rbr.rdata")


source("src/correlated-t-test.r")
source("src/plots.r")

err_estimation <- 
  lapply(final_results,
         function(x) {
           x$err_estimation
         })

err_estimation2 <- 
  lapply(final_results2,
         function(x) {
           x$err_estimation
         })

for (iter in seq_along(err_estimation)) {
  err_estimation[[iter]] <- 
    c(err_estimation[[iter]], err_estimation2[[iter]])
}

nms <- names(final_results[[1]][[1]])
names(final_results2[[1]][[1]])
fr <- do.call(rbind.data.frame, err_estimation)
colnames(fr) <- nms
colnames(fr) <- 
  c("CV", "CV-Bl", "CV-Mod","CV-hvBl",
    "Preq-Bls", "Preq-Sld-Bls",
    "Preq-Bls-Gap","Holdout", "Rep-Holdout",
    "Preq-Grow","Preq-Slide")

fr_abs <- abs(fr)

fr_abs_rank <- apply(fr_abs, 1, rank)
avg <- rowMeans(fr_abs_rank)
sdev <- apply(fr_abs_rank,1, sd)

avg_rank_plot(avg,sdev)
percdiff_plot_log(fr)

colMeans(fr)
apply(fr,2, sd)


