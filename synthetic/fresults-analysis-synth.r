load("/Users/vcerqueira/Desktop/Results/final_results_synthetic_ts1_preq_rbr.rdata")
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

avg_rank_plot <-
  function(avg, sdev) {
    require(reshape2)
    require(ggplot2)
    
    ord <- names(sort(avg))
    
    methods <- names(avg)
    
    ds <-
      data.frame(
        avg = avg,
        sdev = sdev,
        methods = methods,
        row.names = NULL
      )
    ds$methods <- factor(ds$methods, levels = ord)
    
    #ds <- melt(ds)
    
    ggplot(data = ds,
           aes(x = methods,
               y = avg)) +
      geom_bar(stat = "identity",
               fill = "#33CCCC") +
      theme_minimal() +
      theme(axis.text.x  = element_text(
        angle = 45,
        size = 14,
        hjust = 1
      )) +
      theme(axis.text.y  = element_text(size = 12),
            axis.title.y = element_text(size = 12)) +
      #geom_hline(yintercept = 1) +
      geom_errorbar(aes(ymin = avg - sdev,
                        ymax = avg + sdev),
                    width = .5,
                    position = position_dodge(.9)) +
      labs(x = "",
           y = "Avg. rank & Std dev.",
           title = "")
  }

avg_rank_plot(avg,sdev)
percdiff_plot_log(fr)

colMeans(fr)
apply(fr,2, sd)


