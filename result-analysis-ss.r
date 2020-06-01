load("final_results_samplesize_rbr.rdata")
load("stationarity_tsdl.rdata")
load("series_length.rdata")

# err_estimation <- 
#   lapply(final_results[!(is_stationary_2ensemble[len>1000])],
#          function(x) {
#            x <- do.call(rbind, x)
#            rnk <- t(apply(abs(x),1,rank))
#            rnk
#          })

err_estimation <- 
  lapply(final_results,
         function(x) {
           x <- do.call(rbind, x)
           rnk <- t(apply(abs(x),1,rank))
           rnk
         })

err_arr <- simplify2array(err_estimation)
err_ovr <- apply(err_arr,1:2,mean)

library(tsensembler)
err_ovr <- 
  roll_mean_matrix(as.data.frame(err_ovr), 2)


colnames(err_ovr) <- 
  c("CV", "CV-Bl", "CV-Mod","CV-hvBl",
    "Preq-Bls", "Preq-Sld-Bls",
    "Preq-Bls-Gap","Holdout", "Rep-Holdout",
    "Preq-Slide","Preq-Grow")

df <- t(round(err_ovr,2))
df <- as.data.frame(df)
df$Method <- as.factor(rownames(df))
rownames(df) <- NULL
colnames(df) <- c(as.character(seq(from=100,to=900,by=100)),"Method")

library(hrbrthemes)
library(GGally)
library(viridis)
ggparcoord(df,
           columns = 1:9, groupColumn = 10, 
           scale = "globalminmax",
           showPoints = TRUE, 
           alphaLines = 0.9
) + theme_minimal() +
  labs(x="Training Sample Size",
       y= "Average Rank")

