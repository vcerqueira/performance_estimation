source("synthetic/simulate-ts.r")
source("src/utils.r")

library(forecast)
library(tsensembler)

mcreps <- 1000; 
seq. <- seq_len(mcreps)
ts.len <- 200

TS1 <- lapply(seq., function(j) {
  as_positive(
    as.vector(
      simulateLinearTS(ts.len,
                       ar = TRUE,
                       ma = FALSE,
                       lags = 3,
                       maxRoot = 5,
                       n.start = 300)[["ts"]]
    )
  )
})

TS2 <- lapply(seq., function(j) {
  as_positive(
    as.vector(
      simulateLinearTS(ts.len,
                       ar = FALSE,
                       ma = TRUE,
                       lags = 1,
                       maxRoot = 5,
                       n.start = 100)[["ts"]]
    )
  )
})

data(USAccDeaths)
Y <- as.vector(USAccDeaths)
arima.fit <- Arima(Y, order=c(12,0,0), seasonal = c(1, 0, 0))

TS3 <- lapply(seq., function(j) {
  as.vector(
    simulate(object = arima.fit,
             nsim = ts.len)
  )
})

TS1 <- lapply(TS1, embed_timeseries, embedding.dimension = 5)
TS2 <- lapply(TS2, embed_timeseries, embedding.dimension = 5)
TS3 <- lapply(TS3, embed_timeseries, embedding.dimension = 5)

synthetic <- list(TS1=TS1, TS2=TS2,TS3=TS3)


plot_df(synthetic$TS1[[180]]$target)
plot_df(synthetic$TS2[[110]]$target)
plot_df(synthetic$TS3[[2]]$target)

plot_df <- 
  function(x) {
    df <- as.data.frame(x)
    df$Time <- 1:nrow(df)
    
    ggplot(data=df, aes(x=Time, y=x)) +
      geom_line() +
      theme_minimal() +
      theme(axis.title.y = element_blank(),
            axis.text = element_text(size=12))
    
  }



save(synthetic, file = "data/synthetic.rdata")
