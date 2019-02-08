proportion_plot <-
  function(ds) {
    require(reshape2)
    require(ggplot2)
    ds <- as.data.frame(ds)
    ds[ ] <- lapply(ds, unlist)
    ds$method <- rownames(ds)
    dsm <- melt(ds)
    colnames(dsm) <- c("method", "Result","val")
    dsm$Result <- gsub("left|probLeft|winLeft","Rep-Holdout loses",dsm$Result)
    dsm$Result <- gsub("rope|probRope|winRope","draw",dsm$Result)
    dsm$Result <- gsub("right|probRight|winRight","Rep-Holdout wins",dsm$Result)
    dsm$Result <-
      factor(dsm$Result, levels = c("Rep-Holdout wins",
                                       "draw",
                                       "Rep-Holdout loses"))
    
    ggplot(dsm, aes(x = method,
                    y = val,
                    fill = Result)) +
      geom_col(position = "fill") +
      ylab("Proportion of probability") +
      xlab("") +
      theme_minimal() +
      theme(axis.text.x  = element_text(angle = 30,
                                        size = 11,
                                        hjust = 1),
            legend.position = "top") +
      theme(axis.text.y  = element_text(size = 11),
            axis.title.y = element_text(size = 11))# +
      #scale_fill_hue(h = c(180, 300))
    #scale_fill_grey()
    #scale_fill_manual( values=c("red","green","blue"))
    #scale_fill_brewer(palette = "Set1")
  }

percdiff_plot <- 
  function(fresults) {
    require(reshape2)
    require(ggplot2)
    
    log_trans <- function(x) sign(x) * log(abs(x) + 1)
    
    r <- melt(fresults)
    
    ggplot(r, aes(x=1,y= value)) +
      facet_wrap( ~ variable,
                  nrow = 1,
                  scales="fixed") +
      geom_boxplot() +
      geom_hline(yintercept = 0,col="red") +
      theme_minimal() +
      labs(x="",
           y="Percentual difference to true loss") +
      theme(axis.text.x  = element_blank()) +
      theme(axis.text.y  = element_text(size = 14),
            axis.title.y = element_text(size = 14))
  }


percdiff_plot_log <- 
  function(fresults) {
    require(reshape2)
    require(ggplot2)
    
    log_trans <- function(x) sign(x) * log(abs(x) + 1)
    
    r <- melt(fresults)
    
    ggplot(r, aes(x=1,y= log_trans(value))) +
      facet_wrap( ~ variable,
                  nrow = 1,
                  scales="fixed") +
      geom_boxplot() +
      geom_hline(yintercept = 0,col="red") +
      theme_minimal() +
      labs(x="",
           y="Percentual difference to true loss") +
      theme(axis.text.x  = element_blank()) +
      theme(axis.text.y  = element_text(size = 14),
            axis.title.y = element_text(size = 14))
  }


avg_rank_plot <- 
  function(avg, sdev) {
    require(reshape2)
    require(ggplot2)
    
    ord <- names(sort(avg))
    
    methods <- names(avg)
    
    ds <- data.frame(avg=avg,sdev=sdev, methods=methods, row.names = NULL)
    ds$methods <- factor(ds$methods, levels = ord)
    
    #ds <- melt(ds)
    
    ggplot(data = ds,
           aes(x = methods,
               y = avg)) +
      geom_bar(stat="identity",
               fill="#33CCCC") +
      theme_minimal() +
      theme(axis.text.x  = element_text(angle = 35,
                                        size = 14,
                                        hjust = 1)) +
      theme(axis.text.y  = element_text(size = 12),
            axis.title.y = element_text(size = 12)) +
      #geom_hline(yintercept = 1) +
      geom_errorbar(aes(ymin = avg - sdev,
                        ymax = avg + sdev),
                    width = .5,
                    position = position_dodge(.9)) +
      labs(x="",
           y="Avg. rank & Std dev.",
           title = "")
  }