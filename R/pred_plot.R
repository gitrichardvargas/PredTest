predplot <- function(predictionresults, weights, order = FALSE){

  if (missing(weights))
  {

    end <- predictionresults[[3]]
    diff <- unlist(predictionresults[2])
    resulted <- unlist(predictionresults[1])

    forplot <- as.data.frame(   cbind(end, diff  , resulted  )   )
    forplot$diff <- as.numeric(as.character(forplot$diff))
    forplot$resulted <- as.numeric(as.character(forplot$resulted))

    ymax <- max(1.25*max(forplot$diff), 0+.5*sd(forplot$diff))
    ymin <- min(1.25*min(forplot$diff), 0-.5*sd(forplot$diff))


    if (order == FALSE){

      forplot1 <- ggplot(data = forplot, aes(end, diff, fill = factor(resulted)   )  ) +
        geom_bar(aes(x = end, y = diff),stat='identity') +
        scale_y_continuous(limits=c(ymin,ymax)) +
        geom_bar(forplot, mapping = aes(end) ,alpha=0, size=1, color="black", stat='identity')+
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        labs( x = "", y = "Difference", fill = "Prediction\nresults") +
        scale_fill_manual(values = c('0' = "White",'1'= "Black"), labels = c("Incorrect", "Correct"), drop = FALSE) +
        theme(plot.title = element_text(hjust = 0.5))

    } else if (order == TRUE){




      forplot1 <- ggplot(data = forplot, aes(x=reorder(end, -diff), y=diff, fill = factor(resulted)   )  ) +
        geom_bar(stat='identity') +
        scale_y_continuous(limits=c(ymin,ymax)) +
        geom_bar(forplot, mapping = aes(end) ,alpha=0, size=1, color="black", stat='identity')+
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        labs( x = "", y = "Difference", fill = "Prediction\nresults") +
        scale_fill_manual(values = c('0' = "White",'1'= "Black"), labels = c("Incorrect", "Correct"), drop = FALSE) +
        theme(plot.title = element_text(hjust = 0.5))


    }
  } else {

    end <- predictionresults[[3]]
    diff <- unlist(predictionresults[2])*unlist(weights)
    resulted <- unlist(predictionresults[1])

    forplot <- as.data.frame(   cbind(end, diff  , resulted  )   )
    forplot$diff <- as.numeric(as.character(forplot$diff))
    forplot$resulted <- as.numeric(as.character(forplot$resulted))

    ymax <- max(1.25*max(forplot$diff), 0+.5*sd(forplot$diff))
    ymin <- min(1.25*min(forplot$diff), 0-.5*sd(forplot$diff))

    if (order == FALSE)
    {

      forplot1 <- ggplot(data = forplot, aes(end, diff, fill = factor(resulted)   )  ) +
        geom_bar(aes(x = end, y = diff),stat='identity') +
        scale_y_continuous(limits=c(ymin,ymax)) +
        geom_bar(forplot, mapping = aes(end) ,alpha=0, size=1, color="black", stat='identity')+
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        labs( x = "", y = "Weighted difference", fill = "Prediction\nresults") +
        scale_fill_manual(values = c('0' = "White",'1'= "Black"), labels = c("Incorrect", "Correct"), drop = FALSE) +
        theme(plot.title = element_text(hjust = 0.5))
    } else if (order == TRUE){

      forplot1 <- ggplot(data = forplot, aes(x=reorder(end, - diff), y = diff, fill = factor(resulted)   )  ) +
        geom_bar(stat='identity') +
        scale_y_continuous(limits=c(ymin,ymax)) +
        geom_bar(forplot, mapping = aes(end) ,alpha=0, size=1, color="black", stat='identity')+
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
        labs( x = "", y = "Weighted difference", fill = "Prediction\nresults") +
        scale_fill_manual(values = c('0' = "White",'1'= "Black"), labels = c("Incorrect", "Correct"), drop = FALSE) +
        theme(plot.title = element_text(hjust = 0.5))

    }

  }

  return(forplot1)

}
