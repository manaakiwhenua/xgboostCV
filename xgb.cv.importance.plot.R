##############################################
###Bar plot of variable importance in cross-validated xgboost models
###Requires list of predictors to be in same order as in columns of
###CVtrain_x
###Records mean and 95%ci of 3 importance metrics provided by 
###xgb.importance() function
##############################################
xgb.cv.importance.plot = function(cv, #ouput from xgb.cv. Be sure to use callback to save cv models
                       Nfolds, #number of fold models used in cross-validaton
                       Predictors,#names of predictor variables in xgb.cv model)
                       PredictorLabels = Predictors, #optional laels for plotting
                       Filename)#location to print bar plot 
  {
  ###Use rbind to combine results for fold models in single 
  ###dataframe, the print separate bar plot for each metric of importance
  ###looks better if predictor names kept short
  ImportanceList = list()
  Importance = as.data.frame(matrix(nrow = 0,ncol = 4))
  colnames(Importance) = c("Feature","Gain","Cover","Frequency")
  for(fold in 1:Nfolds)
    {
    ImportanceFrame = as.data.frame(matrix(nrow = length(Predictors),ncol = 4))
    colnames(ImportanceFrame) = colnames(Importance)
    ImportanceFrame[,]=0
    ImportanceFrame[,1] = PredictorLabels
    FoldImportance <- as.data.frame(xgb.importance(model = cv$models[[fold]],feature_names = Predictors))
    for(predictor in 1:length(Predictors))
      if(Predictors[predictor] %in% FoldImportance$Feature)
        ImportanceFrame[predictor,2:4] = FoldImportance[FoldImportance$Feature==Predictors[predictor],2:4]
    Importance <- rbind(Importance,ImportanceFrame)
    }
  MeanImportance <- aggregate.data.frame(Importance[,2:4],by = list(Importance[,1]),FUN = mean)
  SDImportance <- aggregate.data.frame(Importance[,2:4],by = list(Importance[,1]),FUN = sd)
  SDImportance[,2:4] <- SDImportance[,2:4]/Nfolds^0.5*1.96
  
  png(Filename,width = 1200, height = 1600)
  par(mfcol = c(3,1), cex.main = 4, cex.lab = 3, cex.axis = 3, mar = c(20,8,8,2), 
      mgp = c(4,1,0),font.lab = 2,oma = c(0, 0, 9, 0))
  MetricNames <- colnames(MeanImportance)[2:ncol(MeanImportance)]
  
  for(i in seq_along(MetricNames))
  {
    metric <- i + 1
    
    Mean <- MeanImportance[,metric]
    SD <- SDImportance[,metric]
    
    Ymax <- max(Mean + SD)
    Ymin <- 0
    
    mp <- barplot(
      Mean,
      main = MetricNames[i],
      ylim = c(Ymin, Ymax),
      names = NULL,
      ylab = MetricNames[i],
      lwd = 3,
      col = "gray80"
    )
    
    wrap_no_split <- function(x, width = 12) {
      sapply(strsplit(x, " "), function(words) {
        line <- ""
        out <- c()
        
        for (w in words) {
          test <- if (nzchar(line)) paste(line, w) else w
          
          if (nchar(test) <= width) {
            line <- test
          } else {
            out <- c(out, line)
            line <- w
          }
        }
        
        c(out, line) |> paste(collapse = "\n")
      })
    }
    
    labs <- wrap_no_split(MeanImportance[,1], width = 12)
    
    mtext(labs, side = 1, at = mp, cex = 2, las = 2, line = 1)
    abline(h = 0, lwd = 3)
    
    segments(mp, Mean+SD, mp, Mean-SD, lwd=2)
    segments(mp - 0.1, Mean+SD, mp + 0.1, Mean+SD, lwd=2)
    segments(mp - 0.1, Mean-SD, mp + 0.1, Mean-SD, lwd=2)
    
    ImportanceDF <- data.frame(
      Predictor = MeanImportance[,1],
      Mean = Mean,
      SD = SD
    )
    
    ImportanceList[[MetricNames[i]]] <- ImportanceDF
  }
  
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 10, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  mtext("Predictor importance across cross-validated fold models", side = 3, cex = 3.5)
  dev.off()
  return(ImportanceList)
}
