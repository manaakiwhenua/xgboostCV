##################################################
###Simple scatter plot of observed and predicted responses
###Designed for easy inspection of xgb.cv predictions
##################################################
xgbm.cv.fit.scatterplot = function(pred,CVtrain_y,path)
  {
  Cor = round(cor(pred,CVtrain_y),digits = 3)
  Title = paste0("Cor = ",Cor)
  Filename = paste0(path,"FitScatterplot.png")
  png(Filename, height = 1600,width = 1600)
  par(mar = c(10,12,12,2), cex.main = 5,cex.lab = 4.8,cex.axis = 4.6,mgp = c(7,2,0))
  plot(pred~CVtrain_y, main = Title,
          xlab = paste0("Observed response"),ylab = paste0("Fitted response"), pch = NA)
  points(CVtrain_y,pred, cex = 2, col = 1)
  abline(0,1,col = 2,lwd = 3)
  dev.off()
  }
