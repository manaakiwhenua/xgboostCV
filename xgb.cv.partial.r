################################################################
################################################################
###Functions calling partial plot function partial_dep()
###from hstats package:
###https://cran.r-project.org/web/packages/hstats/index.html
###for each fold model in a xgb.cv output
###and plotting results for each fold models as well as mean accross folds 
###Options provided for multiclass and binary/continuous responses
###discrete (binary) predictors supported
###Author Norman Mason with support from Michael Mayer
###https://github.com/mayer79/
################################################################
################################################################

library(hstats)
library(xgboost)
###Multiclass resonses
xgbm.cv.partial.multiclass = function
(cv, #output from xgb.cv
Nfolds,#number of fold models
CVtrain_x,#predictor data (as matrix)
          #can be same as data used to train model or a synthetic dataset
var,#predictor variable (integer between 1 and number of predictors)
path,#folder for saving image file
Prevalence = NA, #the proportion of observations for the class in question
                #this is a proxy for "zero effect" of the predictor
ResponseLevel = 1,
Classes
)
{
###predict(xgboost) stacks predictions for different levels of response
###in a single vector so need to apply reshape option in partial_dep function calls
Reshape = FALSE
if(is.null(dim(cv$pred))==FALSE)
  Reshape = TRUE
Predictors = colnames(CVtrain_x)

###Loop through the fold models to obtain partial dependence estimates
PartialPreds = as.data.frame(matrix(nrow = 0,ncol = 3))
colnames(PartialPreds) = c("x","y","Fold")
for(fold in 1:Nfolds)
  {
  Model = cv$models[[fold]]
  ###call partial_dep() function
  Partial = partial_dep(object = Model, v = Predictors[var], X = CVtrain_x,reshape = Reshape)
  Partial = Partial$data[,c(1,(ResponseLevel+1))]
  Partial$Fold = fold
  colnames(Partial) = colnames(PartialPreds)
  PartialPreds = rbind(PartialPreds,Partial)
  }
###Plot partial depency for each fold model and mean across fold models
PartialPredsMean = aggregate(PartialPreds[,2],by = list(PartialPreds[,1]),FUN = mean)
Title = paste0(Classes[ResponseLevel],"  ",Predictors[var])
Filename = paste0(path,Classes[ResponseLevel],".",Predictors[var],".partial.png")
png(Filename,width = 1800, height = 1600)
par(mfcol = c(1,1), cex.main = 4, cex.lab = 3, cex.axis = 3, mar = c(10,8,8,2), 
    mgp = c(4,1,0),font.lab = 2,oma = c(0, 0, 9, 0))
plot(PartialPreds[,1],PartialPreds[,2], ylim = c(0,max(PartialPreds[,2])),pch = NA,
     xlab = Predictors[var], ylab = paste0(Classes[ResponseLevel]),main = Title)
for(fold in 1:Nfolds)
  lines(PartialPreds[PartialPreds$Fold == fold,1],PartialPreds[PartialPreds$Fold == fold,2],lty=3,col = fold,lwd = 3)
lines(PartialPredsMean[,1],PartialPredsMean[,2],lty=1,col = 1,lwd = 10)
###Prevalence is proxy for neutral partial effect of predictors
###Acts as a check the partial effects are estimated correctly
if(is.na(Prevalence) == T)
  abline(h=1/length(Classes),lwd = 8, lty = 2,col = 2)
if(is.na(Prevalence) == F)
  abline(h=Prevalence,lwd = 8, lty = 2,col = 2)
dev.off()
}

###Binary and continuous responses
xgbm.cv.partial = function(
    cv,
    Nfolds,
    CVtrain_x,
    var,
    CVtrain_y,
    Response,
    ResponseName = "yhat",
    PredictorLabels = colnames(CVtrain_x),
    PredictorFileName,
    ylim = NULL,
    ReturnData = FALSE,
    PartialPreds = NULL,
    PartialDir,
    PlotPoints = FALSE,
    CI,
    PlotMeanResponse = TRUE
)
{
  MeanY = mean(na.omit(CVtrain_y))
  Predictors = colnames(CVtrain_x)
  
  ### Align predictor labels to model predictor order
  PredictorLabels = PredictorLabels[Predictors]
  PredictorLabels[is.na(PredictorLabels)] = Predictors[is.na(PredictorLabels)]
  
  
  ### Calculate partial dependence if not supplied
  if(is.null(PartialPreds))
  {
    PartialPreds = as.data.frame(matrix(nrow = 0,ncol = 3))
    colnames(PartialPreds) = c("x","y","Fold")
    
    for(fold in 1:Nfolds)
    {
      Model = cv$models[[fold]]
      
      Partial = partial_dep(
        object = Model,
        v = Predictors[var],
        X = CVtrain_x
      )
      
      Partial = Partial$data
      Partial$Fold = fold
      
      colnames(Partial) = colnames(PartialPreds)
      
      PartialPreds = rbind(PartialPreds,Partial)
    }
  }
  
  
  ### Return PD data only
  if(ReturnData)
    return(PartialPreds)
  
  
  ### Summarise partial predictions across folds
  PartialSummary = aggregate(
    y ~ x,
    data = PartialPreds,
    FUN = function(z)
    {
      c(
        mean = mean(z),
        sd = sd(z),
        n = length(z)
      )
    }
  )
  
  PartialSummary = data.frame(
    x = PartialSummary$x,
    Mean = PartialSummary$y[, "mean"],
    SD = PartialSummary$y[, "sd"],
    N = PartialSummary$y[, "n"]
  )
  
  
  ### Calculate confidence intervals only if requested
  if(!is.na(CI))
  {
    z = qnorm(1 - (1-CI)/2)
    
    PartialSummary$Lower =
      PartialSummary$Mean -
      z * PartialSummary$SD / sqrt(PartialSummary$N)
    
    PartialSummary$Upper =
      PartialSummary$Mean +
      z * PartialSummary$SD / sqrt(PartialSummary$N)
  }
  
  
  Title = PredictorLabels[var]
  
  Filename = paste0(
    PartialDir,
    Response,
    ".",
    PredictorFileName,
    ".partial.png"
  )
  
  
  ### Determine plotting limits
  if(is.null(ylim))
  {
    if(!is.na(CI))
    {
      ylim = range(
        c(
          PartialSummary$Lower,
          PartialSummary$Upper,
          MeanY
        ),
        na.rm = TRUE
      )
    }
    else
    {
      ylim = range(
        c(
          PartialSummary$Mean,
          MeanY
        ),
        na.rm = TRUE
      )
    }
  }
  
  
  png(
    Filename,
    width = 1800,
    height = 1600
  )
  
  par(
    mfcol = c(1,1),
    cex.main = 5,
    cex.lab = 4.8,
    cex.axis = 4.6,
    mar = c(10,12,8,8),
    mgp = c(7,2,0),
    font.lab = 2,
    oma = c(0,0,9,0)
  )
  
  
  plot(
    PartialSummary$x,
    PartialSummary$Mean,
    ylim = ylim,
    type = "n",
    xaxt = "n",
    xlab = PredictorLabels[var],
    ylab = ResponseName,
    main = Title
  )
  ### Draw x-axis
  if(PlotPoints)
  {
    ObsValues = sort(unique(CVtrain_x[, Predictors[var]]))
    AutoTicks = axTicks(1)
    
    if(length(ObsValues) <= length(AutoTicks))
    {
      axis(
        side = 1,
        at = ObsValues,
        labels = ObsValues
      )
    }
    else
    {
      axis(1)
    }
  }
  else
  {
    axis(1)
  }
  
  if(PlotPoints)
  {
    ### Point estimates with optional CI error bars
    
    if(!is.na(CI))
    {
      ### Width of horizontal error bar caps (2% of x-axis range)
      CapWidth = 0.02 * diff(range(PartialSummary$x, na.rm = TRUE))
      
      ### Vertical error bars
      segments(
        x0 = PartialSummary$x,
        y0 = PartialSummary$Lower,
        x1 = PartialSummary$x,
        y1 = PartialSummary$Upper,
        lwd = 3
      )
      
      ### Lower caps
      segments(
        x0 = PartialSummary$x - CapWidth,
        y0 = PartialSummary$Lower,
        x1 = PartialSummary$x + CapWidth,
        y1 = PartialSummary$Lower,
        lwd = 3
      )
      
      ### Upper caps
      segments(
        x0 = PartialSummary$x - CapWidth,
        y0 = PartialSummary$Upper,
        x1 = PartialSummary$x + CapWidth,
        y1 = PartialSummary$Upper,
        lwd = 3
      )
    }
    
    points(
      PartialSummary$x,
      PartialSummary$Mean,
      pch = 16,
      cex = 4
    )
    
  }
  else
  {
    ### Individual fold curves
    
    for(fold in 1:Nfolds)
    {
      lines(
        PartialPreds[PartialPreds$Fold == fold,1],
        PartialPreds[PartialPreds$Fold == fold,2],
        lty = 3,
        col = fold,
        lwd = 3
      )
    }
    
    
    ### Mean partial dependence curve
    lines(
      PartialSummary$x,
      PartialSummary$Mean,
      lwd = 10
    )
    
    
    ### Optional confidence ribbon
    if(!is.na(CI))
    {
      polygon(
        c(
          PartialSummary$x,
          rev(PartialSummary$x)
        ),
        c(
          PartialSummary$Lower,
          rev(PartialSummary$Upper)
        ),
        border = NA
      )
    }
  }
  
  
  ### Mean response reference line
  ### Mean response reference line
  if(PlotMeanResponse)
    {
    abline(
      h = MeanY,
      lwd = 8,
      lty = 2,
      col = 2
    )
    }
  
  
  dev.off()
  
  invisible(PartialSummary)
}

############################################################
## Combine PNG files into a multipanel figure
############################################################
library(png)
library(grid)
library(gridExtra)
png.multipanel <- function(
    png_dir,
    outfile,
    ncol = 2,
    width = 8,
    height = 12,
    labels = LETTERS,
    fontsize = 18
){
  
  png_files <- list.files(
    png_dir,
    pattern = "\\.png$",
    full.names = TRUE
  )
  
  if(length(png_files) == 0){
    warning("No PNG files found in ", png_dir)
    return(invisible(NULL))
  }
  
  labs <- labels[seq_along(png_files)]
  
  img_grobs <- mapply(
    
    function(f, lab){
      
      img <- rasterGrob(
        readPNG(f),
        width = unit(1, "npc"),
        height = unit(1, "npc"),
        interpolate = FALSE
      )
      
      grobTree(
        img,
        textGrob(
          lab,
          x = unit(0.04,"npc"),
          y = unit(0.96,"npc"),
          just = c("left","top"),
          gp = gpar(
            fontsize = fontsize,
            fontface = "bold"
          )
        )
      )
      
    },
    
    png_files,
    labs,
    SIMPLIFY = FALSE
  )
  
  p <- arrangeGrob(
    grobs = img_grobs,
    ncol = ncol,
    padding = unit(0,"mm")
  )
  
  png(
    outfile,
    width = width,
    height = height,
    units = "in",
    res = 300
  )
  
  grid.draw(p)
  
  dev.off()
  
  invisible(outfile)
  
}
