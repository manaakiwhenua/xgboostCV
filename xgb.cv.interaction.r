##########################################################
##########################################################
###Functions for calculating and visualising two-way interactions
###Response level argument permits handling of mulitclass responses
###This is set to 1 by default for binary and continuous responses
###Author Norman Mason with support from Michael Mayer
###https://github.com/mayer79/
###Functions call hstats() and partial_dep()
###from hstats package:
###https://cran.r-project.org/web/packages/hstats/index.html
##########################################################
##########################################################

library(hstats)
xgb.cv.interaction <- function(cv, CVtrain_x, Predictors, PredictorLabels,
                               Nfolds, ResponseLevel = 1, verbose = TRUE)
{
  
  get_label <- function(x, PredictorLabels) {
    
    # labelled case: names(PredictorLabels) = model variables
    if (!is.null(names(PredictorLabels))) {
      return(as.character(PredictorLabels[x]))
    }
    
    # unlabelled case: identity
    return(x)
  }
  
  Reshape <- !is.null(dim(cv$pred))
  
  AllInt <- data.frame(Int = numeric(0), IntVars = character(0))
  
  SetSeed <- runif(1, 1, 1e7)
  
  for (fold in seq_len(Nfolds)) {
    
    Model <- xgb.Booster.complete(cv$models[[fold]])
    
    set.seed(SetSeed)
    
    s <- hstats(
      Model,
      X = as.matrix(na.omit(CVtrain_x)),
      reshape = Reshape,
      verbose = verbose,
      pairwise_m = length(Predictors),
      threeway_m = 0
    )
    
    Int_raw <- h2_pairwise(s, normalize = FALSE)
    
    if (!is.null(Int_raw)) {
      
      Int_mat <- Int_raw[[1]]
      
      Int_vals <- as.numeric(Int_mat[, ResponseLevel])
      
      Int <- data.frame(
        Int = Int_vals,
        IntVars = rownames(Int_mat)
      )
      
      AllInt <- rbind(AllInt, Int)
    }
  }
  
  if (nrow(AllInt) > 0) {
    
    AllInt$Int[is.na(AllInt$Int)] <- 0
    
    MeanInt <- aggregate(Int ~ IntVars, AllInt, mean)
    SDInt   <- aggregate(Int ~ IntVars, AllInt, sd)
    
    IntOut <- merge(MeanInt, SDInt, by = "IntVars", suffixes = c("_mean", "_sd"))
    
    parts <- do.call(rbind, strsplit(IntOut$IntVars, ":"))
    
    lab1 <- get_label(parts[,1], PredictorLabels)
    lab2 <- get_label(parts[,2], PredictorLabels)
    
    IntOut <- data.frame(
      Interaction = paste(lab1, lab2, sep = ":"),
      Mean = IntOut$Int_mean,
      sd   = IntOut$Int_sd
    )
    
    IntOut <- IntOut[order(IntOut$Mean, decreasing = TRUE), ]
  }
  
  return(IntOut)
}

################################################################################
###3-D perspective plot
################################################################################

xgb.cv.perspective = function(cv,Nfolds,CVtrain_x,Var1,Var2,path,Response,ResponseLab = "Response",Var1Lab = NA, Var2Lab = NA,ResponseLevel=1,theta = NA)
{
  if(is.na(Var1Lab)==TRUE)
    Var1Lab = Var1
  if(is.na(Var2Lab)==TRUE)
    Var2Lab = Var2
  ###predict(xgboost) stacks predictions for different levels of response
  ###in a single vector so need to apply reshape option in partial_dep function calls
  Reshape = FALSE
  if(is.null(dim(cv$pred))==FALSE)
    Reshape = TRUE
  
  ###Set up a predictor grid for 3d perspective plots
  ###constrain values to min/max to avoid extrapolation
  ###may still extrapolate to combnations of predictor values
  ###that don't occur in the data
  Xpred = CVtrain_x[1:50,]
  for(i in 1:ncol(Xpred))
    Xpred[,i] = seq(min(na.omit(CVtrain_x[,i])),max(na.omit(CVtrain_x[,i])),length = 50)
  Xpred = as.matrix(Xpred) 
  Allpd = as.data.frame(matrix(ncol=3,nrow = 0))
  colnames(Allpd) = c(Var1,Var2,"y")
  ###Get partial predictions for each fold model
  for(fold in 1:Nfolds)
  {
    ###xgb.Booster.complete() required if loading model from disk
    Model = xgb.Booster.complete(cv$models[[fold]])
    Foldpd <- partial_dep(object = Model, v = c(Var1,Var2), X = Xpred,
                          grid_size = nrow(Xpred)^2,reshape = Reshape)
    Foldpd <- Foldpd$data[,c(1:2,(2+ResponseLevel))]
    colnames(Foldpd) <- colnames(Allpd)
    Allpd = rbind(Allpd,Foldpd)
  }
  ###Calculate mean predictions across fold models
  MeanIntPD = aggregate(Allpd[,3],by = list(Allpd[,1],Allpd[,2]),FUN = mean)
  
  ###Extract values for X,Y and Z used in persp() function call
  X = sort(unique(MeanIntPD[,1]))
  Y=sort(unique(MeanIntPD[,2]))
  Z = matrix(ncol = length(Y),nrow = length(X))
  for(x in 1:length(X))
    Z[x,] = MeanIntPD[MeanIntPD[,1]==X[x],3]
  
  if(is.na(theta))
  {
    theta <- c(315, 45, 225, 135)[which.min(c(
      Z[1,1],                 # LL
      Z[nrow(Z),1],           # LR
      Z[1,ncol(Z)],           # UL
      Z[nrow(Z),ncol(Z)]      # UR
    ))]
  }
  ###print to file
  Filename = paste0(path,Var1,".",Var2,".",Response,".","Perspective.png")
  png(Filename,width = 3200,height=3200)
  par(cex.lab = 7,cex.axis = 5, mar = c(18,20,16,12),mgp = c(14,6,0),
      cex.main = 7.6,bty = "n")
  persp(X,Y,Z,theta=theta, phi=40, r = sqrt(10), d = 3,               # viewing pars
        shade = 0.5,ticktype = "detailed", xlab = Var1Lab,ylab = Var2Lab,zlab = ResponseLab)
  dev.off()
}


############################################################
## Create interaction plots
############################################################

xgb.interaction.plots <- function(
    CV,
    Data,
    Predictors,
    PredictorLabels = Predictors,
    Response,
    ResponseLab = Response,
    path,
    Nfolds,
    N = 3,
    ncol = 2,
    width = 8,
    height = 8
){
  
  IntPath <- paste0(path,"Interaction/")
  dir.create(IntPath,showWarnings = FALSE)
  
  TopInteractions <- head(CV$Interaction$Interaction,N)
  
  for(int in TopInteractions){
    
    
    vars <- strsplit(int, ":")[[1]]
    
    Var1 <- vars[1]
    Var2 <- vars[2]
    
    if (is.null(PredictorLabels)) {
      
      Var1Lab <- Var1
      Var2Lab <- Var2
      
    } else {
      
      i1 <- match(Var1, Predictors)
      i2 <- match(Var2, Predictors)
      
      Var1Lab <- PredictorLabels[i1]
      Var2Lab <- PredictorLabels[i2]
      
    }
    
    xgb.cv.perspective(
      
      CV$Model,
      
      Nfolds,
      
      Data[,Predictors],
      
      Var1 = Var1,
      
      Var2 = Var2,
      
      Var1Lab = Var1Lab,
      
      Var2Lab = Var2Lab,
      
      path = IntPath,
      
      Response = Response,
      
      ResponseLab = ResponseLab
      
    )
    
  }
  
  png.multipanel(
    
    png_dir = IntPath,
    
    outfile = paste0(path,"Interactions.png"),
    
    ncol = ncol,
    
    width = width,
    
    height = height
    
  )
  
}
