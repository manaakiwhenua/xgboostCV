library(caret)
library(ggplot2)
library(pdp)
library(xgboost)
library(boot)
library(yardstick)
source(paste0(ScriptDir,"xgb.cv.importance.plot.R"))
source(paste0(ScriptDir,"xgb.cv.partial.r"))
source(paste0(ScriptDir,"xgb.cv.fit.boxplot.r"))
source(paste0(ScriptDir,"xgb.cv.fit.scatterplot.r"))
source(paste0(ScriptDir,"xgb.cv.interaction.r"))
source(paste0(ScriptDir,"xgb.cv.makefolds.R"))

xgb.cv.continuous = function(Data,Predictors,Response,Objective = "reg:absoluteerror",Metric = "mae",path,Nfolds = 10,Nrounds = 10000,LearningRate=0.001
                             ,Nthread = 2,MaxDepth=3,save = TRUE, Folds = NULL, Monotone = NULL,DoInteraction = TRUE,ResponseLabel = Response,
                             PredictorLabels = Predictors, PlotPoints = FALSE, partial_CI = 0.95, PlotMeanResponse = TRUE)
{
  CVtrain_x = as.matrix(Data[, colnames(Data) %in% Predictors])
  CVtrain_y = Data[,colnames(Data) == Response]
  
  ### Align predictor labels to the order of predictors used in the model
  PredictorLabels = PredictorLabels[colnames(CVtrain_x)]
  PredictorLabels[is.na(PredictorLabels)] = colnames(CVtrain_x)[is.na(PredictorLabels)]
  
  ### Use model predictor order throughout
  Predictors = colnames(CVtrain_x)
  
  ###Convert fold vector (if supplied) to list of obsrvations in each fold
  ###Assumes length of fold vector = nrow(Data)
  K = Nfolds
  FoldList = NULL
  if(is.null(Folds)==FALSE)
  {
    K = min(Nfolds,length(unique(Folds)))
    FoldList <- xgb.cv.makefolds(as.factor(Folds), K)
  }
  Nfolds = K
  
  if(is.null(Monotone)==TRUE)
    Monotone = rep(0,times = ncol(CVtrain_x))
  
  cv <- xgb.cv(data = CVtrain_x, stratified = TRUE,label = CVtrain_y,nrounds = Nrounds, nthread = Nthread, nfold = Nfolds,folds = FoldList,monotone_constraints =Monotone,
               max_depth = MaxDepth, eta = min(50,Nrounds),objective = Objective,metric = Metric,prediction = TRUE,print_every_n = 50,learning_rate = LearningRate,
               save_models = TRUE,early_stopping_rounds = 50,callbacks = list(cb.cv.predict(save_models = TRUE)))
  Nfolds = length(cv$models)
  Cor = round(cor(CVtrain_y,cv$pred),digits = 3)
  if(save==TRUE)
    saveRDS(cv,paste0(path,"xgb.cv.continuous.rds"))
  
  ###Print scatter plot of predicted against observed values of response
  xgbm.cv.fit.scatterplot(cv$pred,Data[, colnames(Data) == Response],path)
  
  ####Use custom function to generate predictor importance bar plots
  ImportanceFile = paste0(path,"PredictorImportance.png")
  Importance <- xgb.cv.importance.plot(
    cv = cv,
    Nfolds = Nfolds,
    Predictors = Predictors,
    PredictorLabels = PredictorLabels,
    Filename = ImportanceFile
  )
  
  
  make_case_safe_names <- function(x) {
    
    out <- x
    
    dup_groups <- split(seq_along(x), tolower(x))
    
    for (g in dup_groups) {
      if (length(g) > 1) {
        
        # keep first unchanged, append suffix to later duplicates
        out[g[-1]] <- paste0(out[g[-1]], "_", seq_along(g[-1]))
      }
    }
    
    out
  }
  PredictorFileNames <- make_case_safe_names(Predictors)
  
  
  ####Use custom function to generate partial dependency plots
  PartialDir = paste0(path,"PartialDependencePlots/")
  dir.create(PartialDir,showWarnings = FALSE)
  
  ### First calculate all partial dependencies
  PD.list = vector("list", length(Predictors))
  PD.mean.list = vector("list", length(Predictors))
  
  for(var in seq_along(Predictors))
  {
    PD.list[[var]] = xgbm.cv.partial(
      cv,
      Nfolds = Nfolds,
      CVtrain_x = na.omit(CVtrain_x),
      var = var,
      CVtrain_y = CVtrain_y,
      Response = Response,
      ReturnData = TRUE
    )
    
    PD.mean.list[[var]] = aggregate(
      y ~ x,
      data = PD.list[[var]],
      FUN = mean
    )
  }
  
  ### Range of partial dependence predictions for each predictor
  PartialRange = data.frame(
    Predictor = PredictorLabels,
    Range = sapply(PD.mean.list, function(x) max(x$y, na.rm = TRUE) -
                     min(x$y, na.rm = TRUE))
  )
  
  
  ### Determine common y-axis limits
  PD_ylim = range(
    c(
      unlist(lapply(PD.list, function(x) x$y)),
      mean(CVtrain_y)
    ),
    na.rm = TRUE
  )
  
  ### Plot using common y-axis limits
  for(var in seq_along(Predictors))
  {
   
     xgbm.cv.partial(
      cv,
      Nfolds = Nfolds,
      CVtrain_x = na.omit(CVtrain_x),
      var = var,
      CVtrain_y = CVtrain_y,
      Response = Response,
      ResponseName = ResponseLabel,
      PredictorLabels = PredictorLabels,
      PredictorFileName = PredictorFileNames[var],
      PartialPreds = PD.list[[var]],
      ylim = PD_ylim,
      PartialDir =PartialDir,
      PlotPoints = PlotPoints,
      CI = partial_CI,
      PlotMeanResponse = PlotMeanResponse
    )
  }
  
  ###Do interaction last as hstats changes model predictions somehow in partial plots
  if(DoInteraction == TRUE)
    Interaction = xgb.cv.interaction(
      cv = cv,
      CVtrain_x = na.omit(CVtrain_x),
      Predictors = Predictors,
      Nfolds = Nfolds,
      PredictorLabels = PredictorLabels
    )
  
  OutList = list()
  Key = "Model"
  OutList[[Key]] = cv
  Key = "Correlation"
  OutList[[Key]] = Cor
  Key = "Predictor importance"
  OutList[[Key]]= Importance
  Key = "PartialRange"
  OutList[[Key]] = PartialRange
  Key = "Interaction"
  if(DoInteraction == TRUE)
    OutList[[Key]] = Interaction
  return(OutList)
}