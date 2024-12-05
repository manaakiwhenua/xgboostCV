library(caret)
library(ggplot2)
library(pdp)
library(xgboost)
library(boot)
library(yardstick)
source(paste0(ScriptDir,"xgb.cv.importance.plot.R"))
source(paste0(ScriptDir,"xgb.cv.partial.r"))
source(paste0(ScriptDir,"xgb.cv.fit.boxplot.r"))
source(paste0(ScriptDir,"xgb.cv.interaction.r"))
source(paste0(ScriptDir,"xgb.cv.makefolds.R"))

xgb.cv.logistic = function(Data,Predictors,Response,Objective = "binary:logistic",Metric = "logloss",path,Nfolds = 10,Nrounds = 10000,LearningRate=0.001
                           ,Nthread = 2,MaxDepth=3,save = TRUE,Folds = NULL, Monotone = NULL,DoInteraction = TRUE)
{
CVtrain_x = as.matrix(Data[, colnames(Data) %in% Predictors])
CVtrain_y = Data[,colnames(Data) == Response]

if(is.null(Monotone)==TRUE)
  Monotone = rep(0,times = ncol(CVtrain_x))
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


cv <- xgb.cv(tree_method = "exact",data = CVtrain_x, stratified = TRUE,label = CVtrain_y,nrounds = Nrounds, nthread = Nthread, nfold = Nfolds,folds = FoldList,monotone_constraints =Monotone,
             max_depth = MaxDepth, eta = min(50,Nrounds), objective = Objective,metric = Metric,prediction = TRUE,print_every_n = 50,learning_rate = LearningRate,
             save_models = TRUE,early_stopping_rounds = 50,callbacks = list(cb.cv.predict(save_models = TRUE)))
Nfolds = length(cv$models)
if(save==TRUE)
  saveRDS(cv,paste0(path,"xgb.cv.logistic.rds"))

PredClass = ifelse(cv$pred >0.5,1,0)

###Test accuracy of predictions
Confusion = confusionMatrix(as.factor(PredClass),as.factor(CVtrain_y))

###Calculate Out of Bag ROC
Pred = cv$pred[order(CVtrain_y)]
Truth = CVtrain_y[order(CVtrain_y)]
CVROC = roc_auc_vec(
  estimate = Pred,
  truth = as.factor(Truth),event_level="second")

###Calculate ROC for mean training preds across fold models
Preds = vector(length = 0)
Truth = vector(length = 0)
for(fold in 1:Nfolds)
 {
 Model = xgb.Booster.complete(cv$models[[fold]])
 Preds = c(Preds,predict(Model, newdata = CVtrain_x[-(cv$folds[[fold]]),]))
 Truth = c(Truth,CVtrain_y[-(cv$folds[[fold]])])
 }
Preds = Preds[order(Truth)]
Truth = Truth[order(Truth)]

TrainingROC = roc_auc_vec(
  estimate = Preds,
  truth = as.factor(Truth),event_level="second")
###Print box plots of predicted probabilities against observed occurrences for each class 
xgbm.cv.fit.boxplot.logistic(cv$pred,Data[, colnames(Data) == Response],ROC = c(TrainingROC,CVROC),path)

####Use custom function to generate predictor importance bar plots
Filename = paste0(path,"PredictorImportance.png")
Names = colnames(CVtrain_x)
Filename = paste0(path,"PredictorImportance.png")
Importance <- xgb.cv.importance.plot(cv, #ouput from xgb.cv. Be sure to use callback to save cv models
                       Nfolds, #number of fold models used in cross-validaton
                       Predictors= Names[Names%in% Predictors],#names of predictor variables
                                                               #this ensures names in right order    
                                                               #for importance function
                       Filename)#location to print bar plot 



####Use custom function to generate partial dependency plots
PartialDir = paste0(path,"PartialDependencePlots/")
dir.create(PartialDir,showWarnings = FALSE)
for(var in 1:length(Predictors))
    xgbm.cv.partial(cv,Nfolds = Nfolds,na.omit(CVtrain_x),var,path = PartialDir,CVtrain_y=CVtrain_y,ResponseName = Response)


###Do interaction last as hstats changes model predictions somehow in partial plots
if(DoInteraction == TRUE)
  Interaction = xgb.cv.interaction(cv,na.omit(CVtrain_x),Predictors,Nfolds)

OutList = list()
Key = "Model"
OutList[[Key]] = cv
Key = "OOBROC"
OutList[[Key]] = CVROC
Key = "TrainingROC"
OutList[[Key]] = TrainingROC
Key = "ConfusionMatrix"
OutList[[Key]] = Confusion
Key = "Predictor importance"
OutList[[Key]]= Importance
Key = "Interaction"
if(DoInteraction == TRUE)
  OutList[[Key]] = Interaction
return(c(OutList))
}