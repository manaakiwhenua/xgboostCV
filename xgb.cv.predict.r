###Return predictions on to new data for each fold model
###Predictions are not summarised across fold models to allow flexibility 
###in expressing results
###Predictors must be identical to predictors names used to fit models in xgb.cv
###PredData must be numerical
xgb.cv.predict = function(cv, ###xgb.cv model object
                          PredData, ###Data on which to make predictions 
                          Predictors = Predictors, ###Names of predictor variables
                          Nfolds ###Number of fold models this could be obtained 
                                 ###automatically from model object
                          )
{
###Predict function requires data as a matrix
PredData = as.matrix(PredData[,colnames(PredData) %in% Predictors])
Preds = vector(length = 0)
Fold = vector(length = 0)
for(fold in 1:Nfolds)
 {
 Model = xgb.Booster.complete(cv$models[[fold]])
 Preds = c(Preds,predict(Model, newdata = PredData))
 Fold = c(Fold, rep(fold, times = nrow(PredData)))
 }
return(cbind(Fold,Preds))  
}

