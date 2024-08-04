###Return predictions on to new data for each fold model
###Predictions are not summarised across fold models to allow flexibility 
###in expressing results
###Predictors must be identical to predictors names used to fit models in xgb.cv
###PredData must be numerical
###Predictions for multi-level responses will be stacked
xgb.cv.predict = function(cv, ###xgb.cv model object
                          PredData, ###Data on which to make predictions 
                          Predictors = Predictors, ###Names of predictor variables
                          Nfolds ###Number of fold models this could be obtained 
                                 ###automatically from model object
                          )
{
###Predict function requires data as a matrix
PredData = as.matrix(PredData[,colnames(PredData) %in% Predictors])
Preds = as.data.frame(matrix(nrow = nrow(PredData), ncol = 0))
for(fold in 1:Nfolds)
 {
 Model = xgb.Booster.complete(cv$models[[fold]])
 Preds = cbind(Preds,predict(Model, newdata = PredData))
 }
return(Preds)  
}

