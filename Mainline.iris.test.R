rm(list = ls())
ScriptDir = r"[C:\Users\MasonN\OneDrive - MWLR\SourceFiles\xgb.cv\]"

#############################################################
###Call a wrapper function which fits cross-validated multiclass models
###and generates outputs for assessing goodness of fit
###and interpreting influence of predictors
###Try the commonly used iris dataset
###to check custom functions are working as expected
#############################################################

###Load the wrapper source file
###this loads other required files and packages
source(paste0(ScriptDir,"wrapper.xgb.cv.multi.R"))
data(iris)
head(iris)

Predictors = colnames(iris[1:4])
Response = colnames(iris[5])

###Set path for storing results
path = paste0(ScriptDir,"Iris/")
dir.create(path)

###set "hyper-parameters"
###results for different parameter combinations can be stored separately
###by using paramters to define "path"
Nthread = 2
MaxDepth = 3
Nfolds = 10
Nrounds = 10000
LearningRate = 0.1

###Call the wrapper function
###Assign output to variable for easy recall of results and model
CV <-xgb.cv.multi(Data = iris,Predictors=Predictors,Response=Response,path=path,Nfolds = Nfolds,Nrounds = Nrounds,LearningRate = LearningRate,
             Nthread = Nthread,MaxDepth=MaxDepth, save = TRUE)
CV$"Predictor importance"
###If save == TRUE (default) you can load the xgb.cv output from source code
###in this case need to apply xgb.Booster.complete() to fold models
###This is done automatically in xgb.cv.perspective()
cv = readRDS(paste0(path,"xgb.cv.multi.rds"))


###The wrapper function output includes the output from xgb.cv
###so you can retrieve the fold models like this too.
cv <-CV$Model

###Inspect interactions
###Note interaction estimated separately for each level of response
CV$Interaction

###Print perspective plots for largest interactions
###xgb.cv.perspective() automatically rotates plots for 
###good (not necessarily best) visualiation
###Changing order of interacting variables can improve visualisation

Classes = unique(as.character(iris[,5]))
xgb.cv.perspective(cv,Nfolds,iris[,colnames(iris) %in% Predictors],Var1="Sepal.Width",Var2="Petal.Width",path = path, Response = Classes[1],ResponseLab = Classes[1],ResponseLevel=1)

xgb.cv.perspective(cv,Nfolds,iris[,colnames(iris) %in% Predictors],Var1="Petal.Length",Var2="Petal.Width",path = path, Response = Classes[2],ResponseLab = Classes[2],ResponseLevel=2)
xgb.cv.perspective(cv,Nfolds,iris[,colnames(iris) %in% Predictors],Var1="Petal.Length",Var2="Petal.Width",path = path, Response = Classes[3],ResponseLab = Classes[3],ResponseLevel=3)

###Try using fold vector
Group = c(rep(1,times = 50),rep(2,times = 50),rep(3,times = 50))
Group = Group[sample(1:150,size  = 150)]
path = paste0(ScriptDir,"IrisFoldVector/")
dir.create(path)
CVfold <-xgb.cv.multi(Data = iris,Predictors=Predictors,Response=Response,path=path,Nfolds = Nfolds,Nrounds = Nrounds,LearningRate = LearningRate,
                  Nthread = Nthread,MaxDepth=MaxDepth, save = TRUE,Folds = Group)


#############################################################
#############################################################
#############################################################
###Call a wrapper function which fits cross-validated logistic models
###and generates outputs for assisting goodness of fit
###and interpreting influence of predictors
###Try the commonly used iris dataset
###to check custom functions are working as expected
#############################################################
rm(list = ls())
ScriptDir = r"[C:\Users\MasonN\OneDrive - MWLR\SourceFiles\xgb.cv\]"
source(paste0(ScriptDir,"wrapper.xgb.cv.logistic.r"))
###Set path for storing results
path = paste0(ScriptDir,"virginica/")
dir.create(path)

Predictors = colnames(iris[1:4])

Data = iris
Data$virginica = ifelse(Data$Species == "virginica",1,0)
Response = "virginica"
CVtrain_x = as.matrix(Data[, colnames(Data) %in% Predictors])
CVtrain_y = Data[,colnames(Data) == Response]
colnames(CVtrain_x)
Nthread = 2
MaxDepth = 3
Nfolds = 10
Nrounds = 10000
LearningRate = 0.1
Monotone = c(1,1,-1,0)
CV <-xgb.cv.logistic(Data=Data,Predictors=Predictors,Response=Response,path=path,Nrounds = Nrounds,LearningRate = LearningRate,
                Nthread = 2,MaxDepth=MaxDepth,save = TRUE,Monotone=Monotone)
CV$"Predictor importance"

cv = CV$Model

CV$Interaction
xgb.cv.perspective(cv,Nfolds,iris[,colnames(Data) %in% Predictors],Var1="Petal.Length",Var2="Petal.Width",path = path,Response = "virginica", ResponseLab = "virginica")

###Try using fold vector
Group = c(rep(1,times = 50),rep(2,times = 50),rep(3,times = 50))
Group = Group[sample(1:150,size  = 150)]
path = paste0(ScriptDir,"virginicaFoldVector/")
dir.create(path)

CVfold <-xgb.cv.logistic(Data=Data,Predictors=Predictors,Response=Response,path=path,Nrounds = Nrounds,LearningRate = LearningRate,
                     Nthread = 2,MaxDepth=MaxDepth,save = TRUE,Folds = Group)

#############################################################
#############################################################
#############################################################
###Call a wrapper function which fits cross-validated continuous regression
###and generates outputs for assessing goodness of fit
###and interpreting influence of predictors
###Try the commonly used iris dataset
###to check custom functions are working as expected
#############################################################
rm(list = ls())
ScriptDir = r"[C:\Users\MasonN\OneDrive - MWLR\SourceFiles\xgb.cv\]"
source(paste0(ScriptDir,"wrapper.xgb.cv.continuous.R"))
Data = iris
###code species as binary variables
###technically could leave one out
###code all species for ease of visualising/interpetting model
Data$setosa = ifelse(Data$Species == "setosa",1,0)
Data$versicolor = ifelse(Data$Species == "versicolor",1,0)
Data$virginica = ifelse(Data$Species == "virginica",1,0)
Predictors = colnames(Data[c(1,2,4,6:8)])
Response = colnames(Data[3])
Monotone = c(-1,0,-1,0,0,0)
###Set path for storing results
path = paste0(ScriptDir,Response,"/")
dir.create(path)

Nthread = 2
MaxDepth = 3
Nfolds = 10
Nrounds = 10000
LearningRate = 0.01
CV <-xgb.cv.continuous(Data = Data,Predictors = Predictors,Response=Response,Nfolds = Nfolds,path=path,Nrounds = Nrounds,LearningRate = LearningRate,
                Nthread = 2,MaxDepth=MaxDepth, Monotone = Monotone)
#cv = readRDS(paste0(path,"xgb.cv.continuous.rds"))
cv <-CV$Model
plot(Data$Petal.Width,cv$pred)
CV$"Predictor importance"
CV$Interaction
xgb.cv.perspective(cv,Nfolds,Data[,colnames(Data) %in% Predictors],"Sepal.Length","Petal.Width",path,Response = Response,ResponseLab = Response)

xgb.cv.perspective(cv,Nfolds,Data[,colnames(Data) %in% Predictors],"versicolor","Petal.Width",path,Response = Response,ResponseLab = Response)

###Try using fold vector
Group = c(rep(1,times = 50),rep(2,times = 50),rep(3,times = 50))
Group = Group[sample(1:150,size  = 150)]
path = paste0(ScriptDir,Response,"FoldVector/")
dir.create(path)

CV <-xgb.cv.continuous(Data=Data,Predictors=Predictors,Response=Response,path=path,Nrounds = Nrounds,LearningRate = LearningRate,
                     Nthread = 2,MaxDepth=MaxDepth,save = TRUE,Folds = Group)
