# MiLB Projection Ensemble Modeling Pitchers


library(caret)
library(caretEnsemble)
library(Metrics)


# Shift c, for log(x +c) transformation to normalize response
c <- 5

milbTrainP <- readRDS("data/Pitchers/milbTrainP.rds")
milbTestP <- readRDS("data/Pitchers/milbTestP.rds")


# Define possible modelling columns as numeric, categoric , response or transformed response
# Dont use : W/L Ratio, SO/BB Ratio, both end up with NA's, including native variables anyway
predcol <- c("Year","Age","AgeDif","W","L","ERA","RAvg","G","GS","GF","CG","SHO",
             "SV","IP","H","R","ER","HR","BB","IBB","SO","HBP","BK","WP","BF",
             "WHIP","H9","HR9","BB9","SO9")
predcolFact <- c("Lg","Lev")
respcol <- c("mlb_WAR_6yr")
respcolTrans <- c("log_mlb_WAR_6yr_plus_c")


# Custom Transformed RMSE function
transRMSESummary <- function (data,
                              lev = NULL,
                              model = NULL) {
  out1 <- rmse(exp(data$obs)- c, exp(data$pred)-c) 
  out2 <- cor(exp(data$obs)- c, exp(data$pred)-c)*cor(exp(data$obs)- c, exp(data$pred)-c) 
  out <- c(out1,out2)
  names(out) <- c("MyRMSE", "Rsquared")
  out
}


control <- trainControl(method="repeatedcv", 
                        number=5, 
                        repeats=1, 
                        verboseIter = TRUE,
                        summaryFunction = transRMSESummary
)

#
# Base Models BCCS
#
set.seed(669)
modelsBCCSP <- caretList(milbTrainP[, c(predcol, predcolFact)],
                        milbTrainP[, c(respcolTrans)],
                        trControl=control, 
                        tuneList=list(
                          lm= caretModelSpec(method = "lm",
                                             preProcess= c("BoxCox","center","scale"),
                                             metric = "MyRMSE",
                                             maximize= FALSE),
                          pls= caretModelSpec(method = "pls",
                                              preProcess= c("BoxCox","center","scale"),
                                              tuneLength =ncol(milbTrainP[, c(predcol, predcolFact)]),
                                              metric = "MyRMSE",
                                              maximize= FALSE),
                          #      enet = caretModelSpec(method = "enet",
                          #                            preProcess= c("BoxCox","center","scale"),
                          #                            tuneGrid= expand.grid(.lambda= c(0,0.001, 0.01, 0.1),
                          #                                                 .fraction = seq(0.05,1,length=20)),
                          #                          metric = "RMSE",
                          #                       maximize= FALSE),
                          earth= caretModelSpec(method = "earth",
                                                preProcess= c("BoxCox","center","scale"),
                                                tuneGrid = expand.grid(.degree=1:5,
                                                                       .nprune = 2:50),
                                                metric = "MyRMSE",
                                                maximize= FALSE),
                          rpart = caretModelSpec(method = "rpart",
                                                 preProcess= c("BoxCox","center","scale"),
                                                 tuneLength=30,
                                                 metric = "MyRMSE",
                                                 maximize= FALSE),
                          ctree = caretModelSpec(method = "ctree",
                                                 preProcess= c("BoxCox","center","scale"),
                                                 tuneLength =20,
                                                 metric = "MyRMSE",
                                                 maximize= FALSE),
                          treebag = caretModelSpec(method = "treebag",
                                                   preProcess= c("BoxCox","center","scale"),
                                                   metric = "MyRMSE",
                                                   maximize= FALSE),
                          cubist = caretModelSpec(method = "cubist",
                                                  preProcess= c("BoxCox","center","scale"),
                                                  tuneGrid = expand.grid(.committees =c(1,50,100),
                                                                         .neighbors =c(0,1,5,9)),
                                                  metric = "MyRMSE",
                                                  maximize= FALSE),
                          gbm = caretModelSpec(method = "gbm",
                                               preProcess= c("BoxCox","center","scale"),
                                               tuneGrid = expand.grid(.interaction.depth =seq(1,16,by=3),
                                                                      .n.trees = seq(100,2000, by = 200),
                                                                      .shrinkage = c(0.001,0.01,0.1),
                                                                      .n.minobsinnode =10),
                                               metric = "MyRMSE",
                                               maximize= FALSE),
                          nnet = caretModelSpec(method = "nnet",
                                                preProcess= c("BoxCox","center","scale"),
                                                tuneGrid = expand.grid(.decay = c(1,1.5,2,2.5,3), 
                                                                       .size = c(seq(1,7, by=2))) ,
                                                metric = "MyRMSE",
                                                maximize= FALSE),
                          rf= caretModelSpec(method = "rf",
                                             preProcess= c("BoxCox","center","scale"),
                                             tuneGrid = expand.grid(.mtry=c(1,5,15,25,35,45)) ,
                                             metric = "MyRMSE",
                                             maximize= FALSE)#,
                          # svmRadial= caretModelSpec(method = "svmRadial",
                          #                          preProcess= c("BoxCox","center","scale"),
                          #        Why no                  tuneLength = 9 ,
                          #       work?                 metric = "MyRMSE",
                          #                       maximize= FALSE)
                          
                        ))
# Top Model GBM : RMSE = 2.059
resultsP <- resamples(modelsBCCSP)
summary(resultsP)
dotplot(resultsP)
parallelplot(resultsP)
# saveRDS(modelsBCCSP, "data/Models/Pitchers/modelsBCCSP.rds")
# modelsBCCSP <- readRDS("data/Models/Pitchers/modelsBCCSP.rds")

finalBCCSP <- modelsBCCSP
finalBCCSP$nnet <- NULL

resultsFinalP <- resamples(finalBCCSP)

# Parallel Plot of Base Models
jpeg("Plots/Pitcher_Model_Comp/EnsembleRMSE.jpeg", width = 500, height = 500)
parallelplot(resultsFinalP)
dev.off()

# Variable Importance for GBM Base Model
jpeg("Plots/Pitcher_Model_Comp/VariableImportanceGBM.jpeg", width = 500, height = 500)
plot(varImp(finalBCCSP$gbm))
dev.off()

smallList <- c(modelsBCCSP$earth, modelsBCCSP$rf, modelsBCCSP$gbm)

set.seed(669)
greedy_ensembleP <- caretEnsemble(
  modelsBCCSP, 
  metric="MyRMSE",
  maximize= FALSE,
  trControl=trainControl(
    number=10,
    classProbs=TRUE,
    verboseIter = TRUE,
    savePredictions = TRUE,
    summaryFunction = transRMSESummary
  ))
summary(greedy_ensembleP)
saveRDS(greedy_ensembleP, "data/Models/Pitchers/greedy_ensembleP.rds")
# Greedy Ensemble modelsBCCSP : RMSE = 2.0431
# Greedy Ensemble smallList : RMSE = 


stackControl <- trainControl(method="repeatedcv", 
                             number=5, repeats=1, 
                             savePredictions=TRUE,
                             verboseIter = TRUE, 
                             classProbs=TRUE,
                             summaryFunction = transRMSESummary)


set.seed(669)
stack.rfP <- caretStack(modelsBCCSP, 
                       method="rf", 
                       metric="MyRMSE",
                       maximize= FALSE, 
                       tuneGrid = expand.grid(.mtry=seq(1,11, by=2)),
                       trControl=stackControl)
saveRDS(stack.rfP, "data/Models/Pitchers/stack.rfP.rds")
# RF Stack modelsBCCSP : RMSE = 2.062431

set.seed(669)
gbmGrid <- expand.grid(.interaction.depth =seq(1,33,by=4),
                       .n.trees = seq(100,2100, by = 200),
                       .shrinkage = c(0.001,0.01,0.05),
                       .n.minobsinnode =10)
stack.gbmP <- caretStack(modelsBCCSP, 
                        method="gbm", 
                        tuneGrid = gbmGrid,
                        metric="MyRMSE",
                        maximize= FALSE
                        , trControl=stackControl)
saveRDS(stack.gbmP, "data/Models/Pitchers/stack.gbmP.rds")
# GBM Stack modelsBCCSP : RMSE = 2.058553


set.seed(669)
nnetGrid <- expand.grid(.size = seq(1,11,by=2),
                        .decay = c( 0,0.5,1,2,3,5,10,15))
stack.nnetP <- caretStack(modelsBCCSP, 
                         method="nnet", 
                         tuneGrid = nnetGrid,
                         metric="MyRMSE",
                         maximize= FALSE, 
                         trControl=stackControl)
saveRDS(stack.nnetP, "data/Models/Pitchers/stack.nnetP.rds")
# NNet Stack modelsBCCSP : RMSE = WTF?

set.seed(669)
stack.svmrP <- caretStack(modelsBCCSP, 
                         method="svmRadial", 
                         metric="MyRMSE",
                         maximize= FALSE,
                         tuneLength = 8,
                         trControl=stackControl)
saveRDS(stack.svmrP, "data/Models/Pitchers/stack.svmrP.rds")
# SVMR Stack modelsBCCSP : RMSE = 2.141185




