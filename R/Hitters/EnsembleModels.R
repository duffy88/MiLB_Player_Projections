# MiLB Projection Ensemble Modeling


library(caret)
library(caretEnsemble)
library(Metrics)


# Shift c, for log(x +c) transformation to normalize response
c <- 5

trainName <- "data/Hitters/milbFiltTrain.rds"
testName <- "data/Hitters/milbFiltTest.rds"

milbTrain <- readRDS(trainName)
milbTest <- readRDS(testName)

# Define possible modelling columns as numeric, categoric , response or transformed response
predcol <- c("Year","Age","AgeDif","G","PA","AB","R","H","X2B",
             "X3B","HR","RBI","SB","CS","BB","SO","BA","OBP","SLG","OPS","TB",
             "GDP","HBP","SH","SF","IBB","BBPct","SOPct","ISO","BABIP","BB.SORat",
             "X1B","SBPct","wOBA","Spd")
predcolFact <- c("Lg","Lev")
respcol <- c("mlb_oWAR_6yr")
respcolTrans <- c("log_mlb_oWAR_6yr_plus_c")



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
modelsBCCS <- caretList(milbTrain[, c(predcol, predcolFact)],
                    milbTrain[, c(respcolTrans)],
                    trControl=control, 
                    tuneList=list(
                      lm= caretModelSpec(method = "lm",
                                         preProcess= c("BoxCox","center","scale"),
                                         metric = "MyRMSE",
                                         maximize= FALSE),
                      pls= caretModelSpec(method = "pls",
                                          preProcess= c("BoxCox","center","scale"),
                                          tuneLength =ncol(milbTrain[, c(predcol, predcolFact)]),
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
# Top Model BCCS GBM : RMSE = 2.263
# Top Model BCCS Filtered  : RMSE = 2.264
results <- resamples(modelsBCCS)
summary(results)
dotplot(results)
parallelplot(results)

# saveRDS(modelsBCCS, "data/Models/Hitters/modelsFiltBCCS.rds")
# modelsBCCS <- readRDS("data/Models/Hitters/modelsBCCS.rds")


finalBCCS <- modelsBCCS
finalBCCS$nnet <- NULL

resultsFinal <- resamples(finalBCCS)

# Parallel Plot of Base Models
jpeg("Plots/Hitter_Model_Comp/EnsembleRMSE.jpeg", width = 500, height = 500)
parallelplot(resultsFinal)
dev.off()

# Variable Importance for GBM Base Model
jpeg("Plots/Hitter_Model_Comp/VariableImportanceGBM.jpeg", width = 500, height = 500)
plot(varImp(finalBCCS$gbm))
dev.off()

smallList <- c(modelsBCCS$earth, modelsBCCS$rf, modelsBCCS$gbm)

set.seed(669)
greedy_ensemble <- caretEnsemble(
  modelsBCCS, 
  metric="MyRMSE",
  maximize= FALSE,
  trControl=trainControl(
    number=10,
    classProbs=TRUE,
    verboseIter = TRUE,
    savePredictions = TRUE,
    summaryFunction = transRMSESummary
  ))
summary(greedy_ensemble)
saveRDS(greedy_ensemble, "data/Models/Hitters/greedy_ensembleFilt.rds")
# Greedy Ensemble modelsBCCS : RMSE = 2.235
# Greedy Ensemble finalBCCS : RMSE = 2.2352
# Greedy Ensemble smallList : RMSE = 2.2369
# Greedy Ensemble modelsBCCS Filtered: RMSE = 2.2378


stackControl <- trainControl(method="repeatedcv", 
                             number=5, repeats=1, 
                             savePredictions=TRUE,
                             verboseIter = TRUE, 
                             classProbs=TRUE,
                             summaryFunction = transRMSESummary)


set.seed(669)
stack.rf <- caretStack(modelsBCCS, 
                       method="rf", 
                       metric="MyRMSE",
                       maximize= FALSE, 
                       tuneGrid = expand.grid(.mtry=seq(1,11, by=2)),
                       trControl=stackControl)
saveRDS(stack.rf, "data/Models/Hitters/stack.rf.rds")
# RF Stack modelsBCCS : RMSE = 2.255744

set.seed(669)
gbmGrid <- expand.grid(.interaction.depth =seq(1,33,by=4),
                       .n.trees = seq(100,2100, by = 200),
                       .shrinkage = c(0.001,0.01,0.05),
                       .n.minobsinnode =10)
stack.gbm <- caretStack(modelsBCCS, 
                        method="gbm", 
                        tuneGrid = gbmGrid,
                        metric="MyRMSE",
                        maximize= FALSE
                        , trControl=stackControl)
saveRDS(stack.gbm, "data/Models/Hitters/stack.gbm.rds")
# GBM Stack modelsBCCS : RMSE = 2.261488


set.seed(669)
nnetGrid <- expand.grid(.size = seq(1,11,by=2),
                        .decay = c( 0,0.5,1,2,3,5,10,15))
stack.nnet <- caretStack(modelsBCCS, 
                         method="nnet", 
                         tuneGrid = nnetGrid,
                         metric="MyRMSE",
                         maximize= FALSE, 
                         trControl=stackControl)
saveRDS(stack.nnet, "data/Models/Hitters/stack.nnet.rds")
# NNet Stack modelsBCCS : RMSE = 3.729510

set.seed(669)
stack.svmr <- caretStack(modelsBCCS, 
                         method="svmRadial", 
                         metric="MyRMSE",
                         maximize= FALSE,
                         tuneLength = 8,
                         trControl=stackControl)
saveRDS(stack.svmr, "data/Models/Hitters/stack.svmr.rds")
# SVMR Stack modelsBCCS : RMSE = 2.354004



