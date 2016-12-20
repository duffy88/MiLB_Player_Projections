# MiLB Player Projection Modelling 


library(caret)


# Shift c, for log(x +c) transformation to normalize response
c <- 5

milbTrain <- readRDS("data/Hitters/milbTrain.rds")
milbTest <- readRDS("data/Hitters/milbTest.rds")


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


# Linear Reg
set.seed(669)
linTune <- train(milbTrain[, c(predcol, predcolFact)],
                 milbTrain[, c(respcolTrans)],
                 method="lm",
                 preProcess= c("BoxCox","center","scale"),
                 trControl = controlObject)
# Center Scaled : RMSE = 2.414802 ; R^2 = 0.08190108

# Center Scaled BoxCox : RMSE = 2.40037 ; R^2 = 0.09309894
# Center Scaled BoxCox TestSet: RMSE = 2.491157 ; R^2 = 0.08752434

# Center Scaled BoxCox RespTrans: RMSE = 0.2401569 ; R^2 = 0.09497965
# Center Scaled BoxCox RespTrans TestSet: RMSE = 2.522226 ; R^2 = 0.09312896
saveRDS(linTune, "data/Models/Hitters/linTuneBoxCoxRespTrans.rds")

# PLS Model
set.seed(669)
plsTune <- train( milbTrain[, c(predcol, predcolFact)],
                  milbTrain[, c(respcolTrans)],
                  method="pls",
                  preProcess= c("BoxCox","center","scale"),
                  tuneLength =50,
                  trControl = controlObject)
# Center Scaled : RMSE = 2.414782 ; R^2 = 0.08190874 ; n= 14

# Center Scaled BoxCox : RMSE = 2.400346 ; R^2 = 0.09311752 ; n= 24
# Center Scaled BoxCox TestSet: RMSE = 2.491325 ; R^2 = 0.08740314 ; n= 24

# Center Scaled BoxCox RespTrans: RMSE = 0.2401475 ; R^2 = 0.095040445 ; n= 21
# Center Scaled BoxCox RespTrans TestSet: RMSE = 2.522484 ; R^2 = 0.09282073 ; n= 21
saveRDS(plsTune, "data/Models/Hitters/plsTuneBoxCoxRespTrans.rds")
jpeg("Plots/Hitter_Tuning/PLSBoxCoxRespTrans.jpeg", width = 1000, height = 500)
library(caret)
plot(plsTune)
dev.off()



# ENet
enetGrid <- expand.grid(.lambda= c(0,0.001, 0.01, 0.1),
                        .fraction = seq(0.05,1,length=20))
set.seed(669)
enetTune <- train(log_mlb_oWAR_6yr_plus_c~.,
                  data= milbTrain,
                  method="enet",
                  preProcess= c("BoxCox","center","scale"),
                  tuneGrid= enetGrid,
                  trControl = controlObject)
# Center Scaled : RMSE = 2.414416 ; R^2 = 0.08194773 ; fraction = 0.35 ; lambda = 0

# Center Scaled BoxCox : RMSE = 2.400325 ; R^2 = 0.09309841 ; fraction = 0.95 ; lambda = 0.001
# Center Scaled BoxCox TestSet: RMSE = 2.491181 ; R^2 = 0.08749712 ; fraction = 0.95 ; lambda = 0.001

# Center Scaled BoxCox RespTrans: RMSE = 0.2401392 ; R^2 = 0.09494146 ; fraction = 0.85 ; lambda = 0
# Center Scaled BoxCox RespTrans TestSet: RMSE = 2.522226 ; R^2 = 0.09312896 ; fraction = 0.85 ; lambda = 0
saveRDS(enetTune, "data/Models/Hitters/enetTuneBoxCoxRespTrans.rds")
jpeg("Plots/Hitter_Tuning/ENetBoxCoxRespTrans.jpeg", width = 1000, height = 500)
plot(enetTune)
dev.off()

# MARS
set.seed(669)
marsTune <- train(log_mlb_oWAR_6yr_plus_c~.,
                  data= milbTrain,
                  method="earth",
                  preProcess= c("BoxCox","center","scale"),
                  tuneGrid = expand.grid(.degree=1:5,
                                         .nprune = 2:50),
                  trControl = controlObject)
# Center Scaled : RMSE = 2.340111 ; R^2 = 0.13979571 ; nprune = 34 ; degree = 3

# Center Scaled BoxCox: RMSE = 2.335996  ; R^2 = 0.14235339 ; nprune = 33 ; degree = 3
# Center Scaled BoxCox TestSet: RMSE =  2.400329 ; R^2 = 0.1528377 ; nprune = 33 ; degree = 3

# Center Scaled BoxCox RespTrans: RMSE = 0.2341386 ; R^2 = 0.14155081 ; nprune = 44 ; degree = 3
# Center Scaled BoxCox RespTrans TestSet: RMSE = 2.444658 ; R^2 = 0.1441682 ; nprune = 44 ; degree = 3
saveRDS(marsTune, "data/Models/Hitters/marsTuneBoxCoxRespTrans.rds")
jpeg("Plots/Hitter_Tuning/MarsBoxCoxRespTrans.jpeg", width = 1000, height = 500)
library(caret)
plot(marsTune)
dev.off()


# CART
set.seed(669)
rpartTune <- train(log_mlb_oWAR_6yr_plus_c~.,
                   data= milbTrain,
                   method= "rpart",
                   preProcess= c("BoxCox","center","scale"),
                   tuneLength=30,
                   trControl = controlObject) 
# Center Scaled : RMSE = 2.474299 ; R^2 = 0.05065674 ; cp = 0.004177538

# Center Scaled BoxCox: RMSE = 2.474299 ; R^2 = 0.05065674 ; cp = 0.004177538
# Center Scaled BoxCox TestSet: RMSE = 2.508736 ; R^2 = 0.07476185 ; cp = 0.004177538

# Center Scaled BoxCox RespTrans: RMSE = 0.2464507 ; R^2 = 0.05389027 ; cp = 0.002890674
# Center Scaled BoxCox RespTrans TestSet: RMSE = 2.534282 ; R^2 = 0.06934903 ; cp = 0.002890674
saveRDS(rpartTune, "data/Models/Hitters/rpartTuneBoxCoxRespTrans.rds")
jpeg("Plots/Hitter_Tuning/RPartBoxCoxRespTrans.jpeg", width = 1000, height = 500)
library(caret)
plot(rpartTune)
dev.off()


# C-Inf Tree
set.seed(669)
ctreeTune <- train(log_mlb_oWAR_6yr_plus_c~.,
                   data= milbTrain,
                   method = "ctree",
                   preProcess= c("BoxCox","center","scale"),
                   tuneLength =20,
                   trControl = controlObject)
# Center Scaled : RMSE = 2.456726 ; R^2 = 0.05857093 ; mincriterion = 0.99 

# Center Scaled BoxCox: RMSE = 2.448265 ; R^2 = 0.06853914 ; mincriterion = 0.8811111 
# Center Scaled BoxCox TestSet: RMSE = 2.419821 ; R^2 = 0.1390885 ; mincriterion = 0.8811111 

# Center Scaled BoxCox RespTrans: RMSE = 0.2456132 ; R^2 = 0.06036817 ; mincriterion = 0.99 
# Center Scaled BoxCox RespTrans TestSet: RMSE = 2.519239 ; R^2 = 0.07675195 ; mincriterion = 0.99 
saveRDS(ctreeTune, "data/Models/Hitters/ctreeTuneBoxCoxRespTrans.rds")
jpeg("Plots/Hitter_Tuning/CTreeBoxCoxRespTrans.jpeg", width = 1000, height = 500)
library(caret)
plot(ctreeTune)
dev.off()

# Tree (bagged)
set.seed(669)
treebagTune <- train(log_mlb_oWAR_6yr_plus_c~.,
                     data= milbTrain,
                     method="treebag",
                     preProcess= c("BoxCox","center","scale"),
                     trControl = controlObject)
# Center Scaled : RMSE = 2.460771 ; R^2 = 0.05246822

# Center Scaled BoxCox: RMSE = 2.46072 ; R^2 = 0.05256425
# Center Scaled BoxCox TestSet: RMSE = 2.55484 ; R^2 = 0.04945331

# Center Scaled BoxCox RespTrans: RMSE = 0.2476526 ; R^2 = 0.03898673
# Center Scaled BoxCox RespTrans TestSet: RMSE = 2.587517 ; R^2 = 0.03804177
saveRDS(treebagTune, "data/Models/Hitters/treebagTuneBoxCoxRespTrans.rds")





# Cubist 
# Max committess = 100 ; neighbors = 9
cubistGrid <- expand.grid(.committees =c(1,50,100),
                          .neighbors =c(0,1,5,9))
set.seed(669)
cubistTune <- train(log_mlb_oWAR_6yr_plus_c~.,
                    data= milbTrain,
                    method= "cubist",
                    preProcess= c("BoxCox","center","scale"),
                    tuneGrid=cubistGrid,
                    trControl = controlObject)
# Center Scaled : RMSE = 2.418252 ; R^2 = 0.08596737 ; committees = 100 ; neighbors = 9

# Center Scaled BoxCox: RMSE = 2.422580 ; R^2 = 0.06880900 ; committees = 50 ; neighbors = 9
# Center Scaled BoxCox TestSet: RMSE = 1.684629 ; R^2 = 0.6781164 ; committees = 50 ; neighbors = 9
# What in the world with that Test Set performance? 
# Model doesn't seem to have a stray response as a hidden predictor...what else?

# Center Scaled BoxCox RespTrans: RMSE = 0.2416305 ; R^2 = 0.13026636 ; committees = 100 ; neighbors = 0
# Center Scaled BoxCox RespTrans TestSet: RMSE = 2.533924 ; R^2 = 0.1539513 ; committees = 100 ; neighbors = 0
saveRDS(cubistTune, "data/Models/Hitters/cubistTuneBoxCoxRespTrans.rds")
jpeg("Plots/Hitter_Tuning/CubistBoxCoxRespTrans.jpeg", width = 1000, height = 500)
library(caret)
plot(cubistTune)
dev.off()


# GBM  
gbmGrid <- expand.grid(.interaction.depth =seq(1,16,by=3),
                       .n.trees = seq(100,2000, by = 200),
                       .shrinkage = c(0.001,0.01,0.1),
                       .n.minobsinnode =10)
set.seed(669)
gbmTune <- train(log_mlb_oWAR_6yr_plus_c~.,
                 data= milbTrain,
                 method="gbm",
                 preProcess= c("BoxCox","center","scale"),
                 tuneGrid = gbmGrid,
                 verbose=FALSE,
                 trControl = controlObject)
# Center Scaled : RMSE = 2.301112 ; R^2 = 0.16618523 ; 
# Center Scaled : n.trees = 1300 ; interaction.depth = 13 ; shrinkage = 0.01 ; n.minobsinnode = 10

# Center Scaled BoxCox: RMSE = 2.196225 ; R^2 = 0.1757742 ; 
# Center Scaled BoxCox: n.trees = 1300 ; interaction.depth = 13 ; shrinkage = 0.01 ; n.minobsinnode = 10
# Center Scaled BoxCox TestSet: RMSE = 2.128499 ; R^2 = 0.3696471 ; 

# Center Scaled BoxCox RespTrans: RMSE = 0.2319867 ; R^2 = 0.15583125 ; 
# Center Scaled BoxCox RespTrans: n.trees = 1100 ; interaction.depth = 16 ; shrinkage = 0.01 ; n.minobsinnode = 10
# Center Scaled BoxCox RespTrans: RMSE = 2.431284 ; R^2 = 0.1620823 ; 
saveRDS(gbmTune, "data/Models/Hitters/gbmTuneBoxCoxRespTrans.rds")
jpeg("Plots/Hitter_Tuning/GBMBoxCoxRespTrans.jpeg", width = 1000, height = 500)
library(caret)
plot(gbmTune)
dev.off()


# Model Trees (current to do)
#set.seed(669) 
#mtTune <- train(log_mlb_oWAR_6yr_plus_c~.,
 #               data= milbTrain,
  #              method="M5",
   #             preProcess= c("center","scale"),
    #            trControl = controlObject)
# Orig : RMSE =  R^2 = 
# CS : RMSE =  R^2 = 
#saveRDS(mtTune, "data/Models/Hitters/mtTuneRespTrans.rds")
#jpeg("Plots/Hitter_Tuning/ModelTreesRespTrans.jpeg", width = 1000, height = 500)
#library(caret)
#plot(mtTune)
#dev.off()


# NeuralNet
nnetGrid <- expand.grid(.decay = c(1,1.5,2,2.5,3), 
                        .size = c(seq(1,7, by=2)),
                        .bag=c(T,F)) 
set.seed(669) 
nnetModelTune <- train(log_mlb_oWAR_6yr_plus_c~.,
                       data= milbTrain,
                       method="avNNet",
                       preProcess= c("BoxCox","center","scale"),
                       tuneGrid= nnetGrid,
                       linout =TRUE,
                       trace =FALSE,
                       maxit=1000,
                       trControl = controlObject) 
# Center Scaled : RMSE = 2.267691 ; R^2 = 0.1931118 ; size = 5 ; decay = 1 ; bag = FALSE

# Center Scaled BoxCox : RMSE = 2.267504 ; R^2 = 0.1945174 ; size = 3 ; decay = 3 ; bag = FALSE
# Center Scaled BoxCox TestSet: RMSE = 2.178095 ; R^2 = 0.1925812 ; size = 3 ; decay = 3 ; bag = FALSE
saveRDS(nnetModelTune, "data/Models/Hitters/nnetModelTuneBoxCoxRespTrans.rds")
jpeg("Plots/Hitter_Tuning/NeuralNetBoxCoxRespTrans.jpeg", width = 1000, height = 500)
library(caret)
plot(nnetModelTune)
dev.off()





# Random Forest 
set.seed(669)
rfTune <- train(log_mlb_oWAR_6yr_plus_c~.,
                data= milbTrain,
                method="rf",
                preProcess= c("BoxCox","center","scale"),
                tuneGrid = expand.grid(.mtry=seq(20,35, by=5)),
                ntrees=1000,
                importance = TRUE,
                trControl = controlObject)
# Center Scaled : RMSE = 2.352028 ; R^2 = 0.1308038 ; mtry = 25

# Center Scaled BoxCox: RMSE = 2.352266 ; R^2 = 0.1305822 ; mtry = 25
# Center Scaled BoxCox TestSet: RMSE = 2.233384 ; R^2 = 0.1499995 ; mtry = 25
saveRDS(rfTune, "data/Models/Hitters/rfTuneBoxCoxRespTrans.rds")
jpeg("Plots/Hitter_Tuning/RandForBoxCoxRespTrans.jpeg", width = 1000, height = 500)
library(caret)
plot(rfTune)
dev.off()


# SVM Rad
set.seed(669)
svmrTune <- train(log_mlb_oWAR_6yr_plus_c~.,
                  data= milbTrain,
                  method="svmRadial",
                  preProcess= c("BoxCox","center","scale"),
                  tuneLength = 9,
                  
                  trControl = controlObject)
# Center Scaled : RMSE = 2.434119 ; R^2 = 0.08478477 ; sigma = 0.01308432 ; C = 64
saveRDS(svmrTune, "data/Models/Hitters/svmrTuneBoxCoxRespTrans.rds")
jpeg("Plots/Hitter_Tuning/SVMRBoxCoxRespTrans.jpeg", width = 1000, height = 500)
library(caret)
plot(svmrTune)
dev.off()

