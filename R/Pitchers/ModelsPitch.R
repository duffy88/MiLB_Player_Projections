# MiLB Player Projection Modelling Pitchers


library(caret)


milbTrainP <- readRDS("data/Pitchers/milbTrainP.rds")
milbTestP <- readRDS("data/Pitchers/milbTestP.rds")


controlObject <- trainControl(method = "repeatedcv",
                              repeats=1,
                              number=3,
                              verboseIter = TRUE)
# Can only include 1 response remove others and non transformed
milbTrainP$mlb_WAR_28yo <- milbTrainP$mlb_WAR_6yr<- NULL

# Linear Reg
set.seed(669)
linTuneP <- train(log_mlb_WAR_6yr_plus_c~.,
                 data=milbTrainP,
                 method="lm",
                 preProcess= c("BoxCox","center","scale"),
                 trControl = controlObject)
# Center Scaled : RMSE = 2.095884 ; R^2 = 0.05098496

# Center Scaled BoxCox : RMSE = 2.13011 ; R^2 = 0.05659811
# Center Scaled BoxCox Test Set: RMSE = 2.230762 ; R^2 = 0.06594377

# Center Scaled BoxCox RespTrans: RMSE = 0.2415597 ; R^2 = 0.04346326
# Center Scaled BoxCox RespTrans Test Set: RMSE = 2.101261 ; R^2 = 0.06330753

saveRDS(linTuneP, "data/Models/Pitchers/linTunePBoxCoxRespTrans.rds")



# PLS Model
set.seed(669)
plsTuneP <- train( log_mlb_WAR_6yr_plus_c~.,
                  data=milbTrainP,
                  method="pls",
                  preProcess= c("BoxCox","center","scale"),
                  tuneLength =25,
                  trControl = controlObject)
# Center Scaled : RMSE = 2.095884 ; R^2 = 0.05098456 ; n =  14

# Center Scaled BoxCox: RMSE = 2.091186 ; R^2 = 0.05526541 ; n =  24
# Center Scaled BoxCox Test Set: RMSE = 2.073403 ; R^2 = 0.06563023 ; n =  24

# Center Scaled BoxCox RespTrans : RMSE = 0.2415593 ; R^2 = 0.04346618 ; n =  24
# Center Scaled BoxCox RespTrans Test Set: RMSE = 2.101266 ; R^2 = 0.06330164 ; n =  24
saveRDS(plsTuneP, "data/Models/Pitchers/plsTunePBoxCoxRespTrans.rds")
jpeg("Plots/Pitcher_Tuning/PitchPLSBoxCoxRespTrans.jpeg", width = 1000, height = 500)
library(caret)
plot(plsTuneP)
dev.off()


# ENet
enetGrid <- expand.grid(.lambda= c(0,0.001, 0.01, 0.1),
                        .fraction = seq(0.05,1,length=20))
set.seed(669)
enetTuneP <- train(log_mlb_WAR_6yr_plus_c~.,
                  data= milbTrainP,
                  method="enet",
                  preProcess= c("BoxCox","center","scale"),
                  tuneGrid= enetGrid,
                  trControl = controlObject)
# Center Scaled : RMSE = 2.095371 ; R^2 = 0.05139651 ; fraction = 0.7 ; lambda =   0.001

# Center Scaled BoxCox: RMSE = 2.090938 ; R^2 = 0.05541447 ; fraction = 0.6 ; lambda =   0
# Center Scaled BoxCox Test Set: RMSE = 2.073386 ; R^2 = 0.06564577 ; fraction = 0.6 ; lambda =   0

# Center Scaled BoxCox RespTrans: RMSE = 0.2415528 ; R^2 = 0.04372592 ; fraction = 0.65 ; lambda =   0
# Center Scaled BoxCox RespTrans Test Set: RMSE = 2.101261 ; R^2 = 0.06330753 ; fraction = 0.65 ; lambda =   0
saveRDS(enetTuneP, "data/Models/Pitchers/enetTunePBoxCoxRespTrans.rds")
jpeg("Plots/Pitcher_Tuning/PitchENetBoxCoxRespTrans.jpeg", width = 1000, height = 500)
plot(enetTuneP)
dev.off()


# MARS
set.seed(669)
marsTuneP <- train(log_mlb_WAR_6yr_plus_c~.,
                  data= milbTrainP,
                  method="earth",
                  preProcess= c("BoxCox","center","scale"),
                  tuneGrid = expand.grid(.degree=1:5,
                                         .nprune = 2:50),
                  trControl = controlObject)
# Center Scaled : RMSE = 2.044857 ; R^2 = 0.09860744 ; nprune = 34 ; degree = 2

# Center Scaled Box Cox : RMSE = 2.046511 ; R^2 = 0.09813297 ; nprune = 31 ; degree = 2
# Center Scaled Box Cox Test Set: RMSE = 2.030477 ; R^2 = 0.1037088 ; nprune = 31 ; degree = 2

# Center Scaled Box Cox RespTrans: RMSE = 0.2355705 ; R^2 = 0.09226996 ; nprune = 25 ; degree = 3
# Center Scaled Box Cox RespTrans Test Set: RMSE = 2.062612 ; R^2 = 0.09604171 ; nprune = 25 ; degree = 3
saveRDS(marsTuneP, "data/Models/Pitchers/marsTunePBoxCoxRespTrans.rds")
jpeg("Plots/Pitcher_Tuning/PitchMarsBoxCoxRespTrans.jpeg", width = 1000, height = 500)
library(caret)
plot(marsTuneP)
dev.off()


# CART
set.seed(669)
rpartTuneP <- train(log_mlb_WAR_6yr_plus_c~.,
                    data= milbTrainP,
                    method= "rpart",
                    preProcess= c("BoxCox","center","scale"),
                    tuneLength=30,
                    trControl = controlObject) 
# Center Scaled : RMSE = 2.108282 ; R^2 = 0.04241561 ; cp= 0.006457563

# Center Scaled BoxCox : RMSE = 2.108282 ; R^2 = 0.04241561 ; cp= 0.006457563

# Center Scaled Box Cox RespTrans: RMSE = 0.2410438 ; R^2 = 0.05016020 ; cp= 0.003034289
# Center Scaled Box Cox RespTrans Test Set: RMSE = 2.132555 ; R^2 = 0.01989857 ; cp= 0.003034289
saveRDS(rpartTuneP, "data/Models/Pitchers/rpartTunePBoxCoxRespTrans.rds")
jpeg("Plots/Pitcher_Tuning/PitchRPartBoxCoxRespTrans.jpeg", width = 1000, height = 500)
library(caret)
plot(rpartTuneP)
dev.off()


# C-Inf Tree
set.seed(669)
ctreeTuneP <- train(log_mlb_WAR_6yr_plus_c~.,
                    data= milbTrainP,
                    method = "ctree",
                    preProcess= c("BoxCox","center","scale"),
                    tuneLength =10,
                    trControl = controlObject)
# Center Scaled : RMSE = 2.090449 ; R^2 = 0.06448212 ; mincriterion = 0.7722222

# Center Scaled BoxCox : RMSE = 2.100337 ; R^2 = 0.05338807 ; mincriterion = 0.99

# Center Scaled Box Cox RespTrans: RMSE = 0.2398182 ; R^2 = 0.05887268 ; mincriterion = 0.99
# Center Scaled Box Cox RespTrans Test Set: RMSE = 2.10693 ; R^2 = 0.04300392 ; mincriterion = 0.99
saveRDS(ctreeTuneP, "data/Models/Pitchers/ctreeTunePBoxCoxRespTrans.rds")
jpeg("Plots/Pitcher_Tuning/PitchCTreeBoxCoxRespTrans.jpeg", width = 1000, height = 500)
library(caret)
plot(ctreeTuneP)
dev.off()


# Tree (bagged)
set.seed(669)
treebagTuneP <- train(log_mlb_WAR_6yr_plus_c~.,
                      data= milbTrainP,
                      method="treebag",
                      preProcess= c("BoxCox","center","scale"),
                      trControl = controlObject)
# Center Scaled : RMSE = 2.098619 ; R^2 = 0.0518542

# Center Scaled BoxCox : RMSE = 2.098565 ; R^2 = 0.05193581

# Center Scaled Box Cox RespTrans: RMSE = 0.2423876 ; R^2 =  0.03719598
# Center Scaled Box Cox RespTrans Test Set: RMSE = 2.146226 ; R^2 =  0.007312239
saveRDS(treebagTuneP, "data/Models/Pitchers/treebagTunePBoxCoxRespTrans.rds")



# Cubist 
# Max committess = 100 ; neighbors = 9
cubistGrid <- expand.grid(.committees =c(1,50,100),
                          .neighbors =c(0,1,5,9))
set.seed(669)
cubistTuneP <- train(log_mlb_WAR_6yr_plus_c~.,
                     data= milbTrainP,
                     method= "cubist",
                     preProcess= c("BoxCox","center","scale"),
                     tuneGrid=cubistGrid,
                     trControl = controlObject)
# Center Scaled : RMSE = 2.125473 ; R^2 = 0.05279434 ; committees = 100 ; neighbors = 9

# Center Scaled BoxCox : RMSE = 2.125521 ; R^2 = 0.07140590 ; committees = 100 ; neighbors = 0

# Center Scaled Box Cox RespTrans: RMSE = 0.2407067 ; R^2 = 0.08205754 ; committees = 50 ; neighbors = 0
# Center Scaled Box Cox RespTrans Test Set: RMSE = 2.130725 ; R^2 = 0.05757379 ; committees = 50 ; neighbors = 0
saveRDS(cubistTuneP, "data/Models/Pitchers/cubistTunePBoxCoxRespTrans.rds")
jpeg("Plots/Pitcher_Tuning/PitchCubistBoxCoxRespTrans.jpeg", width = 1000, height = 500)
library(caret)
plot(cubistTuneP)
dev.off()




# GBM  
gbmGrid <- expand.grid(.interaction.depth =seq(1,9,by=2),
                       .n.trees = seq(200,2000, by = 200),
                       .shrinkage = c(0.001,0.01,0.1),
                       .n.minobsinnode =10)
set.seed(669)
gbmTuneP <- train(log_mlb_WAR_6yr_plus_c~.,
                  data= milbTrainP,
                  method="gbm",
                  preProcess= c("BoxCox","center","scale"),
                  tuneGrid = gbmGrid,
                  verbose=FALSE,
                  trControl = controlObject)
# Center Scaled : RMSE = 2.029255 ; R^2 = 0.11086343 ; 
# Center Scaled : n.trees = 1400 ; interaction.depth = 7 ; shrinkage = 0.01 ; n.minobsinnode = 10

# Center Scaled BoxCox : RMSE = 2.028970 ; R^2 = 0.11109478 ; 
# Center Scaled BoxCox : n.trees = 1400 ; interaction.depth = 7 ; shrinkage = 0.01 ; n.minobsinnode = 10

# Center Scaled BoxCox RespTrans: RMSE = 0.2343688 ; R^2 = 0.10029432 ; 
# Center Scaled BoxCox RespTrans: n.trees = 1400 ; interaction.depth = 9 ; shrinkage = 0.01 ; n.minobsinnode = 10
# Center Scaled BoxCox RespTrans TestSet: RMSE = 2.06444 ; R^2 = 0.09049812 ; 
saveRDS(gbmTuneP, "data/Models/Pitchers/gbmTunePBoxCoxRespTrans.rds")
jpeg("Plots/Pitcher_Tuning/PitchGBMBoxCoxRespTrans.jpeg", width = 1000, height = 500)
library(caret)
plot(gbmTuneP)
dev.off()







# Random Forest 
set.seed(669)
rfTuneP <- train(log_mlb_WAR_6yr_plus_c~.,
                data= milbTrainP,
                method="rf",
                preProcess= c("BoxCox","center","scale"),
                tuneGrid = expand.grid(.mtry=seq(6,14, by=2)),
                ntrees=1000,
                importance = TRUE,
                trControl = controlObject)
# Center Scaled : RMSE = 2.047456 ; R^2 = 0.09613907 ; mtry = 10

# Center Scaled BoxCox : RMSE = 2.047626 ; R^2 = 0.09596546 ; mtry = 10
# Center Scaled BoxCox TestSet: RMSE = 1.303295 ; R^2 = 0.7394576 ; mtry = 10
# What in the world with that Test Set performance? 
# Model doesn't seem to have a stray response as a hidden predictor...what else?

# Center Scaled BoxCox RespTrans: RMSE = 0.2365201 ; R^2 = 0.08491527 ; mtry = 12
# Center Scaled BoxCox RespTrans TestSet: RMSE = 2.075958 ; R^2 = 0.07393586 ; mtry = 12
saveRDS(rfTuneP, "data/Models/Pitchers/rfTunePBoxCoxRespTrans.rds")
jpeg("Plots/Pitcher_Tuning/PitchRandForBoxCoxRespTrans.jpeg", width = 1000, height = 500)
library(caret)
plot(rfTuneP)
dev.off()


# Model Trees 
#set.seed(669) 
#mtTuneP <- train(log_mlb_WAR_6yr_plus_c~.,
 #               data= milbTrainP,
  #              method="M5",
   #             preProcess= c("BoxCox","center","scale"),
    #            trControl = controlObject)
# Orig : RMSE =  R^2 = 
# CS : RMSE =  R^2 = 
#saveRDS(mtTuneP, "data/Models/Pitchers/mtTunePBoxCoxRespTrans.rds")
#jpeg("Plots/Pitcher_Tuning/PitchModelTreesBoxCoxRespTrans.jpeg", width = 1000, height = 500)
#library(caret)
#plot(mtTuneP)
#dev.off()


# NeuralNet
nnetGrid <- expand.grid(.decay = c(1,1.5,2,2.5,3), 
                        .size = c(seq(1,7, by=2)),
                        .bag=c(T,F)) 
set.seed(669) 
nnetModelTuneP <- train(log_mlb_WAR_6yr_plus_c~.,
                       data= milbTrainP,
                       method="avNNet",
                       preProcess= c("BoxCox","center","scale"),
                       tuneGrid= nnetGrid,
                       linout =TRUE,
                       trace =FALSE,
                       maxit=1000,
                       trControl = controlObject) 
# Center Scaled : RMSE = 2.009796 ; R^2 = 0.12788832
# Center Scaled : size = 3 ; decay = 2.5 ; bag = FALSE

# Center Scaled BoxCox : RMSE = 2.010296 ; R^2 = 0.12754747
# Center Scaled BoxCox : size = 3 ; decay = 3 ; bag = FALSE
# Center Scaled BoxCox Test Set: RMSE = 2.094232 ; R^2 = 0.1805437

# Center Scaled BoxCox RespTrans: RMSE = 0.2330333 ; R^2 = 0.11015008
# Center Scaled BoxCox RespTrans: size = 5 ; decay = 1 ; bag = TRUE
# Center Scaled BoxCox RespTrans Test Set: RMSE = 2.039494 ; R^2 = 0.1228562
saveRDS(nnetModelTuneP, "data/Models/Pitchers/nnetModelTunePBoxCoxRespTrans.rds")
jpeg("Plots/Pitcher_Tuning/PitchNeuralNetBoxCoxRespTrans.jpeg", width = 1000, height = 500)
library(caret)
plot(nnetModelTuneP)
dev.off()



# SVM Rad
set.seed(669)
svmrTuneP <- train(log_mlb_WAR_6yr_plus_c~.,
                   data= milbTrainP,
                   method="svmRadial",
                   preProcess= c("BoxCox","center","scale"),
                   tuneLength = 9,
                   
                   trControl = controlObject)
# Center Scaled : RMSE = 2.120196 ; R^2 = 0.05610218 ; sigma = 0.01371828 ; C = 16

# Center Scaled BoxCox : RMSE = 2.120705 ; R^2 = 0.05729892 ; sigma = 0.01371828 ; C = 16
# Center Scaled BoxCox TestSet: RMSE = 2.054702 ; R^2 = 0.1503358 ; sigma = 0.01371828 ; C = 16

# Center Scaled BoxCox RespTrans: RMSE = 0.2414857 ; R^2 = 0.06186988 ; sigma = 0.01371828 ; C = 16
# Center Scaled BoxCox RespTrans TestSet: RMSE = 2.123461 ; R^2 = 0.05675109 ; sigma = 0.01371828 ; C = 16
saveRDS(svmrTuneP, "data/Models/Pitchers/svmrTunePBoxCoxRespTrans.rds")
jpeg("Plots/Pitcher_Tuning/PitchSVMRBoxCoxRespTrans.jpeg", width = 1000, height = 500)
library(caret)
plot(svmrTuneP)



dev.off()