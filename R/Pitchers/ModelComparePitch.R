# Model Comparisons Pitchers

library(caret)

ctreeTuneP <- readRDS("data/Models/Pitchers/ctreeTuneP.rds")
cubistTuneP <- readRDS("data/Models/Pitchers/cubistTuneP.rds")
enetTuneP <- readRDS("data/Models/Pitchers/enetTuneP.rds")
gbmTuneP <- readRDS("data/Models/Pitchers/gbmTuneP.rds")
linTuneP <- readRDS("data/Models/Pitchers/linTuneP.rds")
marsTuneP <- readRDS("data/Models/Pitchers/marsTuneP.rds")
plsTuneP <- readRDS("data/Models/Pitchers/plsTuneP.rds")
rpartTuneP <- readRDS("data/Models/Pitchers/rpartTuneP.rds")
svmrTuneP <- readRDS("data/Models/Pitchers/svmrTuneP.rds")
treebagTuneP <- readRDS("data/Models/Pitchers/treebagTuneP.rds")
rfTuneP <- readRDS("data/Models/Pitchers/rfTuneP.rds")
nnetModelTuneP <- readRDS("data/Models/Pitchers/nnetModelTuneP.rds")

resampP <- resamples(list("Linear Reg" = linTuneP,
                         "PLS" = plsTuneP,
                         "Elastic Net" = enetTuneP,
                         MARS = marsTuneP,
                         SVM = svmrTuneP,
                         "Neural Networks" = nnetModelTuneP,
                         CART = rpartTuneP,
                         "Cond Inf Tree" = ctreeTuneP,
                         "Bagged Tree" = treebagTuneP,
                        "Boosted Tree" = gbmTuneP,
                     "Random Forest"= rfTuneP,
                         Cubist = cubistTuneP))
                  #  "Model Trees" = mtTuneP)
jpeg("Plots/Pitcher_Model_Comp/CompareModelsPitchRMSE.jpeg", width = 1000, height = 500)
parallelplot(resampP)
dev.off()

jpeg("Plots/Pitcher_Model_Comp/CompareModelsPitchR2.jpeg", width = 1000, height = 500)
parallelplot(resampP, metric = "Rsquared")
dev.off()



rmse <- resampP$values[,grepl("RMSE", names(resampP$values))]
library(reshape2)
rmse <- melt(rmse)

jpeg("Plots/Pitcher_Model_Comp/CompareModelsPitchRMSEBoxPlot.jpeg", width = 1000, height = 500)
bwplot(variable~value ,data=rmse, vertical = TRUE)
dev.off()



