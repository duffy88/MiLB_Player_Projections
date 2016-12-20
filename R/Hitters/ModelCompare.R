# Model Comparisons

library(caret)

ctreeTune <- readRDS("data/Models/Hitters/ctreeTune.rds")
enetTune <- readRDS("data/Models/Hitters/enetTune.rds")
gbmTune <- readRDS("data/Models/Hitters/gbmTune.rds")
linTune <- readRDS("data/Models/Hitters/linTune.rds")
marsTune <- readRDS("data/Models/Hitters/marsTune.rds")
plsTune <- readRDS("data/Models/Hitters/plsTune.rds")
rpartTune <- readRDS("data/Models/Hitters/rpartTune.rds")
treebagTune <- readRDS("data/Models/Hitters/treebagTune.rds")
svmrTune <- readRDS("data/Models/Hitters/svmrTune.rds")
cubistTune <- readRDS("data/Models/Hitters/cubistTune.rds")
rfTune <- readRDS("data/Models/Hitters/rfTune.rds")
nnetModelTune <- readRDS("data/Models/Hitters/nnetModelTune.rds")


resamp <- resamples(list("Linear Reg" = linTune,
                            "PLS" = plsTune,
                            "Elastic Net" = enetTune,
                            MARS = marsTune,
                            SVM = svmrTune,
                            "Neural Networks" = nnetModelTune,
                            CART = rpartTune,
                            "Cond Inf Tree" = ctreeTune,
                            "Bagged Tree" = treebagTune,
                            "Boosted Tree" = gbmTune,
                            "Random Forest"= rfTune,
                            Cubist = cubistTune))
                     #  "Model Trees" = mtTune)
jpeg("Plots/Hitter_Model_Comp/CompareModelsRMSE.jpeg", width = 1000, height = 500)
parallelplot(resamp)
dev.off()

jpeg("Plots/Hitter_Model_Comp/CompareModelsR2.jpeg", width = 1000, height = 500)
parallelplot(resamp, metric = "Rsquared")
dev.off()



rmse <- resamp$values[,grepl("RMSE", names(resamp$values))]
library(reshape2)
rmse <- melt(rmse)

jpeg("Plots/Hitter_Model_Comp/CompareModelsRMSEBoxPlot.jpeg", width = 1000, height = 500)
bwplot(variable~value ,data=rmse, vertical = TRUE)
dev.off()


