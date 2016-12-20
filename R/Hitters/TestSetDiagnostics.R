# MiLB Projection Diagnostics


library(caret)


milbTrain <- readRDS("data/Hitters/milbTrain.rds")
milbTest <- readRDS("data/Hitters/milbTest.rds")

# Check c value!!
c <- 5
ModelFile <- "data/Models/Hitters/gbmTuneBoxCoxRespTrans.rds"
diag.model <- readRDS(ModelFile)

diag.model.res <- as.data.frame( predict(diag.model,
                                         milbTest)) 
if(!grepl("Trans", ModelFile)){
  names(diag.model.res)[1] <- "Pred"
  
}else {
  names(diag.model.res)[1] <- "TransPred"
  diag.model.res$Pred <- exp(diag.model.res$TransPred)-c
}
diag.model.res$Obs <- milbTest$mlb_oWAR_6yr


diag.model.res$Resid <- with( diag.model.res, Obs - Pred)
diag.model.res$StdResid <- with(diag.model.res, Resid / sd(Resid))
diag.model.res$SqRtAbsStdResid <- with(diag.model.res, sqrt(abs(Resid / sd(Resid))))

with(diag.model.res, cor(Pred, Obs)^2)
with(diag.model.res, RMSE(Pred, Obs))

jpeg("Plots/Hitter_TestSet_Diagnostics/ResidPred.jpeg", width = 1000, height = 500)
xyplot(Resid ~ Pred,
       data = diag.model.res,
       type= c("p","smooth"),
       col="black",
       col.line="red"
)
dev.off()

jpeg("Plots/Hitter_TestSet_Diagnostics/ObsPred.jpeg", width = 1000, height = 500)
xyplot(Obs ~ Pred,
       data = diag.model.res,
       type= c("p","smooth"),
       col="black",
       col.line="red"
)
dev.off()

jpeg("Plots/Hitter_TestSet_Diagnostics/SqRtAbsStdResidPred.jpeg", width = 1000, height = 500)
xyplot(SqRtAbsStdResid ~ Pred,
       data = diag.model.res,
       type= c("p","smooth"),
       col="black",
       col.line="red"
)
dev.off()

jpeg("Plots/Hitter_TestSet_Diagnostics/NormQQ.jpeg", width = 1000, height = 500)
qqnorm(diag.model.res$StdResid, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="Norm Q-Q", plot.it = T) 
qqline(diag.model.res$StdResid, col="red") 
dev.off()


