# MiLB Projection Diagnostics Pitch


library(caret)


milbTrainP <- readRDS("data/Pitchers/milbTrainP.rds")
milbTestP <- readRDS("data/Pitchers/milbTestP.rds")

# Check c value!!
c <- 5
ModelFile <- "data/Models/Pitchers/svmrTunePBoxCoxRespTrans.rds"
diag.model <- readRDS(ModelFile)


diag.model.res <- as.data.frame( predict(diag.model,
                                         milbTestP)) 
if(!grepl("Trans", ModelFile)){
  names(diag.model.res)[1] <- "Pred"
  
}else {
  names(diag.model.res)[1] <- "TransPred"
  diag.model.res$Pred <- exp(diag.model.res$TransPred)-c
}
diag.model.res$Obs <- milbTestP$mlb_WAR_6yr


diag.model.res$Resid <- with( diag.model.res, Obs - Pred)
diag.model.res$StdResid <- with(diag.model.res, Resid / sd(Resid))
diag.model.res$SqRtAbsStdResid <- with(diag.model.res, sqrt(abs(Resid / sd(Resid))))

with(diag.model.res, cor(Pred, Obs)^2)
with(diag.model.res, RMSE(Pred, Obs))

jpeg("Plots/Pitcher_TestSet_Diagnostics/ResidPredPitch.jpeg", width = 1000, height = 500)
xyplot(Resid ~ Pred,
       data = diag.model.res,
       type= c("p","smooth"),
       col="black",
       col.line="red"
)
dev.off()

jpeg("Plots/Pitcher_TestSet_Diagnostics/ObsPredPitch.jpeg", width = 1000, height = 500)
xyplot(Obs ~ Pred,
       data = diag.model.res,
       type= c("p","smooth"),
       col="black",
       col.line="red"
)
dev.off()


jpeg("Plots/Pitcher_TestSet_Diagnostics/SqRtAbsStdResidPredPitch.jpeg", width = 1000, height = 500)
xyplot(SqRtAbsStdResid ~ Pred,
       data = diag.model.res,
       type= c("p","smooth"),
       col="black",
       col.line="red"
)
dev.off()

jpeg("Plots/Pitcher_TestSet_Diagnostics/NormQQPitch.jpeg", width = 1000, height = 500)
qqnorm(diag.model.res$StdResid, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="Norm Q-Q", plot.it = T) 
qqline(diag.model.res$StdResid, col="red") 
dev.off()


