# MiLB Player Projection PreProcssing 
# By Doug Duffy 2016

# Load packages
library(corrplot)
library(e1071)
library(caret)
library(data.table)
library(Boruta)
library(plyr)
library(dplyr)
library(pROC)
library(gridExtra)

# Shift c, for log(x +c) transformation to normalize response
c <- 5

# Load data
milb <- readRDS("data/Hitters/milb.rds")
mlb <- readRDS("data/Hitters/mlb.rds")

# Remove players who are not yet 30 years of age (unsure of full potential)
milb <- subset(milb, (Year-Age)<=1986)

# 45,516 Player Seasons
# 11,283 Players

# Define possible modelling columns as numeric, categoric , response or transformed response
predcol <- c("Year","Age","AgeDif","G","PA","AB","R","H","X2B",
             "X3B","HR","RBI","SB","CS","BB","SO","BA","OBP","SLG","OPS","TB",
             "GDP","HBP","SH","SF","IBB","BBPct","SOPct","ISO","BABIP","BB.SORat",
             "X1B","SBPct","wOBA","Spd")
predcolFact <- c("Lg","Lev")
respcol <- c("mlb_oWAR_6yr")
respcolTrans <- c("log_mlb_oWAR_6yr_plus_c")

# Removes ID variables (bref_id, MiLB/MLB team)
milb <- milb[ , c(predcol,predcolFact, respcol)]

# Ensure all factor variables are factors
for(i in predcolFact){
  milb[, i] <- as.factor(milb[ , i ])
}

# Create data frames of only numeric and factor variables
milb_fact <- milb[ , predcolFact]
milb_num <- milb[ , predcol]

# Examine Missing
colSums(sapply(milb, is.na))
# No missing values!

# Setup function to plot histograms of factor variables
plotHist <- function(data_in, i) {
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}


# Function to plot density plots of numeric variables along with the skewness of their distribution
plotDen <- function(data_in, i){
  data <- data.frame(x=data_in[[i]])#, SalePrice = data_in$SalePrice
  p <- ggplot(data= data) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
    xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme_light() 
  return(p)
  
}

# Function to run plots in groups 
doPlots <- function(data_in, fun, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

# Plot and save histograms of Lg/Lvl factors 
jpeg("Plots/Hitter_PreProcess/FactorHistogram.jpeg", width = 1000, height = 500)
doPlots(milb_fact, fun = plotHist, ii = 2:1, ncol = 1)
dev.off()
# Level : Very few player seasons in Foreign Rookie level (FRk), 
#  most player seasons at higher levels of MiLB
# League : Very few players in Dominican/Venezuelan Summer Leagues.
# The other small league (AA) is the American Association (old AAA league disbanded 1997)

# Lets examine density plots and skewness of numeric variables
jpeg("Plots/Hitter_PreProcess/NumVarDistributionSkewness1to6.jpeg", width = 1000, height = 500)
doPlots(milb_num, fun = plotDen, ii = 1:6, ncol = 2)
dev.off()
# Year distribution kinda funky based on how we've taken out players who aren't yet 30
# Nothing here terribly skewed

jpeg("Plots/Hitter_PreProcess/NumVarDistributionSkewness7to12.jpeg", width = 1000, height = 500)
doPlots(milb_num, fun = plotDen, ii = 7:12, ncol = 2)
dev.off()
# HR and 3B are somewhat skewed, RBI less so

jpeg("Plots/Hitter_PreProcess/NumVarDistributionSkewness13to18.jpeg", width = 1000, height = 500)
doPlots(milb_num, fun = plotDen, ii = 13:18, ncol = 2)
dev.off()
# SB, CS skewed; BB, SO somewhat skewed; BA/OBP remarkably normally distributed

jpeg("Plots/Hitter_PreProcess/NumVarDistributionSkewness19to24.jpeg", width = 1000, height = 500)
doPlots(milb_num, fun = plotDen, ii = 19:24, ncol = 2)
dev.off()
# HBP, SH skewed; TB, GDP somewhat; again rate stats normally distributed,

jpeg("Plots/Hitter_PreProcess/NumVarDistributionSkewness25to30.jpeg", width = 1000, height = 500)
doPlots(milb_num, fun = plotDen, ii = 25:30, ncol = 2)
dev.off()
# IBB skewed

jpeg("Plots/Hitter_PreProcess/NumVarDistributionSkewness31to35.jpeg", width = 1000, height = 500)
doPlots(milb_num, fun = plotDen, ii = 31:35, ncol = 2)
dev.off()
# BB:SO skewed

correlations <- cor(milb[ , c(predcol,respcol)])
# No predictors highly correlated with response
# OPS and wOBA are most correlated with cor = 0.17

# Examine only between predictor correlation
correlations <- cor(milb[ , c(predcol)])

jpeg("Plots/Hitter_PreProcess/CorrPlot.jpeg", width = 1000, height = 500)
corrplot::corrplot(correlations,type="lower",order = "hclust", method="square")
dev.off()
# Most counting stats highly correlated with PA, also clusters of correlated variables for :
# - Speed (SBPct, SB, SPd, CS, X3B, SH)
# - Rate stats (BA, OBP, SLG, OPS, wOBA, BABIP)
# - Power (HR, ISO)
# - Plate discipline (BBPct, BB.SORat)


# Not terribly interested in feature plots, no numeric predictors that correlated with response
for(i in 1:length(predcol)){
  jpeg(paste("Plots/Hitter_PreProcess/FeaturePlots/FeaturePlot",predcol[i],".jpeg",sep=""), width = 1000, height = 500)
  
  print(featurePlot(x = milb[,predcol[i]],
                    y = milb[,respcol],
                    between = list(x=1, y=1),
                    type= c("g","p","smooth"),
                    col.line= c("red"),
                    col = "black",
                    lwd=3))
  
  dev.off()
  
  
}
# Nothing really useful..


# Boruta Analysis to see which variables may be important
# Take 10 hours to run, tells us all variables are important (except IBB)...useless
set.seed(669)
#bor.results <- Boruta(milb[ , c(predcol, predcolFact)], milb[, c(respcol)],
 #                    maxRuns =101,
  #                  doTrace=0)

bor.results

impAttr <- getSelectedAttributes(bor.results)

jpeg("Plots/Hitter_PreProcess/VariableImportanceBoruta.jpeg", width = 1000, height = 500)
plot(bor.results)
dev.off()

arrange(cbind(attr=rownames(attStats(bor.results)), attStats(bor.results)),desc(medianImp))


# Resolving Response Skewness

skewResp <- skewness(milb[ , respcol])
kurtResp <- kurtosis(milb[ , respcol])
# mlb_oWAR_6yr very skewed, with negative and zeros
# skewness = 6.313597 ; kurtosis = 50.05255

# Density plot of response (MLB WAR)
jpeg("Plots/Hitter_PreProcess/RespDistribution.jpeg", width = 1000, height = 500)
densityplot(~mlb_oWAR_6yr, milb)
dev.off()
# Vast majority players with 0 WAR, top players over 30 WAR
# Includes negative players (min = -4.2 WAR), players who make majors but end up sucking.

c <- 5

# Transformation of log(x + c) type, min c = 4.8
milb[ , respcolTrans] <- log(milb[ , respcol] + c )


skewTransResp <- skewness(milb[ , respcolTrans])
kurtTransResp <- kurtosis(milb[ , respcolTrans])
# skewness = 3.650643 ; kurtosis = 18.12062


jpeg("Plots/Hitter_PreProcess/TransRespDistribution.jpeg", width = 1000, height = 500)
densityplot(~log_mlb_oWAR_6yr_plus_c, milb)
dev.off()
# Still skewed, but this seems to be as good as we can do. 
# All higher shifts increase skewness



# Separate into training/testing sets
set.seed(975)
forTraining <- createDataPartition(milb[,ncol(milb)],
                                   p =0.8)[[1]]
milbTrain <- milb[forTraining,]
milbFiltTrain <- milbFilt[forTraining,]

# 36,414 Player Seasons
# 10,629 Players

milbTest <- milb[-forTraining, ]
milbFiltTest <- milbFilt[-forTraining, ]

# 9,102 Player Seasons
# 5,697 Players

saveRDS(milbTrain,"data/Hitters/milbTrain.rds")
saveRDS(milbTest,"data/Hitters/milbTest.rds")
saveRDS(milbFiltTrain,"data/Hitters/milbFiltTrain.rds")
saveRDS(milbFiltTest,"data/Hitters/milbFiltTest.rds")
