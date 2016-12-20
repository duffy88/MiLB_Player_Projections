# MiLB Player Projection PreProcssing Pitchers
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

# Shift c, for log(x +c) transformation
c <- 5

# Load data
milbP <- readRDS("data/Pitchers/milbP.rds")
mlbP <- readRDS("data/Pitchers/mlbP.rds")

# Remove players who are not yet 30 years of age (unsure of full potential)
milbP <- subset(milbP, (Year-Age)<=1986)

# Define possible modelling columns as numeric, categoric , response or transformed response
# Dont use : W/L Ratio, SO/BB Ratio, both end up with NA's, including native variables anyway
predcol <- c("Year","Age","AgeDif","W","L","ERA","RAvg","G","GS","GF","CG","SHO",
             "SV","IP","H","R","ER","HR","BB","IBB","SO","HBP","BK","WP","BF",
             "WHIP","H9","HR9","BB9","SO9")
predcolFact <- c("Lg","Lev")
respcol <- c("mlb_WAR_6yr")
respcolTrans <- c("log_mlb_WAR_6yr_plus_c")

# Removes ID variables (bref_id, MiLB/MLB team)
milbP <- milbP[ , c(predcol, predcolFact,respcol)]

# Missing Values
# Examine Missing
colSums(sapply(milbP, is.na))
# No missing values!
# Literally just one player has NA for IBB, BK...
milbP$IBB[is.na(milbP$IBB)]<- 0
milbP$BK[is.na(milbP$BK)]<- 0

# Ensure all factor variables are factors
for(i in predcolFact){
  milbP[, i] <- as.factor(milbP[ , i ])
}


# Create data frames of only numeric and factor variables
milbP_fact <- milbP[ , predcolFact]
milbP_num <- milbP[ , predcol]


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
jpeg("Plots/Pitcher_PreProcess/FactorHistogram.jpeg", width = 1000, height = 500)
doPlots(milbP_fact, fun = plotHist, ii = 2:1, ncol = 1)
dev.off()
# Level : Very few player seasons in Foreign Rookie level (FRk), 
#  most player seasons at higher levels of MiLB
# League : Very few players in Dominican/Venezuelan Summer Leagues.
# The other small league (AA) is the American Association (old AAA league disbanded 1997)

# Lets examine density plots and skewness of numeric variables
jpeg("Plots/Pitcher_PreProcess/NumVarDistributionSkewness1to10.jpeg", width = 1000, height = 500)
doPlots(milbP_num, fun = plotDen, ii = 1:10, ncol = 2)
dev.off()
# Year distribution kinda funky based on how we've taken out players who aren't yet 30
# Nothing here terribly skewed

# Lets examine density plots and skewness of numeric variables
jpeg("Plots/Pitcher_PreProcess/NumVarDistributionSkewness11to20.jpeg", width = 1000, height = 500)
doPlots(milbP_num, fun = plotDen, ii = 11:20, ncol = 2)
dev.off()
# CG,SHO, SV skewed, IBB kinda skewed

# Lets examine density plots and skewness of numeric variables
jpeg("Plots/Pitcher_PreProcess/NumVarDistributionSkewness21to30.jpeg", width = 1000, height = 500)
doPlots(milbP_num, fun = plotDen, ii = 21:30, ncol = 2)
dev.off()
# BK skewed

correlationsP <- cor(milbP[ , c(predcol, respcol)])
# No predictors highly correlated with response
# GS and SO are most correlated with cor = 0.08-0.09

# Examine only between predictor correlation
correlationsP <- cor(milbP[ , c(predcol)])


jpeg("Plots/Pitcher_PreProcess/CorrPlotPitch.jpeg", width = 1000, height = 500)
corrplot::corrplot(correlationsP,type="lower",order = "hclust", method="square")
dev.off()
# Many counting stats highly correlated with BF, also clusters of correlated variables for :
# - 

# Not terribly interested in feature plots, no numeric predictors that correlated with response
for(i in 1:length(predcol)){
  jpeg(paste("Plots/Pitcher_PreProcess/FeaturePlots/FeaturePlot",predcol[i],".jpeg",sep=""), width = 1000, height = 500)
  
  print(featurePlot(x = milbP[,predcol[i]],
                    y = milbP[,respcol],
                    between = list(x=1, y=1),
                    type= c("g","p","smooth"),
                    col.line= c("red"),
                    col = "black",
                    lwd=3))
  
  dev.off()
  
  
}
# Nothing really useful..



# Boruta Analysis to see which variables may be important
# Take 10 hours to run, tells us all variables are important (except BK/SV)...useless
set.seed(669)
#bor.resultsP <- Boruta(milbP[ , c(predcol, predcolFact)], milbP[, c(respcol)],
 #                   maxRuns =101,
  #                doTrace=0)

bor.resultsP

impAttrP <- getSelectedAttributes(bor.resultsP)

jpeg("Plots/Pitcher_PreProcess/VariableImportanceBoruta.jpeg", width = 1000, height = 500)
plot(bor.resultsP)
dev.off()

arrange(cbind(attr=rownames(attStats(bor.resultsP)), attStats(bor.resultsP)),desc(medianImp))
# Top 5 Imporant Variables : SO9, Age, WHIP, H9, ERA

# Resolving Response Skewness
skewRespP <- skewness(milbP[ , respcol])
kurtRespP <- kurtosis(milbP[ , respcol])
# mlb_WAR_6yr also very skewed, with negative and zeros
# skewness = 6.213924 ; kurtosis = 53.25155

jpeg("Plots/Pitcher_PreProcess/RespDistributionPitch.jpeg", width = 1000, height = 500)
densityplot(~mlb_WAR_6yr, milbP)
dev.off()
# Vast majority players with 0 WAR, top players over 30 WAR
# Includes negative players (min = -4.2 WAR), players who make majors but end up sucking.

# Transformation of log(x + c) type, min c = 4.2
milbP[ , respcolTrans] <- log(milbP[ , respcol] + c )

skewTransRespP <- skewness(milbP[ , respcolTrans])
kurtTransRespP <- kurtosis(milbP[ , respcolTrans])
# skewness = 2.741747 ; kurtosis = 15.06597
# Still skewed but appears to be the best we can do with simple log(x+c) transformation


jpeg("Plots/Pitcher_PreProcess/TransRespDistributionPitch.jpeg", width = 1000, height = 500)
densityplot(~log_mlb_WAR_6yr_plus_c, milbP)
dev.off()

# Separate into training/testing sets
set.seed(975)
forTraining <- createDataPartition(milbP[,ncol(milbP)],
                                   p =0.8)[[1]]
milbTrainP <- milbP[forTraining,]
milbTestP <- milbP[-forTraining, ]


saveRDS(milbTrainP,"data/Pitchers/milbTrainP.rds")
saveRDS(milbTestP,"data/Pitchers/milbTestP.rds")
