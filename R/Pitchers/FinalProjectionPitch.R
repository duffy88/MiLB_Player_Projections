# MiLB Player Projection Pitch


library(corrplot)
library(e1071)
library(caret)

# Shift c, for log(x +c) transformation to normalize response
c <- 5

milbP <- readRDS("data/Pitchers/milbP.rds")
mlbP <- readRDS("data/Pitchers/mlbP.rds")

nametable <- readRDS("data/PlayerNames.rds")

diag.model <-readRDS("data/Models/Pitchers/greedy_ensembleP.rds")

# Keep players who are not yet 30 years of age (unsure of full potential)
#milbP <- subset(milbP, (Year-Age)>1986)



# Define possible modelling columns as numeric, categoric , response or transformed response
# Dont use : W/L Ratio, SO/BB Ratio, both end up with NA's, including native variables anyway
predcol <- c("Year","Age","AgeDif","W","L","ERA","RAvg","G","GS","GF","CG","SHO",
             "SV","IP","H","R","ER","HR","BB","IBB","SO","HBP","BK","WP","BF",
             "WHIP","H9","HR9","BB9","SO9")
predcolFact <- c("Lg","Lev")
respcol <- c("mlb_WAR_6yr")
respcolTrans <- c("log_mlb_WAR_6yr_plus_c")

ident <- milbP[ , c("Aff","bref_id","IP","ERA")]

milbP <- milbP[ , c(predcol, predcolFact)]



# Two missing values (1 BK, 1 IBB)
milbP$IBB[ is.na(milbP$IBB)] <- 0
milbP$BK[ is.na(milbP$BK)] <- 0


# Ensure all factor variables are factors
for(i in predcolFact){
  milbP[, i] <- as.factor(milbP[ , i ])
}

diag.model.res <- as.data.frame( predict(diag.model,
                                         milbP)) 
diag.model.res <- exp(diag.model.res)- c

names(diag.model.res)[1] <- "Pred_WAR_6yr"


milbP$Pred_WAR_6yr <- round(diag.model.res$Pred_WAR_6yr,1)
milbP <- cbind(milbP, ident)


milbP <- milbP[ order(milbP$Pred_WAR_6yr, decreasing = T),]

milbShinyP <- milbP[ , c("Pred_WAR_6yr","Year","bref_id","Age","Aff","Lev","Lg","G","IP",
                         "ERA","WHIP","W","L","SV","GS","SO9","BB9","HR9","H9")]


milbShinyP <- merge(milbShinyP, nametable, by="bref_id", all.x=T)

milbShinyP$Name2 <- paste('<a href="',milbShinyP$urlMiLB,'" target="_blank" >', #class="btn btn-primary"
                         milbShinyP$Name,'</a>', sep="")

# Remove the pitcher-seasons where pitcher was over 30, I believe these are rehab assignments (?) with > 100 BF
milbShinyP <- milbShinyP[ milbShinyP$Age <= 30, ]

names(milbShinyP)[names(milbShinyP)=="Pred_WAR_6yr"] <- "Pred WAR"


milbShinyP <- milbShinyP[ order(milbShinyP[ , "Pred WAR"], decreasing = TRUE),
                          c("Pred WAR","Year","Name2","Age","Aff","Lev","Lg","G","IP",
                         "ERA","WHIP","W","L","SV","GS","SO9","BB9","HR9","H9")]

names(milbShinyP)[names(milbShinyP)=="Name2"] <- "Name"

milbP2016f <- milbP[ milbP$Year =="2016", c("bref_id","Aff","Lev","Lg","Age",
                             "IP","ERA","WHIP","SO9","BB9","Pred_WAR_6yr")]

saveRDS(milbP2016f, "data/Pitchers/milbP2016f.rds")
saveRDS(milbShinyP, "ShinyApp/data/milbShinyP.rds")


