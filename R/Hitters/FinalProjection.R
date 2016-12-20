 # MiLB Player Projection 


library(corrplot)
library(e1071)
library(caret)


# Shift c, for log(x +c) transformation to normalize response
c <- 5

milb <- readRDS("data/Hitters/milb.rds")
mlb <- readRDS("data/Hitters/mlb.rds")

nametable <- readRDS("data/PlayerNames.rds")

diag.model <-readRDS("data/Models/Hitters/greedy_ensemble.rds")



# Keep players who are not yet 30 years of age (unsure of full potential)
# milb <- subset(milb, (Year-Age)>1986)


predcol <- c("Year","Age","AgeDif","G","PA","AB","R","H","X2B",
             "X3B","HR","RBI","SB","CS","BB","SO","BA","OBP","SLG","OPS","TB",
             "GDP","HBP","SH","SF","IBB","BBPct","SOPct","ISO","BABIP","BB.SORat",
             "X1B","SBPct","wOBA","Spd")
predcolFact <- c("Lg","Lev")

ident <- milb[ ,c("Aff","bref_id","PA","SLG","OPS","wOBA","BA","R","RBI")]

milb <- milb[ , c(predcol,predcolFact)]

# Ensure all factor variables are factors
for(i in predcolFact){
  milb[, i] <- as.factor(milb[ , i ])
}


diag.model.res <- as.data.frame( predict(diag.model,
                                         milb)) 
diag.model.res <- exp(diag.model.res)- c

names(diag.model.res)[1] <- "Pred_oWAR_6yr"

milb$Pred_oWAR_6yr <- round(diag.model.res$Pred_oWAR_6yr,1)
milb <- cbind(milb, ident)

milb <- milb[ order(milb$Pred_oWAR_6yr, decreasing = T),]

milbShiny <- milb[ , c("Pred_oWAR_6yr","Year","bref_id","Age","Aff","Lev","Lg","G","PA","BA","OBP",
                       "SLG","HR","SB","BBPct","SOPct","ISO","BABIP")]

milbShiny <- merge(milbShiny, nametable, by="bref_id", all.x=T)

milbShiny$Name2 <- paste('<a href="',milbShiny$urlMiLB,'" target="_blank" >', #class="btn btn-primary"
                        milbShiny$Name,'</a>', sep="")

names(milbShiny)[names(milbShiny)=="Pred_oWAR_6yr"] <- "Pred oWAR"
names(milbShiny)[names(milbShiny)=="BBPct"] <- "BB%"
names(milbShiny)[names(milbShiny)=="SOPct"] <- "K%"

milbShiny[ ,"BB%"] <- round(milbShiny[ ,"BB%"], 1)
milbShiny[ ,"K%"] <- round(milbShiny[ ,"K%"], 1)
milbShiny$BABIP <- round(milbShiny$BABIP, 3)
#milbShiny$wOBA <- round(milbShiny$wOBA, 3)

milbShiny <- milbShiny[ order(milbShiny[ , "Pred oWAR"], decreasing = T), 
                        c("Pred oWAR","Year","Name2","Age","Aff","Lev","Lg","G","PA","BA","OBP",
                       "SLG","HR","SB","BB%","K%","ISO","BABIP")]
names(milbShiny)[names(milbShiny)=="Name2"] <- "Name"


milb2016f <- milb[milb$Year =="2016" ,c("bref_id","Aff","Lev","Lg","Age","PA",
                      "BA","OBP","SLG","Pred_oWAR_6yr") ]

saveRDS(milbShiny, "ShinyApp/data/milbShiny.rds")
saveRDS(milb2016f, "data/Hitters/milb2016f.rds")


