# SQL Query Data for MiLB Player Projections Hitters
# by Doug Duffy 2016

# Load packages
library(RMySQL)

# Load Fangraphs wOBA Coefficient Table
wOBACoeff <- read.csv("data/wOBACoeff.csv")

# In 1992 GDP start being recorded, so lets start with that year of data
minYear <- 1992
# Remove player seasons of less than 100 PA
minPA <- 100

# Setup SQL connection
conn <- dbConnect(MySQL(), user="root", dbname="BRef-Hitters2016", 
                  unix.sock="/Applications/XAMPP/xamppfiles/var/mysql/mysql.sock")
# Get milb data
SQL <- "SELECT * FROM Minors"
milb <- dbGetQuery(conn, SQL)
# Get mlb data
SQL <- "SELECT * FROM MLB"
mlb <- dbGetQuery(conn, SQL)
# Disconnect from SQL DB
dbDisconnect(conn=conn)

# Convert characters to numeric 
for(j in c(1:4, 9:31,34,37:51)){
  milb[ , j] <- as.numeric(milb[ , j])
  mlb[ , j] <- as.numeric(mlb[ , j])
  
}

# 251,284 Player Seasons
# 51,091 Players

# Remove player seasons from before 1992
milb <- milb[ milb$Year >= minYear, ]

# 111,429 Player Season
# 22,237 Players

# Remove player seasons of less than 100 PA
milb <- milb[  milb$PA >= minPA , ]

# 69,722 Player Season
# 18,511 Players

# This cleans up stray rows of MLB data, combinations of multiple teams/lgs, missing rows
milb <- milb[  milb$Lev != "MLB" &
                 !(grepl("Teams", milb$Tm)) &
                 !(grepl("Lgs", milb$Lg)) &
                 !is.na(milb$Lg),]

# Calculate some additional stats that may be useful
milb$BBPct <- with(milb, BB/PA*100) # Walk percentage
milb$SOPct <- with(milb, SO/PA*100) # Strikeout percentage
milb$ISO <- with(milb, SLG-BA) # Isolate power 
milb$BABIP <- with(milb, (H-HR)/(AB-SO-HR+SF)) # Batting average on balls in play
milb$BB.SORat <- with(milb, BB/SO) # Walk to strikeout ratio
milb$X1B <- with(milb, (H-HR-X3B-X2B)) # Explicitly calculate singles
milb$SBPct <- with(milb, (SB+CS)/(X1B + BB + HBP)) # Stolen base attempt percentage

# Calculate some more complicated stats
# wOBA (Weighted on Base Average) using wOBA coefficients from Fangraphs
# Bill James "Speed Score"
for(i in 1:nrow(milb)){
  # wOBA
  wOBA <- subset(wOBACoeff, Season==milb$Year[i])
  milb$wOBA[i] <-  ((wOBA$wBB*milb$BB[i])+
                      (wOBA$wHBP*milb$HBP[i])+
                      (wOBA$w1B*milb$X1B[i])+
                      (wOBA$w2B*milb$X2B[i])+
                      (wOBA$w3B*milb$X3B[i])+
                      (wOBA$wHR*milb$HR[i]))/(milb$AB[i]+
                                                milb$BB[i]-
                                                milb$IBB[i]+
                                                milb$SF[i]+
                                                milb$HBP[i])
  # Speed score
  factSB <-  ((milb$SB[i]+3)/(milb$SB[i]+milb$CS[i]+7)-0.4)*20
  factSBA <-  sqrt((milb$SB[i]+milb$CS[i])/(milb$X1B[i]+milb$BB[i]+milb$HBP[i]))/0.07
  fact3B <- (milb$X3B[i]/(milb$AB[i]-milb$HR[i]-milb$SO[i]))/0.0016
  factRS <- ((milb$R[i]-milb$HR[i])/(milb$H[i]+milb$BB[i]+milb$HBP[i]-milb$HR[i])-0.1)*25
  if(factSB < 0 | is.na(factSB)) factSB <- 0
  if(factSB >10 ) factSB <- 10
  if(factSBA < 0 | is.na(factSBA)) factSBA <- 0
  if(factSBA >10 ) factSBA <- 10
  if(fact3B < 0 | is.na(fact3B)) fact3B <- 0
  if(fact3B >10 ) fact3B <- 10
  if(factRS < 0 | is.na(factRS)) factRS <- 0
  if(factRS >10 ) factRS <- 10
  milb$Spd[i] <- mean(c(factSB,factSBA,fact3B,factRS))
  
  # Separate out advanced rookie ball
  if(milb$Lg[i] %in% c("APPY","PION")){
    milb$Lev[i] <- "Rk+"
  }
}
# Define unique players in MiLB data
playids <- unique(milb$bref_id)

# Set column we will add to NA
milb$bref_id_mlb <-  NA
milb$mlb_oWAR_28yo <- NA
milb$mlb_oWAR_6yr <- NA
# Sort MLB data in ascending Year order, so as to extract the data from first 6 years
mlb <- mlb[ order(mlb$Year, decreasing=F), ]

# For each unique MiLB player look up his MLB WAR first 6 yr, MLB WAR at 28 years old and MLB brefid
for(i in 1:length(playids)){
  if(playids[i] %in% mlb$bref_id){
    milb[milb$bref_id==playids[i],
         "bref_id_mlb"] <- subset(mlb, 
                                  bref_id==playids[i])[1, "bref_id_mlb"]
    milb[milb$bref_id==playids[i],
         "mlb_oWAR_28yo"] <- sum(subset(mlb, 
                                        bref_id==playids[i] & Age <=28)[,"oWAR"], na.rm=T)
    milb[milb$bref_id==playids[i],
         "mlb_oWAR_6yr"] <- sum(subset(mlb, 
                                  bref_id==playids[i])[1:6, "oWAR"],na.rm=T)
  }
}
# Convert remaining NA MLB data with 0's (this is all MiLB players who don't end up making MLB)
milb$mlb_oWAR_28yo[is.na(milb$mlb_oWAR_28yo)] <- 0
milb$mlb_oWAR_6yr[is.na(milb$mlb_oWAR_6yr)] <- 0

# Save MiLB and MLB datasets
saveRDS(milb,"data/Hitters/milb.rds")
saveRDS(mlb, "data/Hitters/mlb.rds")

