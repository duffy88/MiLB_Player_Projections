# SQL Query Data for MiLB Player Projections Hitters
# by Doug Duffy 2016

# Load packages
library(RMySQL)

# Load Fangraphs wOBA Coefficient table
wOBACoeff <- read.csv("data/wOBACoeff.csv")

# In 1992 GDP start being recorded
minYear <- 1992
# Remove pitcher seasons of less than 100 Batters faced
minBF <- 100

# Setup SQL connection
conn <- dbConnect(MySQL(), user="root", dbname="BRef-Pitchers2016", 
                  unix.sock="/Applications/XAMPP/xamppfiles/var/mysql/mysql.sock")

# Get MiLB data
SQL <- "SELECT * FROM Minors"
milbP <- dbGetQuery(conn, SQL)

# Get MLB data
SQL <- "SELECT * FROM MLB"
mlbP <- dbGetQuery(conn, SQL)
# Disconnect from SQL DB
dbDisconnect(conn=conn)

# Convert characters to numeric 
for(j in c(1:4, 9:37,40,41,43:56)){
  milbP[ , j] <- as.numeric(milbP[ , j])
  mlbP[ , j] <- as.numeric(mlbP[ , j])
  
}

# Remove player seasons from before 1992 and less than 100 BF
milbP <- milbP[ milbP$Year >= minYear & 
                milbP$BF >= minBF , ]

# Cleans up stray rows of MLB data, combinations of multiple teams/lgs, missing rows
milbP <- milbP[  milbP$Lev != "MLB" &
                 !(grepl("Teams", milbP$Tm)) &
                 !(grepl("Lgs", milbP$Lg)) &
                 !is.na(milbP$Lg),]

# Separate out Advanced Rookie ball
for(i in 1:nrow(milbP)){
  if(milbP$Lg[i] %in% c("APPY","PION")){
    milbP$Lev[i] <- "Rk+"
  }
}


# Define unique players in MiLB data
playids <- unique(milbP$bref_id)

# Set column we will add to NA
milbP$bref_id_mlb <-  NA
milbP$mlb_WAR_28yo <- NA
milbP$mlb_WAR_6yr <- NA
# Sort MLB data in ascending Year order, so as to extract the data from first 6 years
mlbP <- mlbP[ order(mlbP$Year, decreasing=F), ]

# For each unique MiLB player look up his MLB WAR first 6 yr, MLB WAR at 28 years old and MLB brefid
for(i in 1:length(playids)){
  if(playids[i] %in% mlbP$bref_id){
    milbP[milbP$bref_id==playids[i],
         "bref_id_mlb"] <- subset(mlbP, 
                                  bref_id==playids[i])[1, "bref_id_mlb"]
    milbP[milbP$bref_id==playids[i],
         "mlb_WAR_28yo"] <- sum(subset(mlbP, 
                                        bref_id==playids[i] & Age <=28)[,"WAR"], na.rm=T)
    milbP[milbP$bref_id==playids[i],
         "mlb_WAR_6yr"] <- sum(subset(mlbP, 
                                       bref_id==playids[i])[1:6, "WAR"],na.rm=T)
  }
}
# Convert remaining NA MLB data with 0's (this is all MiLB players who don't end up making MLB)
milbP$mlb_WAR_28yo[is.na(milbP$mlb_WAR_28yo)] <- 0
milbP$mlb_WAR_6yr[is.na(milbP$mlb_WAR_6yr)] <- 0

# Save MiLB and MLB datasets
saveRDS(milbP,"data/Pitchers/milbP.rds")
saveRDS(mlbP, "data/Pitchers/mlbP.rds")

