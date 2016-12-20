# Scrape information from baseball reference and save to file.
#
# Retrieves : Name, link

# Load Packages
library(XML)
library(rvest)
library(stringr)


# Load player lists
bat <- readRDS("ShinyApp/data/milbShiny.rds")
pitch <- readRDS("ShinyApp/data/milbShinyP.rds")

playerIDs <- unique(c(bat$bref_id, pitch$bref_id))

# Progress bar
pb <- txtProgressBar(min = 1, max = length(playerIDs), style = 3)


for(i in 37398:length(playerIDs)){# :length(playerIDs)
  # Set progress bar
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i)
  
  # Define URL
  urlmilb <- paste("http://www.baseball-reference.com/register/player.cgi?id=",playerIDs[i], sep="")
  
  # Name Info
  htmlpage <- read_html(urlmilb)
  namehtml <- html_nodes(htmlpage, "#player_name")
  name <- html_text(namehtml)
  
  
  # Compile output table for all players
  if(i == 25898 ){
    nametable <- cbind(playerIDs[i], name, urlmilb)
  }else{
    nametable <- rbind(nametable, cbind(playerIDs[i], name, urlmilb))
    nametable <- as.data.frame(nametable, stringsAsFactors=F)
  }
  
}
# Close progress bar
close(pb)


if(i== length(playerIDs)){
  names(nametable ) <- c("bref_id","Name","urlMiLB")
  
  # Save to file
  saveRDS(nametable, "data/PlayerNames.rds")
  
  
}




