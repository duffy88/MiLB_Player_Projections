# MiLB Projection Team Comparison
require(plyr)
require(reshape2)
require(plotrix)

milb2016f <- readRDS("data/Hitters/milb2016f.rds")


comp <- ddply(milb2016f,
              ~Aff + Lev, summarise, Pred_oWAR_6yr=sum(Pred_oWAR_6yr))


comp.w <- dcast(comp[1:3],Aff ~ Lev)
comp.w[is.na(comp.w)] <- 0


pos <- c("Overall","AAA","AA","A+","A","A-","Rk+","Rk","FRk")
comp.w <- comp.w[, c("Aff","AAA","AA","A+","A","A-","Rk+","Rk","FRk")]
comp.w$Overall <- rowSums(comp.w[,2:9], na.rm=T)

comp.w <- comp.w[, c("Aff","Overall","AAA","AA","A+","A","A-","Rk+","Rk","FRk")]


target <- c("BAL","BOS","NYY","TBR","TOR",
            "CLE","CHW","DET","KCR","MIN",
            "HOU","LAA","OAK","SEA","TEX",
            "ATL","MIA","NYM","PHI","WSN",
            "CHC","CIN","MIL","PIT","STL",
            "ARI","COL","LAD","SDP","SFG")

comp.w <- comp.w[ match( target, comp.w$Aff), ]


for(i in pos){
  
  
  temp <- comp.w[ , i]
  temp <- matrix(temp, nrow=nrow(comp.w), ncol=1, byrow=T)
  tempcolors<-matrix(NA,nrow=nrow(comp.w),ncol=1)
  tempcolors[temp > median(temp)]<-
    color.scale(temp[temp>median(temp)],
                cs1=c(1,0),cs2=c(1,1),cs3=c(1,0))
  tempcolors[temp < median(temp)]<-
    color.scale(temp[temp<median(temp)],
                cs1=c(1,1),cs2=c(0,1),cs3=c(0,1))
  
  tempcolors[temp ==max(temp)] <- color.scale(temp[temp ==max(temp)],cs1=0,cs2=1,cs3=0)
  
  tempcolors[temp == min(temp)] <- color.scale(temp[temp ==min(temp)],cs1=1,cs2=0,cs3=0)
  if(i=="Overall"){
    plot <- temp
    colors <- tempcolors
  }else {
    plot <-cbind(plot, temp)
    colors <- cbind(colors, tempcolors)
  }
}

for(i in 1:ncol(plot)){
  plot[ ,i] <- round(plot[,i], 1)
}

jpeg("Plots/TeamComparison.jpeg", width = 1000, height = 500)


par(mar=c(1.1,4.1,5.5,4.1))
color2D.matplot(plot,cellcolors=colors,
                show.values=2,axes=F,xlab="",ylab="MLB Team")
axis(1,at=0.5:(length(pos)-0.5),las=2,labels=pos,side=3)
axis(2,at=(nrow(comp.w)-0.5):0.5,las=2,labels=comp.w[,1])
rect(0,0,1,nrow(comp.w), lwd=7)
rect(0,0,ncol(comp.w),nrow(comp.w), lwd=7)
rect(0,15,ncol(comp.w),nrow(comp.w), lwd=7)
rect(0,5,ncol(comp.w),nrow(comp.w), lwd=4)
rect(0,10,ncol(comp.w),nrow(comp.w), lwd=4)
rect(0,15,ncol(comp.w),nrow(comp.w), lwd=4)
rect(0,20,ncol(comp.w),nrow(comp.w), lwd=4)
rect(0,25,ncol(comp.w),nrow(comp.w), lwd=4)
rect(0,30,ncol(comp.w),nrow(comp.w), lwd=4)
mtext("Minor League Level", side = 3, line = 2.5, at = 4.5)
mtext("Predicted oWAR 6yr", side = 3, line = 3.5, at = 4.5, cex= 1.5)
mtext("AL\nEast", side = 4, padj = 1, at =27.5)
mtext("AL\nCentral", side = 4, padj = 1, at =22.5)
mtext("AL\nWest", side = 4, padj = 1, at =17.5)
mtext("NL\nEast", side = 4, padj = 1, at =12.5)
mtext("NL\nCentral", side = 4, padj = 1, at =7.5)
mtext("NL\nWest", side = 4, padj = 1, at =2.5)

dev.off()


