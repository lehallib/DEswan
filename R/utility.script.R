# utility script for trajectory analysis as in Lehallier et al 2019, Nature medicine

rm(list=ls())

require("DEswan")
# devtools::install_github("lehallib/DEswan",build_vignettes = T)

data=DEswan::agingplasmaproteome[,-c(1:3)]
dataSupp=DEswan::agingplasmaproteome[,c(1:3)]

plx.tot=NULL
x=dataSupp$Age
i=1

plx.tot=NULL
for(i in 1:ncol(data)){
  y=as.vector(scale(data[,i]))
  xy=data.frame(na.omit(cbind(x=x,y=y)))
  xy=xy[order(xy$x),]
  plx<-predict(loess(xy$y~xy$x),newdata = min(x):max(x), se=T)
  plx.tot=rbind(plx.tot,plx$fit)
  print(i)
}

colnames(plx.tot)<-paste("X_",min(x):max(x),sep="")
rownames(plx.tot)<-colnames(data)
head(plx.tot)


require(gplots)
pairs.breaks <- seq(-1, 1, by=0.01)
mycol <- colorpanel(n=length(pairs.breaks)-1,low="deepskyblue",mid="black",high="yellow")
require(gplots)
toHeatmap=plx.tot


  # modify margins plot
  par(oma=c(1.1, # bottom
            2.1, # left
            2.1, # top
            5.1)) # right
  
  hm=(heatmap.2(as.matrix(toHeatmap),
                cexRow=.01,cexCol=1,
                trace="none",
                dendrogram="both",
                breaks=pairs.breaks, 
                col=mycol, 
                Rowv=T,key=F,
                Colv=F,
                lhei=c(0.2,10),
                lwid=c(.2,3)
  ))
  
  
  
  
  
  
  # make clustering and generate files for different cutoffs
  hc=hclust(dist(as.matrix(plx.tot)))
  

  hc.list=list()
  jjj=5
  for(jjj in c(2:20)){
    x.ct=mean(c(sort(hc$height,decreasing = T)[jjj-1],sort(hc$height,decreasing = T)[jjj]))
    ct=cutree(hc, h = x.ct)
    hc.list[[jjj]]<-ct
  }
  
