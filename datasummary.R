library(qualtRics)
library(ggplot2)
library(fmsb)

par(family = "Yu Gothic")

ncat<-7
nflv<-7
flavorlab<-c('くん煙臭','焙乾香','甘い','肉質','油臭','酸臭','その他')
tastelab<-c('油っぽさ','コクのある','収斂味・苦味','塩味','甘味','旨味','酸味')


fn<-choose.files()
dat<-qualtRics::read_survey(fn)

samples<-unique(dat$Q2)
samples<-samples[!is.na(samples)]
nsample<-length(samples)

panels<-unique(dat$Q3)
panels<-panels[!is.na(panels)]
npanel<-length(panels)

catdat<-data.frame()
for (i in seq(1,nrow(dat))){
  catdat<-rbind(catdat,c(dat$Q3[i],dat$Q2[i],'cat1', mean(c(dat$Q1_1[i],dat$Q1_2[i],dat$Q1_3[i],dat$Q1_4[i]))))
  catdat<-rbind(catdat,c(dat$Q3[i],dat$Q2[i],'cat2', mean(c(dat$Q1_5[i],dat$Q1_6[i],dat$Q1_7[i],dat$Q1_8[i]))))
  catdat<-rbind(catdat,c(dat$Q3[i],dat$Q2[i],'cat3', mean(c(dat$Q1_8[i],dat$Q1_9[i],dat$Q1_10[i]))))
  catdat<-rbind(catdat,c(dat$Q3[i],dat$Q2[i],'cat4', mean(c(dat$Q1_11[i],dat$Q1_12[i],dat$Q1_13[i],dat$Q1_14[i],dat$Q1_15[i],dat$Q1_16[i],dat$Q1_17[i],dat$Q1_18[i]))))
  catdat<-rbind(catdat,c(dat$Q3[i],dat$Q2[i],'cat5', mean(c(dat$Q1_19[i]))))
  catdat<-rbind(catdat,c(dat$Q3[i],dat$Q2[i],'cat6', mean(c(dat$Q1_20[i],dat$Q1_21[i]))))
  catdat<-rbind(catdat,c(dat$Q3[i],dat$Q2[i],'cat7', mean(c(dat$Q1_22[i],dat$Q1_23[i],dat$Q1_24[i]))))
  catdat<-rbind(catdat,c(dat$Q3[i],dat$Q2[i],'cat8', mean(c(dat$Q1_25[i],dat$Q1_26[i],dat$Q1_27[i],dat$Q1_28[i],dat$Q1_29[i],dat$Q1_30[i],dat$Q1_31[i]))))
  catdat<-rbind(catdat,c(dat$Q3[i],dat$Q2[i],'cat9', mean(c(dat$Q1_32[i],dat$Q1_33[i]))))
  catdat<-rbind(catdat,c(dat$Q3[i],dat$Q2[i],'cat10', dat$Q1_34[i]))
  catdat<-rbind(catdat,c(dat$Q3[i],dat$Q2[i],'cat11', dat$Q1_35[i]))
  catdat<-rbind(catdat,c(dat$Q3[i],dat$Q2[i],'cat12', dat$Q1_36[i]))
  catdat<-rbind(catdat,c(dat$Q3[i],dat$Q2[i],'cat13', dat$Q1_37[i]))
  catdat<-rbind(catdat,c(dat$Q3[i],dat$Q2[i],'cat14', dat$Q1_38[i]))
  catdat<-rbind(catdat,c(dat$Q3[i],dat$Q2[i],'cat15', dat$Q1_39[i]))
}
colnames(catdat)<-c('panel','sample','category','value')

catdat<-catdat[!is.na(catdat$panel),]
catdat<-catdat[!is.na(catdat$sample),]
catdat$value<-as.numeric(catdat$value)-6

samplemean<-matrix(NA,nrow=nsample,ncol=15)
for (smp in samples){
  for (cat in 1:15){
    data<-catdat[catdat$category==paste('cat',cat,sep='') & catdat$sample==smp,]$value
    print(data)
    samplemean[smp,cat]<-mean(data, na.rm = T)
  }
}

mm<-data.frame(
  a=c(6,1),
  b=c(6,1),
  c=c(6,1),
  d=c(6,1),
  e=c(6,1),
  f=c(6,1),
  g=c(6,1)
)

mmflavor<-mm
mmtaste<-mm


meandat<-data.frame(samplemean)
flavor<-meandat[,1:7]
taste<-meandat[,9:15]
colnames(flavor)<-flavorlab
colnames(mmflavor)<-flavorlab

colnames(taste)<-tastelab
colnames(mmtaste)<-tastelab

flavor<-rbind(mmflavor,flavor)
taste<-rbind(mmtaste, taste)



#inddat<-matrix(NA,npanel,15,nsample)
inddat<-array(NA,c(npanel,15,nsample))
cflavor<-data.frame(matrix(NA,4,7))
cflavor[1:2,]<-mmflavor
colnames(cflavor)<-flavorlab
ctaste<-data.frame(matrix(NA,4,7))
ctaste[1:2,]<-mmtaste
colnames(ctaste)<-tastelab


for (pnl in panels){
  #pdf(cat('panel',as.character(pnl),'.pdf',sep=''))
  #pdf("temp3.pdf")
  fname<-paste("panel", as.character(pnl), ".pdf", sep="")
  cairo_pdf(fname, family = "Yu Gothic")
  for (smp in samples){
    for (cat in 1:15){
      if (max(catdat$panel==pnl & catdat$sample==smp & catdat$category==paste('cat',cat,sep='')))
        inddat[pnl,cat,smp]<-catdat[catdat$panel==pnl & catdat$sample==smp & catdat$category==paste('cat',cat,sep=''),]$value
    }
    cflavor[3,]<-meandat[smp,1:7]
    cflavor[4,]<-inddat[pnl,1:7,smp]
    ctaste[3,]<-meandat[smp,9:15]
    ctaste[4,]<-inddat[pnl,9:15,smp]
    # 風味
    par(mfrow = c(1, 2))
    radarchart(cflavor,
               cglty = 1,       # Grid line type
               cglcol = "gray", # Grid line color
               cglwd = 1,       # Line width of the grid
               pcol=2:3,
               plwd = 3,        # Width of the line
               plty = 1,        # Line type of the line 
               axistype = 0,
               seg=6, 
               vlcex=0.8, 
               centerzero=TRUE, 
               vlabels = colnames(flavor))
    legend("bottomleft",
           legend = c('平均','あなた'),
           bty = "n", pch = 20, col = 2:3,
           text.col = "grey25", cex = 0.7)
    r<-round(cor(t(cflavor[3,]),t(cflavor[4,])),2)
    title(paste("sample", as.character(smp), ", flavor:", as.character(r)))
    # 味
    radarchart(ctaste,
               cglty = 1,       # Grid line type
               cglcol = "gray", # Grid line color
               cglwd = 1,       # Line width of the grid
               pcol=2:3,
               plwd = 3,        # Width of the line
               plty = 1,        # Line type of the line 
               axistype = 0, 
               seg=6, 
               vlcex=0.8, 
               centerzero=TRUE, 
               vlabels = colnames(taste))
    
    legend("bottomleft",
           legend = c('平均','あなた'),
           bty = "n", pch = 20, col = 2:3,
           text.col = "grey25", cex=0.7)
    r<-round(cor(t(ctaste[3,]),t(ctaste[4,])),2)
    title(paste("sample", as.character(smp), ", taste:", as.character(r)))
    
  }
  dev.off()
    
}

par(mfrow = c(1, 2))

# 風味
radarchart(flavor,
           cglty = 1,       # Grid line type
           cglcol = "gray", # Grid line color
           cglwd = 1,       # Line width of the grid
           pcol=1:nsample,
           plwd = 3,        # Width of the line
           plty = 1,        # Line type of the line 
           axistype = 0, 
           seg=6, 
           vlcex=0.8, 
           centerzero=TRUE, 
           vlabels = colnames(flavor))
legend("bottomleft",
       legend = paste("S", 1:nsample),
       bty = "n", pch = 20, col = 1:nsample,
       text.col = "grey25", cex = 0.7)

# 味
radarchart(taste,
           cglty = 1,       # Grid line type
           cglcol = "gray", # Grid line color
           cglwd = 1,       # Line width of the grid
           pcol=1:nsample,
           plwd = 3,        # Width of the line
           plty = 1,        # Line type of the line 
           axistype = 0, 
           seg=6, 
           vlcex=0.8, 
           centerzero=TRUE, 
           vlabels = colnames(taste))

legend("bottomleft",
       legend = paste("S", 1:nsample),
       bty = "n", pch = 20, col = 1:nsample,
       text.col = "grey25", cex=0.7)


