library(qualtRics)
library(ggplot2)
library(fmsb)

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

samplemean<-matrix(NA,nrow=nsample,ncol=15)
for (smp in samples){
  for (cat in 1:15){
    data<-catdat[catdat$category==paste('cat',cat,sep='') & catdat$sample==smp,]$value
    print(data)
    samplemean[smp,cat]<-mean(as.numeric(data))
  }
}

mm<-data.frame(
  a=c(12,1),
  b=c(12,1),
  c=c(12,1),
  d=c(12,1),
  e=c(12,1),
  f=c(12,1),
  g=c(12,1)
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

# 風味
radarchart(flavor, axistype = 2, seg=6, plty=1, vlcex=1, centerzero=TRUE, vlabels = colnames(flavor))

# 味
radarchart(taste, axistype = 2, seg=6, plty=1, vlcex=1, centerzero=TRUE, vlabels = colnames(taste))
