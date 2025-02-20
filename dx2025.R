library(qualtRics)
library(ggplot2)
library(patchwork)
library(tidyr)
library(fmsb)

# 表示・保存のためのフォント設定
if (Sys.info()['sysname']=='Windows'){
  par(family = "Yu Gothic")
  
  windowsFonts(
    `Yu Mincho` = windowsFont("Yu Mincho"),
    `Yu Gothic` = windowsFont("Yu Gothic")
  )
}else{
  par(family="Hiragino Sans W3")
  quartzFonts(
    `Hiragino Sans` = quartzFont(rep("HiraginoSans-W3", 4)),
    `Hiragino Mincho ProN` = quartzFont(rep("HiraMinProN-W3", 4))
  )
}

# 評価語設定n
nword<-17
wdlb<-c('硬さ','口どけ','なめらか','油まとわり','飲後さらっ','触感','コク','濃厚','後味','異味','まろやか','甘味','甘い香','ロウ','酸味','香ばしい','総合')

# データ読み込みと整形
fn<-file.choose()
dat<-qualtRics::read_survey(fn)

wddat<-data.frame()
for (i in seq(1,nrow(dat))){
  for (j in 1:nword){
  wddat<-rbind(wddat, c(dat$Q2[i], dat$Q3[i], j, as.numeric(dat[i,19+j])))
  }
}


colnames(wddat)<-c('panel','sample','word', 'value')
wddat$value<-as.numeric(wddat$value)
wddat$wdtext<-wdlb[as.numeric(wddat$word)]

# 不完全データ除外
wddat<-drop_na(wddat)

# パネル数、サンプル数抽出

samples<-unique(wddat$sample)
samples<-samples[!is.na(samples)]
nsample<-length(samples)

panels<-unique(wddat$panel)
panels<-panels[!is.na(panels)]
npanel<-length(panels)

# パネル間平均の算出
samplemean<-matrix(NA,nrow=nsample,ncol=nword)
for (smp in 1:nsample){
  for (wd in 1:nword){
    data<-wddat[wddat$word==wd & wddat$sample==samples[smp],]$value
    print(data)
    samplemean[smp,wd]<-mean(data, na.rm = T)
  }
}

# レーダーチャートのための準備
mm<-data.frame(rbind(rep(7,nword),rep(0,nword)))
meandat<-data.frame(samplemean)
colnames(meandat)<-wdlb
colnames(mm)<-wdlb
mmdat<-rbind(mm,meandat)
inddat<-array(NA,c(npanel,nword,nsample))
cdata<-data.frame(matrix(NA,4,nword))
cdata[1:2,]<-mm
colnames(cdata)<-wdlb

# 個人別レーダーチャートのPDF作成
for (pnl in 1:npanel){
  fname<-paste("dx_panel", as.character(panels[pnl]), ".pdf", sep="")
  if (Sys.info()['sysname']=='Windows'){
    cairo_pdf(fname, family = "Yu Gothic")
  }else{
    quartz(type="pdf", file=fname, width=8, height=10)
  }
  for (smp in 1:nsample){
    for (wd in 1:nword){
      inddat[pnl,wd,smp]<-wddat[wddat$panel==panels[pnl] & wddat$sample==samples[smp] & wddat$word==wd,]$value
    }
    cdata[3,]<-meandat[smp,]
    cdata[4,]<-inddat[pnl,,smp]
    # 風味
    radarchart(cdata,
               cglty = 1,       # Grid line type
               cglcol = "gray", # Grid line color
               cglwd = 1,       # Line width of the grid
               pcol=2:3,
               plwd = 3,        # Width of the line
               plty = 1,        # Line type of the line 
               axistype = 0,
               seg=7, 
               vlcex=0.8, 
               centerzero=TRUE, 
               vlabels = colnames(cdata))
    legend("bottomleft",
           legend = c('平均','あなた'),
           bty = "n", pch = 20, col = 2:3,
           text.col = "grey25", cex = 0.7)
    r<-round(cor(t(cdata[3,]),t(cdata[4,])),2)
    title(paste("sample", as.character(samples[smp])))
    
  }
  dev.off()
    
}

# サンプル間の違いのレーダーチャート表示
mmmean<-rbind(mm,meandat)
radarchart(mmmean,
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
           vlabels = colnames(meandat))
legend("bottomleft",
       legend = samples,
       bty = "n", pch = 20, col = 1:nsample,
       text.col = "grey25", cex = 0.7)
#dev.off()

# 普通のグラフの表示
g<-list()
for (i in 1:nsample){
  thisdata<-wddat[which(wddat$sample==samples[i]),]
  g[[i]]<-ggplot(data=thisdata, aes(x=wdtext, y=value))+geom_violin()
}
wrap_plots(g)+plot_layout(ncol=1)
