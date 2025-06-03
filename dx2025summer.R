library(qualtRics)
library(ggplot2)
library(patchwork)
library(tidyr)
library(fmsb)

# 表示・保存のためのフォント設定
# if (Sys.info()['sysname']=='Windows'){
#   par(family = "Yu Gothic")
#   
#   windowsFonts(
#     `Yu Mincho` = windowsFont("Yu Mincho"),
#     `Yu Gothic` = windowsFont("Yu Gothic")
#   )
# }else{
#   par(family="Hiragino Sans")
#   quartzFonts(
#     `Hiragino Sans` = quartzFont(rep("HiraginoSans-W3", 4)),
#     `Hiragino Mincho ProN` = quartzFont(rep("HiraMinProN-W3", 4))
#   )
#   theme_update(text = element_text(family = "Hiragino Sans")) # 追加
# }

# https://ill-identified.hatenablog.com/entry/2020/10/03/200618 フォント設定
if(Sys.info()["sysname"] == "Windows"){
  if(as.integer(str_extract(Sys.info()["release"], "^[0-9]+")) >=8){
    family_sans <- "Yu Gothic"
    family_serif <- "Yu Mincho"
  } else {
    family_sans <- "MS Gothic"
    family_serif <- "MS Mincho"
  }
} else if(Sys.info()["sysname"] == "Linux") {
  family_sans <- "Noto Sans CJK JP"
  family_serif <- "Noto Serif CJK JP"
} else if(Sys.info()["sysname"] == "Darwin"){
  family_serif <- "Hiragino Mincho ProN"
  family_sans <- "Hiragino Sans"
} else {
  # インストールすればとりあえず動く
  family_sans <- "Noto Sans CJK JP"
  family_serif <- "Noto Serif CJK JP"
}

# 評価語設定n
nword<-18
wdlb<-c('規則性','健康','安定','まとまり','激しさ','派手','活発','賑やか','動的','陽気','繊細','柔らかさ','緩んだ','弱い','ぼんやり','なめらか','鈍い','重い')
nfactor<-3
nscl<-7
fctlb<-c('均整性','活動性','力量性')
fct1<-c(1,1,1,1,-1, rep(NA,13))
fct2<-c(rep(NA,5),1,1,1,1,1,1,rep(NA,7))
fct3<-c(rep(NA,11),1,1,1,-1,1,1,-1)
fctr<-data.frame(rbind(fct1,fct2,fct3))

# データ読み込みと整形
fn<-file.choose()
dat<-qualtRics::read_survey(fn)

wddat<-data.frame()
for (i in seq(1,nrow(dat))){
  for (j in 1:nword){
    wddat<-rbind(wddat, c(dat$Q2[i], dat$Q3[i], j, as.numeric(dat[i,19+j])))
  }
}
colnames(wddat)<-c('panel','sample','item','answer')
wddat$answer<-(nscl+1)/2-as.numeric(wddat$answer)
panels<-unique(wddat$panel)
npanel<-length(panels)
samples<-unique(wddat$sample)
nsample<-length(samples)
fctdat<-data.frame()
for (i in 1:npanel){
  for (j in 1:nsample){
    thisdat<-array(NA,nfactor)
    for (k in 1:nfactor){
      thisdat<-sum(wddat[which(wddat$panel==panels[i] & wddat$sample==samples[j]),4] * fctr[k,],na.rm=T)/sum(!is.na(fctr[k,]))
      fctdat<-rbind(fctdat,c(i,j,k,thisdat))  
    }
    
  }
}
colnames(fctdat)<-c('panel','sample','factor','value')


#wddat$value<-as.numeric(wddat$value)
#wddat$wdtext<-wdlb[as.numeric(wddat$word)]

# 不完全データ除外
wddat<-drop_na(wddat)

# パネル数、サンプル数抽出

# samples<-unique(wddat$sample)
# samples<-samples[!is.na(samples)]
# nsample<-length(samples)
# 
# panels<-unique(wddat$panel)
# panels<-panels[!is.na(panels)]
# npanel<-length(panels)

# パネル間平均の算出
samplemean<-matrix(NA,nrow=nsample,ncol=nfactor)
for (smp in 1:nsample){
  for (fct in 1:nfactor){
    data<-fctdat[fctdat$factor==fct & fctdat$sample==smp,]$value
    print(data)
    samplemean[smp,fct]<-mean(data, na.rm = T)
  }
}

# レーダーチャートのための準備
mm<-data.frame(rbind(rep(3,nfactor),rep(-3,nfactor)))
meandat<-data.frame(samplemean)
colnames(meandat)<-fctlb
colnames(mm)<-fctlb
mmdat<-rbind(mm,meandat)
inddat<-array(NA,c(npanel,nfactor,nsample))
cdata<-data.frame(matrix(NA,4,nfactor))
cdata[1:2,]<-mm
colnames(cdata)<-fctlb

# 個人別レーダーチャートのPDF作成
for (pnl in 1:npanel){
  fname<-paste("dx_panel", as.character(panels[pnl]), ".pdf", sep="")
  if (Sys.info()['sysname']=='Windows'){
    cairo_pdf(fname, family = "Yu Gothic")
  }else if(Sys.info()["sysname"] == "Linux") {
    cairo_pdf(fname, family = "Noto Sans CJK JP")
  }else{
    quartz(type="pdf", file=fname, width=8, height=10)
  }
  
  for (smp in 1:nsample){
    
    for (fct in 1:nfactor){
      if (max(fctdat$panel==panels[pnl] & fctdat$sample==smp & fctdat$factor==fct))
        inddat[pnl,fct,smp]<-fctdat[fctdat$panel==panels[pnl] & fctdat$sample==smp & fctdat$factor==fct,]$value
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
           cglwd = 2,       # Line width of the grid
           pcol=1:nsample,
           plwd = 4,        # Width of the line
           plty = 1,        # Line type of the line 
           axistype = 0, 
           seg=6, 
           vlcex=1.2, 
           centerzero=TRUE, 
           vlabels = colnames(meandat))

legend("topright",
       legend = samples,
       bty = "n", pch = 20, col = 1:nsample,
       text.col = "grey25", cex = 0.7)
#dev.off()

# 普通のグラフの表示
g<-list()
for (i in 1:nsample){
  thisdata<-fctdat[which(fctdat$sample==i),]
  g[[i]]<-ggplot(data=thisdata, aes(x=as.factor(factor), y=value))+geom_jitter(height = 0, width = 0.1,size=3.0) +
    theme_bw(base_size = 16) +  # 基本フォントサイズを16に
    theme(
      text = element_text(family = "Hiragino Sans"),  # フォント設定
      axis.text = element_text(size = 18, face = "bold"),  # 軸ラベルを大きく太字
      axis.title = element_text(size = 20, face = "bold"),  # 軸タイトルを大きく太字
      axis.line = element_line(size = 1.5),  # 軸線を太く
      axis.ticks = element_line(size = 1.5)  # 軸目盛りを太く
    )+
    scale_x_discrete(labels=fctlb)+xlab('因子')
}
wrap_plots(g)+plot_layout(ncol=1)
