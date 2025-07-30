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
smpls<-c('A','B','C','D','E')

# データ読み込みと整形
fn<-file.choose()
dat<-qualtRics::read_survey(fn)

wddat<-data.frame()
for (i in seq(1,nrow(dat))){
  for (j in 1:nword){
    wddat<-rbind(wddat, c(dat$Q8[i], dat$Q3[i], dat$Q9[i], j, as.numeric(dat[i,20+j])))
  }
}
colnames(wddat)<-c('group','sample','eval', 'item', 'answer')
wddat$answer<-(nscl+1)/2-as.numeric(wddat$answer)
groups<-unique(wddat$group)
ngroup<-length(groups)
samples<-unique(wddat$sample)
nsample<-length(samples)
evals<-unique(wddat$eval)
neval<-length(evals)


mm<-data.frame(rbind(rep(3,nfactor),rep(-3,nfactor)))
colnames(mm)<-fctlb

fctdat<-data.frame()
for (i in 1:ngroup){
  fname<-paste("dx_panel", as.character(groups[i]), ".pdf", sep="")
  if (Sys.info()['sysname']=='Windows'){
    cairo_pdf(fname, family = "Yu Gothic")
  }else if(Sys.info()["sysname"] == "Linux") {
    cairo_pdf(fname, family = "Noto Sans CJK JP")
  }else{
    quartz(type="pdf", file=fname, width=8, height=10)
  }
  smpgrp<-unique(wddat[wddat$group==groups[i],]$sample)
  nsmpgrp<-length(smpgrp)
  for (j in 1:nsmpgrp){
    evalsmp<-unique(wddat[wddat$group==groups[i] & wddat$sample==smpgrp[j],]$eval)
    neval<-length(evalsmp)
    cdata<-data.frame(matrix(NA,neval+2,nfactor))
    cdata[1:2,]<-mm
    for (k in 1:neval){
      thisdat<-array(NA,nfactor)
      for (l in 1:nfactor){
        thisdat<-sum(wddat[which(wddat$group==groups[i] & wddat$sample==smpgrp[j] & wddat$eval==evalsmp[k]),5] * fctr[l,],na.rm=T)/sum(!is.na(fctr[l,]))  
        fctdat<-rbind(fctdat,c(groups[i],smpgrp[j],evalsmp[k],l,thisdat))  
      }
      cdata[2+k,]<-thisdat
    }
    colnames(fctdat)<-c('group','sample','eval','factor','value')
    colnames(cdata)<-fctlb
    print(cdata)
    radarchart(cdata,
               cglty = 1,       # Grid line type
               cglcol = "gray", # Grid line color
               cglwd = 1,       # Line width of the grid
               pcol=2:5,
               plwd = 3,        # Width of the line
               plty = 1,        # Line type of the line 
               axistype = 0,
               seg=7, 
               vlcex=0.8, 
               centerzero=TRUE, 
               vlabels = colnames(cdata))
    legend("bottomleft",
           legend = evalsmp,
           bty = "n", pch = 20, col = 2:5,
           text.col = "grey25", cex = 0.7)
    g<-list()
    for (k in 1:neval){
      thisdata<-fctdat[which(fctdat$group==groups[i] & fctdat$sample==smpgrp[j]),]
      thisdata$value<-as.numeric(thisdata$value)
      thisdata$factor<-fctlb
      g[[i]]<-ggplot(data=thisdata, aes(x=as.factor(factor), y=value))+geom_jitter(height = 0, width = 0.1)+ylim(-3,3)
    }
    wrap_plots(g)+plot_layout(ncol=1)
  }
  dev.off()
}



# 普通のグラフの表示
