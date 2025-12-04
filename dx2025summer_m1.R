library(qualtRics)
library(ggplot2)
library(patchwork)
library(tidyr)
library(fmsb)

rm(list=ls())

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
fctlb<-c('1. 均整ーランダム','2. 活動ー静穏','3. 柔和ー重厚')
fct1<-c(1,1,1,1,-1, rep(NA,13))
fct2<-c(rep(NA,5),1,1,1,1,1,1,rep(NA,7))
fct3<-c(rep(NA,11),1,1,1,-1,1,1,-1)
fctr<-data.frame(rbind(fct1,fct2,fct3))
smpls<-c('A','B','C','D','E','F','G','H','I','J')

# データ読み込みと整形
fn<-file.choose()
dat<-qualtRics::read_survey(fn)

wddat<-data.frame()
for (i in seq(1,nrow(dat))){
  for (j in 1:nword){
    wddat<-rbind(wddat, c(i, dat$Q8[i], dat$Q3[i], dat$Q9[i], j, as.numeric(dat[i,20+j])))
  }
}
colnames(wddat)<-c('panel','group','sample','eval', 'item', 'answer')
wddat$answer<-(nscl+1)/2-as.numeric(wddat$answer)

# パネル別データ集計
pnldat<-data.frame()
for (i in 1:max(wddat$panel)){
  tpdat<-wddat[wddat$panel==i,]
  attach(tpdat)
  for (j in 1:nfactor){
    fctd<-sum(answer*fctr[j,],na.rm = T)/sum(abs(fctr[j,]),na.rm = T)
    pnldat<-rbind(pnldat,c(panel[1],group[1],sample[1],eval[1],j,fctd))
  }
  detach(tpdat)
}
colnames(pnldat)<-c('panel','group','sample','eval','fact','value')

# パネル間平均集計
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
    par(family = "Hiragino Sans")
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
        alldat<-wddat[which(wddat$group==groups[i] & wddat$sample==smpgrp[j] & wddat$eval==evalsmp[k]),6]
        thiswgt<-as.numeric(rep(fctr[l,], length(alldat)/nword))
        thisdat[l]<-sum(alldat * thiswgt, na.rm=T)/sum(abs(thiswgt),na.rm = T)
        #thisdat<-sum(wddat[which(wddat$group==groups[i] & wddat$sample==smpgrp[j] & wddat$eval==evalsmp[k]),5] * fctr[l,],na.rm=T)/sum(!is.na(fctr[l,]))  
        fctdat<-rbind(fctdat,c(groups[i],smpgrp[j],evalsmp[k],l,thisdat[l]))  
      }
      cdata[2+k,]<-thisdat
    }
    colnames(fctdat)<-c('group','sample','eval','fct','value')
    colnames(cdata)<-fctlb
    ttl<-paste0('Group',as.character(i),'-Sample ',smpls[smpgrp[j]])
    #print(cdata)
    radarchart(cdata,
               cglty = 1,       # Grid line type
               cglcol = "gray", # Grid line color
               cglwd = 1,       # Line width of the grid
               pcol=2:5,
               plwd = 3,        # Width of the line
               plty = 1,        # Line type of the line 
               axistype = 0,
               seg=7, 
               vlcex=1.2, 
               centerzero=TRUE, 
               vlabels = colnames(cdata))
    legend("bottomleft",
           legend = paste0('#',evalsmp),
           bty = "n", pch = 20, col = 2:5,
           text.col = "grey25", cex = 1.2)
    title(ttl)
    # rm(thisdata)
    for (k in 1:neval){
      # 平均値プロット
      # thisdata<-fctdat[which(fctdat$group==groups[i] & fctdat$sample==smpgrp[j] & fctdat$eval==evalsmp[k]),]
      # print(thisdata)
      # パネル別プロット
      thispdata<-pnldat[pnldat$group==groups[i] & pnldat$sample==smpgrp[j] & pnldat$eval==evalsmp[k],]
      pnldat$fact<-fctlb
      
      gp<-ggplot(data=thispdata, aes(x=fact,y=value))+geom_boxplot()
      gp<-gp+geom_jitter(width=0.1, height=0)#+ylim(-3,3)
      gp<-gp+stat_summary(fun = mean, geom='point', shape=20, size=6, color='red')
      gp<-gp+stat_summary(fun = mean, geom = 'text',aes(label=after_stat(y)),position=position_nudge(y=0.25))
      gp<-gp+labs(title=paste0(ttl,'-#',as.character(k)))
      plot(gp)
    }
    #wrap_plots(g)+plot_layout(ncol=1)
  }
  dev.off()
}

