library(qualtRics)
library(ggplot2)
library(patchwork)
library(tidyr)
library(dplyr)

rm(list=ls())

fn<-file.choose()
tdat<-qualtRics::read_survey(fn,col_types = readr::cols(Q11=readr::col_character()))

# データ数確認
all<-nrow(tdat)
cons<-sum(tdat$Q2=='同意する')
ncons<-sum(tdat$Q2=='同意しない')
jitai<-sum(tdat$Q2=='同意する' & tdat$Q9=='辞退する')
exl<-sum(tdat$Q2=='同意しない' | tdat$Q9=='辞退する')

print(paste0('総数:',as.character(all)))
print(paste0('同意:',as.character(cons)))
print(paste0('有効:',as.character(all-exl)))
print(paste0('有効回答率:',as.character(100*(all-exl)/all),'%'))

# データの全体像

dat<-tdat[(tdat$Q2=='同意する' & tdat$Q9=='辞退しない'),]
dat$Q11f<-apply(dat,1,function(x){
  temp<-strsplit(x[23],',')
  # print(temp)
  return(temp[[1]][1])
})



hist(dat$Q8, ylab='人数', xlab='身長', main='身長の分布')
print(paste0(mean(dat$Q8, na.rm = T), '/ ', sd(dat$Q8, na.rm = T)))

hist(dat$Q15, ylab='人数', xlab='平均朝食回数', main='平均回数の分布')
print(paste0(mean(dat$Q15, na.rm = T), '/ ', sd(dat$Q15, na.rm = T)))

print(paste0('朝食を食べた人: ',sum(dat$Q14=='食べた'),'/',as.character(all-exl)))



# 性差と朝食：質的データの度数をカテゴリ間で比較
Q5table<-dat %>% dplyr::group_by(Q5) %>% dplyr::select(Q14) %>% table
Q5dat<-Q5table %>% as.data.frame()
g5<-ggplot(data=Q5dat, aes(x=Q5, y=Freq, fill=Q14))+geom_bar(stat="identity",position="dodge")
g5<-g5+ylab('人数')+theme(text=element_text(size=18))
g5<-g5+xlab(attr(dat$Q5,'label'))
print(Q5table)
plot(g5)

# 性差と平均：量的データの代表値をカテゴリ間で比較
Q5msummary<- dat %>% group_by(Q5) %>% summarise(count=n(), mean=mean(Q15,na.rm=T), sd=sd(Q15, na.rm=T), median=median(Q15, na.rm=T))
g5m<-ggplot(data=dat, aes(x=Q5, y=Q15))+geom_boxplot()
g5m<-g5m+ylab('朝食平均回数')+theme(text=element_text(size=18))
g5m<-g5m+xlab(attr(dat$Q5,'label'))
print(Q5msummary)
plot(g5m)

# 就寝時間と朝食：度数をカテゴリ間で比較
Q12table<-dat %>% dplyr::group_by(Q12) %>% dplyr::select(Q14) %>% table
Q12dat<-Q12table %>% as.data.frame()
g12<-ggplot(data=Q12dat, aes(x=Q12, y=Freq, fill=Q14))+geom_bar(stat="identity",position="dodge")
g12<-g12+ylab('人数')+theme(text=element_text(size=18))
g12<-g12+xlab(attr(dat$Q12,'label'))
print(Q12table)
plot(g12)

# 課外活動：度数をカテゴリ間で比較
Q11table<-dat %>% dplyr::group_by(Q11f) %>% dplyr::select(Q14) %>% table
Q11dat<- Q11table %>% as.data.frame()
g11<-ggplot(data=Q11dat, aes(x=Q11f, y=Freq, fill=Q14))+geom_bar(stat="identity",position="dodge")
g11<-g11+xlab(attr(dat$Q11,'label'))+ylab('人数')+theme(text=element_text(size=18))
plot(g11)

# 身長と平均：量的データ同士の関係を表示
#Q8table<-dat %>% dplyr::group_by(Q8) %>% dplyr::select(Q15) %>% table
#Q8dat<- Q8table %>% as.data.frame()
g8<-ggplot(data=dat, aes(x=Q8, y=Q15))+geom_point(size=5)
xl<-attr(dat$Q8,'label')
g8<-g8+xlab(substr(xl,1,2))+ylab('平均回数')+theme(text=element_text(size=18))
plot(g8)

# 朝食と平均：見方を変える（縦軸として比較していたものを横軸に）
Q14msummary<- dat %>% group_by(Q14) %>% summarise(count=n(), mean=mean(Q15,na.rm=T), sd=sd(Q15, na.rm=T), median=median(Q15, na.rm=T))
g14m<-ggplot(data=dat, aes(x=Q14, y=Q15))+geom_boxplot()
g14m<-g14m+ylab('朝食平均回数')+theme(text=element_text(size=18))
g14m<-g14m+xlab('今日の朝食')
print(Q14msummary)
plot(g14m)

# 平均2000と500, SDが5000のダミーデータ

datA<-rnorm(1000, mean=2000, sd=5000)
datB<-rnorm(1000, mean=500, sd=5000)

barplot(c(mean(datA), mean(datB)), ylim=c(0,2000))

wdat<-data.frame(cbind(datA,datB))
colnames(wdat)<-c('A','B')
ldat<-pivot_longer(wdat, cols = c('A','B'))
demoG<-ggplot(data = ldat, aes(x=name, y=value))+geom_boxplot()
plot(demoG)
