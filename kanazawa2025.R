library(qualtRics)
library(ggplot2)
library(patchwork)
library(tidyr)
library(dplyr)

rm(list=ls())

fn<-file.choose()
tdat<-qualtRics::read_survey(fn,col_types = readr::cols(Q11=readr::col_character()))
tdat<-transform(tdat, Q11=factor(Q11, levels=c('文化部','運動部','その他の活動','所属していない','答えない')))

all<-nrow(tdat)
cons<-sum(tdat$Q2=='同意する')
ncons<-sum(tdat$Q2=='同意しない')
jitai<-sum(tdat$Q2=='同意する' & tdat$Q9=='辞退する')
exl<-sum(tdat$Q2=='同意しない' | tdat$Q9=='辞退する')
dat<-tdat[(tdat$Q2=='同意する' & tdat$Q9=='辞退しない'),]

dat$Q11f<-apply(dat,1,function(x){
  temp<-strsplit(x[23],',')
  return(temp[[1]][1])
})

# 就寝時間と朝食
Q12table<-dat %>% dplyr::group_by(Q12) %>% dplyr::select(Q14) %>% table
Q12dat<-Q12table %>% as.data.frame()
g12<-ggplot(data=Q12dat, aes(x=Q12, y=Freq, fill=Q14))+geom_bar(stat="identity",position="dodge")
g12<-g12+ylab('人数')+theme(text=element_text(size=18))
g12<-g12+xlab(attr(dat$Q12,'label'))
print(Q12table)
plot(g12)

# 身長と平均
#Q8table<-dat %>% dplyr::group_by(Q8) %>% dplyr::select(Q15) %>% table
#Q8dat<- Q8table %>% as.data.frame()
g8<-ggplot(data=dat, aes(x=Q8, y=Q15))+geom_point(size=5)
xl<-attr(dat$Q8,'label')
g8<-g8+xlab(substr(xl,1,2))+ylab('平均回数')+theme(text=element_text(size=18))
plot(g8)

# 課外活動
Q11table<-dat %>% dplyr::group_by(Q11f) %>% dplyr::select(Q14) %>% table
Q11dat<- Q11table %>% as.data.frame()
g11<-ggplot(data=Q11dat, aes(x=Q11f, y=Freq, fill=Q14))+geom_bar(stat="identity",position="dodge")
g11<-g11+xlab(attr(dat$Q11,'label'))+ylab('人数')+theme(text=element_text(size=18))
plot(g11)

