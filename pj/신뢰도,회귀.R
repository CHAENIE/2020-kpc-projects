#w7, lc9(대중교통이용만족도)-신뢰도분석 152p~
setwd("C:/Rpro")
busan<-read.csv("busan.csv", header = T)
library(psych)
library(dplyr)

#근로여건만족도, 대중교통이용만족도 - 신뢰도분석
busan$lc9_1 <- ifelse(busan$lc9_1==6, NA, busan$lc9_1)
busan$lc9_2 <- ifelse(busan$lc9_2==6, NA, busan$lc9_2)
busan$lc9_3 <- ifelse(busan$lc9_3==6, NA, busan$lc9_3)

w7_na<-busan %>% filter(!is.na(w7+lc9_1+lc9_2+lc9_3))     #결측치 제거해서 저장
w7_na$lc9_3

w7_sati <- select(w7_na,w7_1,w7_2,w7_3,w7_4,w7_5)
lc9_sati <- select(w7_na,lc9_1,lc9_2,lc9_3)
alpha(w7_sati) #raw_alpha: all 0.99
alpha(lc9_sati) #raw_alpha: all 0.78 / lc9_1 0.61,lc9_2 0.68,lc9_3 0.79



library(car)

#삶에 대한 만족도,근로여건 만족도,부산시 정주의사 선형회귀분석
busan<-read.csv("busan.csv", header = T)

#삶에 대한 만족도
lc9_na <- busan %>% filter(!is.na(lc9_1+lc9_2+lc9_3))
lc9_na$lc9 <- (lc9_na$lc9_1+lc9_na$lc9_2+lc9_na$lc9_3)/3

lc9_na$f43 <- ifelse(lc9_na$f43==6, NA, lc9_na$f43)
lc9_na$w7 <- ifelse(lc9_na$w7==6, NA, lc9_na$w7)
lc9_na <- lc9_na %>% filter(!is.na(f43+w7+lc9))
lc9_na$w7;lc9_na$f43;lc9_na$lc9;lc9_na$lc10

lc9_na$s31<-(lc9_na$s31_1+lc9_na$s31_2)/2
lc9_na$s31 <- 10-lc9_na$s31

s31.lm <- lm(s31~lc9+lc10+sl20+sl21+in40_1+f43, data=lc9_na)
summary(s31.lm)       
vif(s31.lm)           
s31.lm2 <- lm.beta(s31.lm) #표준화회귀계수값(베타)
summary(s31.lm2)

#근로여건 만족도
lc9_na$w7

w7.lm <- lm(w7~lc9+lc10+sl20+sl21+in40_1+f43, data=lc9_na)
summary(w7.lm)        
vif(w7.lm)            
w7.lm2 <- lm.beta(w7.lm) #표준화회귀계수값(베타)
summary(w7.lm2)


#부산시 정주의사
lc9_na$opinion

op.lm <- lm(opinion~lc9+lc10+sl20+sl21+in40_1+f43+s31+w7, data=lc9_na)
summary(op.lm)        
vif(op.lm)           
op.lm2 <- lm.beta(op.lm) #표준화회귀계수값(베타)
summary(op.lm2)

#그래프
library(plotly)
plotly(lc9_na, x=lc9_na$opinion, y=lc9_na$w7, type='scatter')

library(ggplot2)
ggplot(data=lc9_na, aes(x=opinion))+geom_bar()

#s31 :높긍 #w7 : 높부 #opinion : 높부
f.model <- lm(opinion~, data=lc9_na)
 r.model <- step(f.model, direction = "both")
# sl31.model <- glm(sl22~.,data=lc9_na)


# library(ggplot2)
# lc9_na$age2[lc9_na$age>=15 & lc9_na$age<20] <- "10s"
# lc9_na$age2[lc9_na$age>=20 & lc9_na$age<30] <- "20s"
# lc9_na$age2[lc9_na$age>=30 & lc9_na$age<40] <- "30s"
# lc9_na$age2[lc9_na$age>=40 & lc9_na$age<50] <- "40s"
# lc9_na$age2[lc9_na$age>=50 & lc9_na$age<60] <- "50s"
# lc9_na$age2[lc9_na$age>=60] <- "over 60s"
# ggplot(data=lc9_na, aes(x=lc9, y=w7, colour=age2))+geom_point()+geom_smooth()

