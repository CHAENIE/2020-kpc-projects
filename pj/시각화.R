library(ggplot2)
install.packages("ggpubr")
library(ggpubr)
install.packages("plotly")
library(plotly)
install.packages("moonBook")
library(moonBook)
female$s31

mytable(w2~gender, data=busan)

ggplot(data=busan, aes(x="gender", y="s31"))+ 
  geom_boxplot(aes(fill = gender))+ 
  stat_compare_means(method = "wilcox.test",label.x=1.4,label.y=35,size=5)+
  scale_fill_manual(values=c("green","blue"))+ 
  theme_bw()
plotly::ggplotly(gg)
boxplot(s31~gender,data=busan)

ggboxplot(busan, x = "gender", y = "s31", 
          color = "gender", palette = c("#00AFBB", "#E7B800"),
          ylab = "s31", xlab = "gender")

#1-gender wilcox#############################
#s31
busan$gender;busan$s31
male <- subset(busan, gender==1)
male
female <- subset(busan, gender==2)
female
malecount <- length(male$gender)
femalecount <- length(female$gender)
malecount;femalecount

malemean <- round(mean(male$s31),2)
femalemean <- round(mean(female$s31),2)
malemean;femalemean
mfcount <- c(malecount,femalecount)
mfmean <- c(malemean,femalemean)
mftable <- data.frame(Freq=mfcount, Mean=mfmean)
mftable
var.test(male$s31, female$s31)
wilcox.test(male$s31, female$s31)

male$s31
female$s31


#그래프
#성별
gender_bar <- aggregate(opinion2~gender,data=busan, mean)
plot_ly(gender_bar, x=~gender, y=~opinion2, type="bar")
#연령대
age2_bar <- aggregate(opinion2~age2,data=busan, mean)
plot_ly(age2_bar, x=~age2, y=~opinion2, type="bar")

group10s<-subset(busan,age2=="10s")
group20s<-subset(busan,age2=="20s")
group30s<-subset(busan,age2=="30s")
group40s<-subset(busan,age2=="40s")
group50s<-subset(busan,age2=="50s")
group60s<-subset(busan,age2=="over 60s")

groupmean=c(round(mean(group10s$opinion2),2),round(mean(group20s$opinion2),2),round(mean(group30s$opinion2),2),round(mean(group40s$opinion2),2),round(mean(group50s$opinion2),2),round(mean(group60s$opinion2),2))
groupmeantable=data.frame(Mean=groupmean)
groupmeantable

#ggmap
cen <- c(35, 129) # set center

map <- get_googlemap(center=cen) # create map

ggmap(map) # on the map

install.packages('tidyverse')
library(tidyverse)
library(ggmap)

# Googke Map API 입력 (API가 없으면 geocode가 작동하지 않는다)
register_google(key='AIzaSyB-6o0vZa7QcNPYBMcXMVP1FG1tE7U4JK4')

#스마트팜부산
# 주소 tibble 생성
address = c('47558 부산광역시 연제구 고분로191번길 1')
name = c('smartfarm_busan')

list = cbind(name, address) %>% as.tibble

# geocode 함수로 경도 위도 데이터 생성
coordinate = list$address %>% enc2utf8() %>% geocode()
coordinate

# 주소와 경도 위도 값의 결합
geocoded = cbind(list, coordinate) %>% as.tibble
geocoded

smartfarm = get_googlemap('busan',
                        maptype = 'roadmap',
                        zoom = 13)
ggmap(smartfarm) +
  geom_point(data = geocoded,
             aes(x = lon, y = lat), color = 'red', size = 3)

#부산 서구 free wifi
setwd("C:/Rpro")
wifi=read.csv("busan_west_wifi2.csv", header = T)
View(a)
head(a)

attach(wifi)

bmap <- (ggmap(get_googlemap(center = c("busan"), zoom=11, maptype = "roadmap")) 
         + geom_point(data=wifi, aes(x=LON, y=LAT), size=2.5, colour='blue')
         + geom_point(data = geocoded,aes(x = lon, y = lat), color = 'red', size = 3))
print(bmap)



