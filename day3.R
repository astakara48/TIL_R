#텍스트마이닝:text mining
#문장->형태소분석->명사,동사 의미를 갖고 있는 품사 추출->시각화
#텍스트마이닝을 하려면 R에서는 JAVA가 설치가 되어 있어야 함

install.packages('rJava')
install.packages('memoise')
install.packages("KoNLP")
library(KoNLP)
Sys.setenv(JAVA_HOME="C:/Program Files (x86)/Java/jre1.8.0_241")
library("rJava")
library(KoNLP)

install.packages('ggiraphExtra')
library(ggiraphExtra)

str(USArrests)

head(USArrests)

library(tibble)

crime<-rownames_to_column(USArrests, var='state')
head(crime)
crime$state<-tolower(crime$state)
crime$state
str(crime)

#미국 지도 데이터 준비
library(ggplot2)
install.packages('maps')
library(maps)
map_data('state')
states_map<-map_data('state')
str(states_map)

install.packages("mapproj")
library(mapproj)
ggChoropleth(data=crime, # 지도에 표시할 데이터
             aes(fill=Murder, # 색으로 표시 할 변수
                 map_id=state), # 지역 기준 변수
             map=states_map,
             interactive=T) # 지도 데이터터

install.packages("stringi")
install.packages("devtools")
devtools::install_github("cardiomoon/kormaps2014")
library(devtools)
library(kormaps2014)
str(changeCode(korpop3))
#head(changeCode(korpop2))

library(dplyr)
korpop1<-rename(korpop1, pop="총인구_명",name="행정구역별_읍면동")
korpop1<-changeCode(korpop1)
str(korpop1)

library(ggiraphExtra)
str(changeCode(korpop1))
ggChoropleth(data=korpop1, # 지도에 표시할 데이터
             aes(fill=pop, # 색깔로 나타낼 변수
                 map_id=code, # 지역에 대한 기준 변수
                 tooltip=name),  # 지도 위에 표시할 데이터 부분
             map=kormap1,
             interactive=T)

ggplot(korpop1,
       aes(map_id=code, fill="총인구_명"))+
  geom_map(map=kormap1, colour="black", size=0.1)+
  expand_limits(x=kormap1$long, y=kormap1$lat)+
#  scale_fill_gradientn(colours = c("white","orange","red"))+
  ggtitle("2015년 인구분포도")+
  coord_map()

1devtools::install_github("cardiomoon/moonBook2")
ggChoropleth(korpop2,kormap2,fillvar="남자_명")



#######머신러닝########
#통계적 기법, 연산 능력, 빅데이터
#데이터마이닝?  

subject_name<-c('John','Jane','Steve')
temp<-c(37,35,33)
flu_status<-c(TRUE,FALSE,TRUE)
temp[2:3]
temp[-2] # 2번을 제외하고 출력
temp[c(TRUE,FALSE,TRUE)] # 불린값으로 주면 TRUE에 해당하는 것만 출력

#팩터:명목형 데이터를 표현

gender<-factor(c("M","F","M"))
gender

blood<-factor(c("O","AB","A"), levels=c("O","AB","A","B"))
blood

factor(c("A","F","C"), levels=c('A','B','C','D','F'), ordered=T)

subject_name
#리스트:순서x, 타입이 다양

sb1<-list(fn=subject_name[1],temp=temp[1],flu=flu_status[1])
sb1
sb1$fn
class(sb1[1])
class(sb1[[1]])

sb1[c("temp","flu")] # temp롸 flu에 해당하는 값 추출

df=data.frame(sb1, stringsAsFactors = F)
# stringsAsFactors : 팩터형으로 문자열을 읽을것인가?
str(df)

#apply계열함수:함수연산을 특정단위로 쉽게 할 수 있도록 지원
#for,while(소규모 반복연산)
#apply(대규모 반복 연산)

iris_num<-NULL
iris
class(iris)
str(iris)
ncol(iris)
for(x in 1:ncol(iris)){
  if(is.numeric(iris[,x]))
    iris_num<-cbind(iris_num,iris[,x])
  #print(iris[,x])
}
class(iris_num)
iris_num<-data.frame(iris_num)


iris_num<-iris[,sapply(iris,is.numeric)]
class(iris_num)

iris_num<-iris[1:10,1:4]
set.seed(123)
idx_r<-sample(1:10,2)
idx_c<-sample(1:4,2)

idx_r # 6 5
idx_c # 4 2
for(i in 1:2){
  iris_num[idx_r[i],idx_c[i]]<-NA
  
}
iris_num

#apply:행(1) 또는 열(1) 단위 연산 (MARGIN)
#입력:배열,메트릭스(같은 변수형)
#출력:메트릭스 또는 벡터

apply(iris_num,1,mean,na.rm=T) #1이면 행단위

apply(iris_num,2,mean,na.rm=T) #2이면 열단위

apply(iris_num,2,function(x){x*2+1})

apply(iris_num,2,function(x){median(x*2+1, na.rm=T)})

# lapply : list+apply => 실행결과가 list로 출력력

# 데이터프레임 : 모든 변수가 벡터를 가져야 함 => 타입이 같아야 한다.
# 리스트 : 벡트, 매트릭스, 데이터프레임 다 저장 가능
class(apply(iris_num,2,mean,na.rm=T))

class(lapply(iris_num,mean,na.rm=T))

#sapply : lapply와 비슷, 간단하게 기술
# 연산겨로가가 벡트, 리스트(길이가 다른 경우)

sapply(iris_num,mean,na.rm=T, simplify = F)

#vapply:sapply+템플릿 지정
sapply(iris_num,fivenum) # fivenum 5가지 요약수치를 만들어 주는 거 // 
vapply(iris_num,fivenum,c("Min."=0,"1st."=0,"med."=0,"3rd."=0,"max."=0))

usedcars<-read.csv("Data/usedcars.csv", stringsAsFactors = FALSE)
str(usedcars)

summary(usedcars$year)
summary(usedcars[c("price","mileage")])
range(usedcars$price)

diff(range(usedcars$price))

IQR(usedcars$price)
quantile(usedcars$price,seq(from=0,to=1,by=0.1))
boxplot(usedcars$price, main="Car Prices", ylab="price($)")
boxplot(usedcars$mileage, main="Car mileage", ylab="odometer")

hist(usedcars$price, main="Car Prices", xlab="price($)")
hist(usedcars$mileage, main="Car mileage", xlab="odometer")

var(usedcars$price)
sd(usedcars$price)
#분산 (데이터-평균)^2합의 평균
#표준편차 : 분산의 제곱근

table(usedcars$year)
table(usedcars$model)
table(usedcars$color)

c_table<-table(usedcars$color)
prop.table(c_table)

#일변량 통계
#이변량 통계(두 변수의 관계)
#다변량 통계(두 개 이상의 변수 관계)
#산포도(이변량)

plot(x=usedcars$mileage,
     y=usedcars$price)

usedcars$conservative<-usedcars$color%in%c("Black","Gray","Silver","White")
table(usedcars$conservative)

install.packages("gmodels")
library(gmodels)
CrossTable(x=usedcars$model, y=usedcars$conservative)


wbcd<-read.csv("Data/wisc_bc_data.csv", stringsAsFactors = FALSE)
str(wbcd)
wbcd<-wbcd[-1]
str(wbcd)

table(wbcd$diagnosis)
str(wbcd)
# 결과 레이블은 chr로 하면 안되고 factor로 해야함

wbcd$diagnosis<-factor(wbcd$diagnosis, levels=c("B","M"), labels=c("Benign","Malignant"))
wbcd$diagnosis
table(wbcd$diagnosis)
round(prop.table(table(wbcd$diagnosis))*100,1)
summary(wbcd[c("radius_mean","area_mean","smoothness_mean")])
# 면적의 범위가 너무 다름 => area가 너무 크기에 영향을 가장 많이 받음

normalize<-function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
normalize(c(1,2,3,4,5))

wbcd_n<-as.data.frame((lapply(wbcd[2:31],normalize))) #출력이 list라서 lapply임
wbcd_n
summary(wbcd_n$area_mean)

wbcd_train<-wbcd_n[1:469,] #1~469번 까지는 트레인 데이터로 사용
wbcd_test<-wbcd_n[470:569,] # 469~569 까지는 테스트 데이터로 사용

wbcd_train_labels<-wbcd[1:469,1]
wbcd_train_labels
wbcd_test_labels<-wbcd[470:569,1]

library(class)
wbcd_test_pred<-knn(train=wbcd_train,
    test=wbcd_test,
    cl=wbcd_train_labels,
    k=21) # k=짝수로 주면 동점이 나오는 경우가 있으므로 반드시 홀수로
wbcd_test_pred

library(gmodels)

CrossTable(x=wbcd_test_labels,  # 정답 데이터
           y=wbcd_test_pred,# 추측한 결과 데이터
           prop.chisq=FALSE)

#정규화:표준화는 최대/최소값이 없음.
# 그 값이 중심 방향으로 축소되지 않음
wbcd[1]
wbcd_z<-as.data.frame(scale(wbcd[-1]))
summary(wbcd_z$area_mean)
str(wbcd_z)
# 모델 -> 테스트 -> 정확도
wbcd_z_train<-wbcd_z[1:469,] #1~469번 까지는 트레인 데이터로 사용
wbcd_z_test<-wbcd_z[470:569,] # 469~569 까지는 테스트 데이터로 사용

wbcd_z_train_labels<-wbcd[1:469,1]
wbcd_z_test_labels<-wbcd[470:569,1]

wbcd_z_test_pred<-knn(train=wbcd_z_train,
                    test=wbcd_z_test,
                    cl=wbcd_z_train_labels,
                    k=21) # k=짝수로 주면 동점이 나오는 경우가 있으므로 반드시 홀수로
wbcd_z_test_pred


CrossTable(x=wbcd_z_test_labels,  # 정답 데이터
           y=wbcd_z_test_pred,# 추측한 결과 데이터
           prop.chisq=FALSE)





#iris
#data:1000건
#700                                 300
#490   210
#train validation                    test
#iris(1:35, 51:85, 101:135) => train // 나머지는 테스트 데이터
iris # species
iris
iris_n<-as.data.frame(lapply(iris[-5],scale))
iris_n
summary(iris_n)


iris_train<-iris_n[c(1:25,51:75,101:125),]
iris_validation<-iris_n[c(26:35,76:85,126:135),]
iris_test<-iris_n[c(36:50,86:100,136:150),]


iris_train_labels<-iris[c(1:25,51:75,101:125),5]
iris_train_validation_labels<-iris[c(26:35,76:85,126:135),5]
iris_test_labels<-iris[c(26:35,76:85,126:135),5]



# train-validation 확인
iris_test_validation_pred<-knn(train=iris_train,
                               test=iris_validation,
                               cl=iris_train_labels,
                               k=21)

iris_test_validation_pred

CrossTable(x=iris_test_labels,
           y=iris_test_validation_pred,
           prop.chisq=FALSE)

iris_train<-iris[c(1:35,51:85,101:135),]
iris_train_labels<-iris[c(1:35,51:85,101:135),5]
# train-test 확인
iris_test_pred<-knn(train=iris_train,
                    test=iris_test,
                    cl=iris_train_labels,
                    k=21)

iris_test_pred
iris_train_labels
iris_test_labels

CrossTable(x=iris_test_labels,
           y=iris_test_pred,
           prop.chisq=FALSE)


CrossTable(x=wbcd_z_test_labels,  # 정답 데이터
           y=wbcd_z_test_pred,# 추측한 결과 데이터
           prop.chisq=FALSE)

wbcd_z_test_pred<-knn(train=wbcd_z_train,
                      test=wbcd_z_test,
                      cl=wbcd_z_train_labels,
                      k=21) # k=짝수로 주면 동점이 나오는 경우가 있으므로 반드시 홀수로
wbcd_z_test_pred


CrossTable(x=wbcd_z_test_labels,  # 정답 데이터
           y=wbcd_z_test_pred,# 추측한 결과 데이터
           prop.chisq=FALSE)

