iris
str(iris)
# 4개의 데이터 모두 결국은 길이를 나타냄
# 때문에 굳이 표준화를 해줄 필요가 없다.
head(iris)
colSums(is.na(iris)) # iris에서 NA인 것의 갯수를 조사하는 삼수
panel.fun <- function(x,y,...){
  horizontal<-(par("usr")[1]+par("usr")[2])/2; # 두 값의 중앙값 계산, 상관계수 출력
  vertical<-(par("usr")[3]+par("usr")[4])/2;
  text(horizontal,vertical, format(abs(cor(x,y)),digits=2))
}
pairs(iris[1:4], # paris() : scatter float을 그리는 명령어
      pch=21, # 포인터의 크기
      bg=c("red","green","blue")[unclass(iris$Species)],
      upper.panel=panel.fun,
      main="Scatter")

library(ggplot2)
iris_plot<-ggplot(data=iris, aes(x=Petal.Length,y=Petal.Width, colour=Species))+
  geom_point(shape=19, size=4)

iris_plot2<-iris_plot+annotate("text",x=1.5,y=0.7,label="Setosa")+
  annotate("text",x=3.5,y=1.5,label="Versicolora")+
  annotate("text",x=6,y=2.7,label="Virginica")

iris_plot2+
  annotate("rect",xmin=0,xmax=2.6,ymin=0,ymax=0.8,alpha=0.1,fill="red")+
  # xmin~ymax는 좌표, fill은 색, alpha는 투명도
  annotate("rect",xmin=2.6,xmax=4.9,ymin=0.8,ymax=1.5,alpha=0.1,fill="green")+
  annotate("rect",xmin=4.8,xmax=7.2,ymin=1.5,ymax=2.7,alpha=0.1,fill="blue")

iris_kmeans<-kmeans(iris[,c('Petal.Length','Petal.Width')],3)
iris_kmeans
names(iris_kmeans)
iris_kmeans$size
table(iris_kmeans$cluster)


# ggplot2 패키지의 함수 -> geom_point(): 변수 1개의 산점도 그리기
# corrplot 패키지 : 상관계수 행렬 그리기

str(airquality)
airquality_1<-airquality[,c(1:4)]
str(airquality_1)
colSums(is.na(airquality_1))
sum(is.na(airquality_1$Ozone))
# 0이면 상관관계가 없다, -1 or 1에 근접할수록 상관관계가 높다.
# 1이면 양의 상관관계, -1이면 음의 상관관계
# ex) 키-몸무게 => 양의 상관관계
cor(airquality_1) # NA가 있으면 상관관계도 안나옴
# 결측값이 있는 행을 제거
airquality_2<-na.omit(airquality_1)
str(airquality_2)
colSums(is.na(airquality_2))
cor(airquality_2)


panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(USJudgeRatings, lower.panel = panel.smooth, upper.panel = panel.cor,
      gap=0, row1attop=FALSE)

pairs(iris[-5], log = "xy") # plot all variables on log scale
pairs(iris, log = 1:4, # log the first four
      main = "Lengths and Widths in [log]", line.main=1.5, oma=c(2,2,3,2))


pairs(airquality_2, pch='*',
      main="Scatter Plot",
      lower.panel=panel.lm, # 대각요소를 기준으로 아래쪽, 회귀모델 시각화를 아래쪽에 한 것
      upper.panel=panel.cor, # 대각요소에서 위쪽, 상관계수를 출력하도록
      diag.panel=panel.hist) # 대각요소로 hist를 출력
#pairs 함수를 사용하면 그룹별로 색을 다양하게 출력 가능

credit<-read.csv("Data/credit.csv")
str(credit)
summary(credit)
summary(credit$amount)
table(credit$default)

set.seed(123)
train_sample<-sample(1000,900)
str(train_sample)
credit_train<-credit[train_sample,]
credit_test<-credit[-train_sample,]

prop.table(table(credit_test$default))

install.packages("C50") # 의사결정 트리 알고리즘 패키지

library(C50)

credit_model<-C5.0(x=credit_train[-17],y=credit_train$default)
# 17번째 열을 제외한 나머지
credit_model
# Tree size = Tree depth
summary(credit_model)

credit_pred<-predict(credit_model, credit_test)
# 모델, 테스트로 사용할 데이터
credit_pred

library(gmodels)
CrossTable(credit_test$default, credit_pred, 
           prop.c=F, prop.r=F,
           dnn=c("actual",'predict'))

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}
pairs(USJudgeRatings[1:5], panel = panel.smooth,
      cex = 1.5, pch = 24, bg = "light blue", horOdd=TRUE,
      diag.panel = panel.hist, cex.labels = 2, font.labels = 2)

## put (absolute) correlations on the upper panels,
## with size proportional to the correlations.
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.7, 0.7, txt, cex = cex.cor * r)
}

panel.lm <- function(x,y,col=par("col"),bg=NA,pch=par("pch"),cex=1, col.smooth="black",...)
{
  points(x,y,pch=pch,col=col,bg=bg,cex=cex)
  abline(stats::lm(y~x),col=col.smooth,...)
}

pairs(iris[1:4], main = "Anderson's Iris Data -- 3 species",
      pch = 21, bg = c("red", "green3", "blue")[unclass(iris$Species)])
# pch를 21로 주면 색을 입혀서 출력 가능

unclass(iris$Species) # factor(범주형)->integer vector로 변환





# 부스팅 : 의사결정트리를 여러개 작성
# -> 각 의사결정트리에서 나온 결과에 대해 투표
# 성능이 약한 모델을 모아서 성능 개선

credit_boost10<-C5.0(credit_train[-17],credit_train$default, trials=10)
# 의사결정트리의 갯수 = 10(가장 best)
credit_boost10
summary(credit_boost10)

credit_boost_pred10<-predict(credit_boost10,credit_test)
CrossTable(credit_test$default, credit_boost_pred10, 
           prop.c=F, prop.r=F,
           dnn=c("actual",'predict'))



# 타이타닉 분석



install.packages("readr")
install.packages("rpart")
install.packages("rpart.plot")



library(readr)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(dplyr)

train<-read.csv("titanic/train.csv")
test<-read.csv("titanic/test.csv")

str(train)

dim(bind_rows(train, test))

Survived<-train$Survived
train$Survived<-NULL

dataset<-bind_rows(train,test)

str(dataset)
summary(dataset)

dataset$Fare

dataset$Fare[dataset$PassengerId==1044]<-median(dataset$Fare,na.rm=TRUE)
dataset$PassengerId[is.na(dataset$Fare)==TRUE]             

summary(dataset$Age)
dataset$Age<-sapply(dataset$Age, FUN=function(x){
  ifelse(is.na(x),median(dataset$Age,na.rm=TRUE),x)
})
summary(dataset$Age)

table(dataset$Embarked)

table(dataset$Embarked)/sum(dataset$Embarked!="")

dataset$PassengerId[dataset$Embarked==""]
dataset$Embarked[c(62,830)]<-"S"

nrow(dataset)
1-sum(dataset$Cabin !="")/nrow(dataset)
dataset$Cabin

#주어진 문자열에서 부분 문자열 추출
dataset$Cabin<-substr(dataset$Cabin,1,1)
# substr(a,x,y) -> a에서 x부터 y까지 문자를 추출하라

dataset$Cabin[dataset$Cabin==""]<-'H'

str(dataset)
factor_vars<-c('PassengerId','Pclass','Sex','Embarked','Cabin')
dataset[factor_vars]<-lapply((dataset[factor_vars]),function(x) as.factor(x))
str(dataset)

train_cleaned<-dataset[1:891,]
test_cleaned<-dataset[892:1309,]
train_cleaned$Survived<-Survived
DT<-rpart(Survived~Pclass+Sex+Embarked+Cabin,train_cleaned,method="class")
summary(DT)

predict_dt<-predict(DT,test_cleaned,type="class")

res<-data.frame(PassengerId=test_cleaned$PassengerId,
           Survived=predict_dt)
res

write.csv(res,file="result.csv",row.names=FALSE)
