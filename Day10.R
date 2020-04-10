# 말뭉치 구성
library(tm)
library(stringr)
my.text.location<-"c:/jsy/논문data/ymbaek_papers"
mypaper<-VCorpus(DirSource(my.text.location))
mypaper

#meta():메타데이터 구성
summary(mypaper)
mypaper[[2]] # 2번쨰 문서
mypaper[[2]]$content
mypaper[[2]]$meta

meta(mypaper[[2]], tag='author')<-"g.d.hong"
mypaper[[2]]$meta
# meta : mypaper[[2]]를 설명하는 데이터

myfunc<-function(x){
  # 특수기호(-, /, ...) 전후의 단어를 확인
  str_extract_all(x,"[[:alnum:]]{1,}[[:punct:]]{1}[[:alnum:]]{1,}")
  # alnum 알파벳+숫자 모두 + 특수문자 1글자 + 모든 문자
}
mypuncts<-lapply(mypaper, myfunc)
table(unlist(mypuncts))


myfunc<-function(x){
  # 수치 자료 추출
  str_extract_all(x,"[[:digit:]]{1,}")
  # alnum 알파벳+숫자 모두 + 특수문자 1글자 + 모든 문자
}
mydigits<-lapply(mypaper, myfunc)
table(unlist(mydigits))


myfunc<-function(x){
  # 고유명사추출
  myuppers<-str_extract_all(x,"[[:upper:]]{1}[[:alpha:]]{1,}")
  # alnum 알파벳+숫자 모두 + 특수문자 1글자 + 모든 문자
}



myfunc<-function(x){
  # 고유명사추출
  mydigits<-str_extract_all(x,"[[:digit:]]{1,}")
  # alnum 알파벳+숫자 모두 + 특수문자 1글자 + 모든 문자
}
myuppers<-lapply(mypaper, myfunc)
table(unlist(myuppers))

mycorpus<-tm_map(mypaper,removeNumbers)

mytempfunc<-function(myobject,oldexp,newexp){
  newobject<-tm_map(myobject, 
                    content_transformer(
                      function(x,pattern) gsub(pattern, newexp, 
                                               x)),oldexp)
  #x:myobject, pattern:-collar, newexp:collar
  newobject
}
mycorpus<-mytempfunc(mypaper,"-collar","collar")
mycorpus<-mytempfunc(mypaper,"e\\.g\\.","for example")
mycorpus<-mytempfunc(mypaper,"and/or","and or")

mycorpus<-tm_map(mycorpus,removePunctuation)
mycorpus<-tm_map(mycorpus,stripWhitespace)
mycorpus<-tm_map(mycorpus,
                 content_transformer(tolower))
mycorpus<-tm_map(mycorpus,removeWords, 
                 words=stopwords("SMART"))
mycorpus<-tm_map(mycorpus,
                 stemDocument, language='en')
#어근 동일화

#문자 개수 계산 함수
mycharfunc<-function(x){
  str_extract_all(x, ".")
}
#단어수 계산 함수
mywordfunc<-function(x){
  str_extract_all(x, boundary("word"))
}
mychar<-lapply(mypaper, mycharfunc)
myuniquechar0<-length(table(unlist(mychar))) #79문자 사용
mytotalchar0<-sum(table(unlist(mychar)))#24765글자
myword<-lapply(mypaper,mywordfunc)
myuniqueword0<-length(table(unlist(myword))) #1151 개 종류 단어
mytotalword0<-sum(table(unlist(myword))) #총 3504 개 단어 사용
#전처리 이후
mychar<-lapply(mycorpus, mycharfunc)
myuniquechar1<-length(table(unlist(mychar))) #79문자 사용
mytotalchar1<-sum(table(unlist(mychar)))#24765글자
myword<-lapply(mycorpus,mywordfunc)
myuniqueword1<-length(table(unlist(myword))) #1151 개 종류 단어
mytotalword1<-sum(table(unlist(myword))) #총 3504 개 단어 사용

results.comparing<-rbind(
  c(myuniquechar0, myuniquechar1),
  #전처리 전 글자 종류:79, 전처리 후 : 41
  c(mytotalchar0, mytotalchar1), #조금 후 수정
  c(myuniqueword0, myuniqueword1),
  #1151, 710
  c(mytotalword0, mytotalword1))
#3504, 2060
results.comparing

colnames(results.comparing)<-c("before","after")
rownames(results.comparing)<-c("고유문자수","총문자수",
                               "고유단어수","총단어수")
results.comparing
colnames(results.comparing)<-c("before","after")
rownames(results.comparing)<-c("고유문자수","총문자수","고유단어수","총단어수")
results.comparing
dtm.e$dimnames
# 문서*단어 행렬 구성
dtm.e<-DocumentTermMatrix(mycorpus)
dtm.e
# 가로줄 이름(문서 이름)
rownames(dtm.e[,])
# 단어
colnames(dtm.e[,])
# 행렬의 내용 참조
inspect(dtm.e[1:3,50:55])
# TF-IDF
dtm.e.tfidf<-DocumentTermMatrix(mycorpus, control=list(weighting=function(x) weightTfIdf(x,normalize=FALSE)))
dtm.e.tfidf

inspect(dtm.e.tfidf[1:3,50:55])

# TF는 크지만 ,TF-IDF는 작은 단어들 검출
value.tf.dtm<-as.vector(as.matrix(dtm.e[,]))
value.tf.dtm
value.tfidf.dtm<-as.vector(as.matrix(dtm.e.tfidf[,]))
value.tfidf.dtm

# rep("test",each=3) 특정문자를 반복할때 사용
rep(1:3, each=10)
rep(1:3, times=10) # defalut:times

dim(dtm.e[,])[1] # 24개 문서, 703개 단어
colnames(dtm.e[])

word.label.dtm<-rep(colnames(dtm.e[]),each=dim(dtm.e[,])[1])
doc.label.dtm<-rep(rownames(dtm.e[]),dim(dtm.e[,])[2])
mydata<-data.frame(word.label.dtm, doc.label.dtm,
          value.tf.dtm, value.tfidf.dtm)

colnames(mydata)<-c("word","doc","tf","tfidf")
mydata
mydata[120:130,]

cor.test(mydata$tf,mydata$tfdif, method="kendall") # 0.9839 아주 강한 상관계수
mydata$tf[mydata$tf>0] # tf값이 0보다 큰 자료 출력
mydata$tfdif[mydata$tfdif>0] # tfidf값이 0보다 큰 자료 출력
cor.test(mydata$tf[mydata$tf>0],mydata$tfdif[mydata$tfdif>0],method="kendall")
# 0.4639738
# TF값의 순위가 높아도 TFIDF값의 순위가 높지 않은 경우가 적지 않다.


# 어떤 단어가 TF가 높고 TFIDF가 낮은지?
# 1) TF, TFIDF가 모두 0보다 큰 데이터만 추출
# 2) TF값 > 중위수, TFIDF < 중위수
# class(mydata) => sub dataframe을 구성 할떄 ==> subset 사용

mydata2<-subset(mydata, tf>0 & tfidf>0) # mydata에서 tf가 0보다 크고, idf도 0보다 큰 거
mydata3<-subset(mydata2, tf>median(mydata2$tf) & tfidf<median(mydata2$tfidf))
mydata3
str(mydata3)
table(mydata3$word)[table(mydata3$word)>0]


#tfidf 매트릭스
##########################################
install.packages("KoNLP")
install.packages("rJava")
library(KoNLP)
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jdk-13.0.2')
           

concrete<-read.csv("Data/concrete.csv")
str(concrete)

normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
concrete_norm<-as.data.frame(lapply(concrete,normalize))
summary(concrete_norm)

concrete_train<-concrete_norm[1:773,]
concrete_test<-concrete_norm[774:1030,]
# install.packages("neuralnet")
library(neuralnet)

concrete_model<-neuralnet(formula = strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,
          data=concrete_train)

plot(concrete_model)

concrete_test[1:8]
model_results<-compute(concrete_model, concrete_test[1:8])
str(model_results)

pre_str<-model_results$net.result # 이게 예측값임
pre_str

cor(pre_str,concrete_test$strength)
# 0.8 정도면 매우 강한 상관관계가 있다.

concrete_model2<-neuralnet(formula = strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,
                          data=concrete_train, hidden=5)
plot(concrete_model2)
model_results2<-compute(concrete_model2,concrete_test[1:8])
pre_str2<-model_results2$net.result
cor(pre_str2,concrete_test$strength)


iris<-iris[1:5]

iris
colnames(iris)
# "Sepal.Length" "Sepal.Width"  "Petal.Length" 로 "Petal.Width" 예측해보기
# data를 random하게 shuffle
# 70 : 30 = train : test 
# cor(hidden 레이어 수를 다르게 해보면서 구해보고) cor가 몇개일때 가장 높게 나오는 지?
my_iris<-iris[1:4]
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
my_iris<-lapply(my_iris, normalize)
my_iris<-data.frame(my_iris)
my_iris_sample<-sample(length(my_iris[[1]]), length(my_iris[[1]])*0.7)
my_iris_train<-my_iris[my_iris_sample,]
my_iris_test<-my_iris[-my_iris_sample,]

my_iris_model0<-neuralnet(formula= Petal.Width~Sepal.Length+Sepal.Width+Petal.Length,
                      data=my_iris_train)
plot(my_iris_model0)
my_iris_result0<-compute(my_iris_model0,my_iris_test)
my_iris_str0<-my_iris_result0$net.result
my_iris_str0
cor(my_iris_str0,my_iris_test$Petal.Width)
# 0.9793125



my_iris_model2<-neuralnet(formula= Petal.Width~Sepal.Length+Sepal.Width+Petal.Length,
                          data=my_iris_train, hidden=2)
plot(my_iris_model2)
my_iris_result2<-compute(my_iris_model0,my_iris_test)
my_iris_str2<-my_iris_result0$net.result
my_iris_str2
cor(my_iris_str2,my_iris_test$Petal.Width)
# 0.9793125

my_iris_model3<-neuralnet(formula= Petal.Width~Sepal.Length+Sepal.Width+Petal.Length,
                          data=my_iris_train, hidden=3)
plot(my_iris_model3)
my_iris_result3<-compute(my_iris_model0,my_iris_test)
my_iris_str3<-my_iris_result0$net.result
cor(my_iris_str3,my_iris_test$Petal.Width)





my_iris<-iris[1:4]
my_iris_sample<-sample(length(my_iris[[1]]), length(my_iris[[1]])*0.7)
my_iris_train<-my_iris[my_iris_sample,]
my_iris_test<-my_iris[-my_iris_sample,]
my_iris_model10<-neuralnet(formula= Petal.Width~Sepal.Length+Sepal.Width+Petal.Length,
                          data=my_iris_train, hidden=10)
plot(my_iris_model10)
my_iris_result10<-compute(my_iris_model0,my_iris_test)
my_iris_str10<-my_iris_result10$net.result
cor(my_iris_str10,my_iris_test$Petal.Width)
# 0.9791895

install.packages("Sejong")
install.packages("hash")
install.packages("tau")
install.packages("RSQLite")
install.packages("rgdal")
install.packages("geojsonio")
install.packages("rgeos")
library(KoNLP)
library(httpuv)
library(rgdal)
library(geojsonio)
library(rgeos)
sentence <- '아버지가 방에 들어가신다'
extractNoun(sentence)
