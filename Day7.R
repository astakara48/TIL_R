sms_raw<-read.csv('sms_spam_ansi.txt', stringsAsFactors = FALSE)
str(sms_raw)
sms_raw$type<-factor(sms_raw$type)
str(sms_raw$type)
table(sms_raw$type)

# 텍스트 데이터 정리, 표준화

# tm 패키지: 텍스트 마이닝 패키지
# 설치
install.packages('tm')
str(sms_raw$text)
library(tm) # -> 텍스트 마이닝
# 코퍼스: 단어 집합 생성 -> VCorpus() -> 메모리에 만들어짐
# 데이터 소스 객체 생성 -> VectorSource()
sms_corpus<-VCorpus(VectorSource(sms_raw$text))
sms_corpus
inspect(sms_corpus[1:2]) # chars는 글자수를 나타냄(공백포함)

sms_corpus[1]
sms_corpus[[1]]

as.character(sms_corpus[[1]]) # as.character -> 1번 행의 내용출력
# 1번부터 5번까지 문서 내용 출력
lapply(sms_corpus[1:5], as.character)

sms_corpus_clean<-tm_map(sms_corpus, content_transformer(tolower))
# 옛날 R 버전에서는 아래와 같이 작업할 것
# tm_map(sms_corpus,content_transformer(tolower))
class(sms_corpus_clean)
class(as.character(sms_corpus_clean[[1]]))
# character함수로 변환해줘야 문자열에 대한 작업을 할 수 있다

# 매킨토시 운영체제의 경우 아래와 같은 옵션을 준다
# read.csv(함수에 추가할 것)
# fileEncoding='cp949', encoding='UTF-8'

# 숫자제거
sms_corpus_clean<-tm_map(sms_corpus_clean,removeNumbers)
inspect(sms_corpus_clean[1:5])

# 구두점 제거
removePunctuation('hi.....hello...bye')
sms_corpus_clean<-tm_map(sms_corpus_clean,removePunctuation)
inspect(sms_corpus_clean[1:5])

# 불용어(stop words) 제거
# 불용어: to, and, but, or ...
stopwords()
?stopwords

sms_corpus_clean<-tm_map(sms_corpus_clean, removeWords, stopwords())
inspect(sms_corpus_clean[1:5])

replacePuntuation<-function(x){
  gsub("[[:punct:]]+", " ", x) # + -> 한글자 이상의 punctuation에 대해 적용
}
# x에 전달된 문자열에 대해  punctuation은 제거(" ")로 변경
replacePuntuation('hi+.[hello<;')

x='대한민국 조선 우리나라 대한 민국 대한민국'
gsub('대한민국','코리아',x) # 대한민국을 코리아로 바꿔준다
gsub('한국','코리아',x)
gsub('우리나라','코리아',x)
gsub('조선','코리아',x)

# 형태소 분석
install.packages('SnowballC')
library(SnowballC)
wordStem(c('learn','learned','learning','learns')) # 단어의 어근을 추출

# stemDocument함수: 텍스트 문서의 전체 코퍼스에 적용
# 코퍼스에 wordstem을 적용
sms_corpus_clean<-tm_map(sms_corpus_clean, stemDocument)
inspect(sms_corpus_clean[1:5])

# 추가 여백 제거
sms_corpus_clean<-tm_map(sms_corpus_clean, stripWhitespace)
lapply(sms_corpus[1:3], as.character)
inspect(sms_corpus_clean[1:3])
########################################################################
# 토큰화(단어) -> 위에 했던 작업을 더 단순하게 작업
# DocumentTermMatrix(): sms 메세지 코퍼스 -> 토큰화
# 행: sms 메시지, 열: 단어
# DTM 행렬, TDM 행렬(행: 단어, 열: 메시지)

sms_dtm<- DocumentTermMatrix(sms_corpus_clean)

sms_dtm2<-DocumentTermMatrix(sms_corpus, control = list(
  tolower=TRUE,
  removeNumbers=TRUE,
  stopwords=TRUE,
  removePunctuation=TRUE,
  stemming=TRUE
))

sms_dtm_train<-sms_dtm2[1:4169,]
sms_dtm_test<-sms_dtm2[4170:5559,]

sms_train_labels<-sms_raw[1:4169,]$type
sms_test_labels<-sms_raw[4170:5559,]$type

install.packages('wordcloud')
library(wordcloud)
wordcloud(sms_corpus_clean, scale=c(5,0.2), min.freq=50, max.words=100, rot.per=0.1,
          random.color=T, colors=brewer.pal(10, 'Paired'), random.order=FALSE)
# min.freq=50 -> 해당단어가 최소 50번 이상은 나온다
# max.words=100 -> 단어가 최대 100번
# colors=brewer.pal(10, 'Paired') -> 10은 열가지 색상을 사용한다는 뜻
# rot.per=0.1 글자 회전
# scale=c(5,0.2) -> 글자 크기가 5에서 0.2 사이

spam<-subset(sms_raw, type=='spam')
ham<-subset(sms_raw, type=='ham')
wordcloud(spam$text, max.words=40, scale=c(3, 0.5))
wordcloud(ham$text, max.words=40, scale=c(3, 0.5))

sms_dtm_train

sms_freq_words<-findFreqTerms(sms_dtm_train,5)
# 최소 5번 이상 등장한 단어 출력
str(sms_freq_words)

convert_counts<-function(x){
  x<-ifelse(x>0, 'Yes','No')
}

# 행렬의 열 단위로 전달(apply, MARGIN=1(행),2(열))
sms_dtm_freq_train<-sms_dtm_train[,sms_freq_words]
sms_dtm_freq_test<-sms_dtm_test[,sms_freq_words]
sms_train<-apply(sms_dtm_freq_train,MARGIN = 2, convert_counts )
# 열 단위로 함수에 데이터를 전달
sms_test<-apply(sms_dtm_freq_test,MARGIN = 2, convert_counts )
dim(sms_train)

# 나이브 베이지안 필터기 생성(모델)
install.packages('e1071')
library(e1071)
sms_classifier<-naiveBayes(sms_train, sms_train_labels)

sms_test_pred<-predict(sms_classifier,sms_test)
sms_test_pred # 예측모델
library(gmodels)
CrossTable(sms_test_pred, sms_test_labels,
           prop.t=FALSE, prop.r=FALSE,
           dnn=c('predicted', 'actual'))

sms_classifier2<-naiveBayes(sms_train, sms_train_labels, laplace = 1)
sms_test_pred2<-predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_test_labels,
           prop.t=FALSE, prop.r=FALSE,
           dnn=c('predicted', 'actual')) 


mushrooms<-read.csv("Data/mushrooms.csv")
str(mushrooms) # 8124

mushrooms_corpus<-VCorpus(VectorSource(mushrooms[-1]))


mushroom_dtm<-DocumentTermMatrix(mushrooms_corpus, control = list(
  tolower=TRUE,
  removeNumbers=TRUE,
  stopwords=TRUE,
  removePunctuation=TRUE,
  stemming=TRUE
))
mushroom_dtm

sms_dtm_train<-sms_dtm2[1:4169,]
sms_dtm_test<-sms_dtm2[4170:5559,]

sms_train_labels<-sms_raw[1:4169,]$type
sms_test_labels<-sms_raw[4170:5559,]$type

install.packages('wordcloud')
library(wordcloud)
wordcloud(sms_corpus_clean, scale=c(5,0.2), min.freq=50, max.words=100, rot.per=0.1,
          random.color=T, colors=brewer.pal(10, 'Paired'), random.order=FALSE)

