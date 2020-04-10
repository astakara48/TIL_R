library(arules)
help(Epub)
data(Epub)
summary(Epub)

inspect(Epub[1:10])
itemFrequency(Epub[,1:10])
itemFrequencyPlot(Epub,
                  topN=20,
                  #support=0.01,
                  main='item frequency')

image(sample(Epub,500))



epub_rule<-apriori(data=Epub, 
        parameter=list(support=0.001, 
                       confidence=0.2, 
                       minlen=2))

summary(epub_rule)
inspect(epub_rule)

inspect(sort(epub_rule, by="lift")[1:20])
inspect(sort(epub_rule, by='support')[1:20])

#찾고자 하는 부분을 선택할 수 있는 함수
rule_ins<-subset(epub_rule, items %in% 
                   c("doc_72f","doc_4ac"))
# LHS, RHS 가리지 않고 들어가만 있으면 출력한다.
inspect(rule_ins)

rule_ins<-subset(epub_rule, lhs %in% 
                   c("doc_72f","doc_4ac")) # item 대신 lhs쓰면 lhs에만 있는 경우면 출력
# %in%는 적어도 하나의 제품이 존재하면 해당 규칙을 가져옴
inspect(rule_ins)

rule_ins<-subset(epub_rule, items %pin% 
                   c("60e"))
# %pin% 부분적으로 c("")에 존재하는 규칙을 가져옴
inspect(rule_ins)




rule_ins<-subset(epub_rule, lhs %ain% 
                   c("doc_6e8","doc_6e9"))
# %ain% => C("","")가 2개 요소를 갖고 있다면 반드시 2개 요소를 갖고 있는 규칙을 출력함
inspect(rule_ins)

rule_ins<-subset(epub_rule, items %pin% 
                   c("60e"))
inspect(rule_ins)

rule_ins<-subset(epub_rule, items %pin% 
                   c("60e") & confidence>0.25)
# & 조건 가능 ex) & confidence>0.25
inspect(rule_ins)

install.packages('arulesViz')
library(arulesViz)
# 연관 규칙 전용 시각화 tool
plot(epub_rule)
plot(sort(epub_rule, by='support')[1:20], method='grouped') # 원의 크기로 판단

plot(epub_rule, method='graph', 
     control=list(type='items'),
     vertex.label.cex=0.7, # 점의 크기 default가 1 // vertex로 시작하는건 점과 관련
     edge.arrow.size=0.3, # 화살표의 크기
     edge.arrow.width=2)  # edge로 시작하는 건 선과 관련된 parameter들

# 원의 크기 : 지지도에 비례
# 원의 색깔 : 향상도(lift)에 비례
# 화살표 : lhs에서 rhs로 그려짐

teens<-read.csv("Data/snsdata.csv")
str(teens)
table(teens$gender, useNA='ifany') #useNA='ifany' => NA 값도 나오게 출력
summary(teens$age) # 수치형 데이터의 경우 summary를 통하여 na 확인 가능

# 13세 이상 2세 이하는 그대로, 나머지는 NA로
teens$age<-ifelse(teens$age>=13 & teens$age<20, teens$age, NA)
summary(teens$age)

teens$female<-ifelse(teens$gender=="F"&!is.na(teens$gender),1,0) 
# 기본적인 연산 과정에서 NA 연산 대상이 아님

teens$no_gender<-ifelse(is.na(teens$gender),1,0)
table(teens$no_gender)
table(teens$gender, useNA='ifany')
table(teens$female)
table(teens$no_gender)

mean(teens$age, na.rm=T)
myagg<-aggregate(data=teens, age~gradyear,mean, na.rm=T)
#aggregate : 그룹에 대한 통계 계산 할떄 사용하는 함수
# 대상~기준
#ex)졸업연도가 기준일 경우 나이(평균)에 대한 계산
# 데이터프레임으로 출력됨
help(aggregate)

ave_age<-ave(teens$age, teens$gradyear, FUN=function(x) mean(x, na.rm=TRUE))
class(ave_age)
# ave : 길이가 동일한 벡터로 출력

teens$age<-ifelse(is.na(teens$age),ave_age,teens$age)
summary(teens$age)

interests<-teens[5:40]
set.seed(2345) 
# 처음 클러스터링할때 center 지점을 random하게 배치함, 이것때문에 seed를 지정한 것


interests_z<-as.data.frame(lapply(interests, scale))
head(interests_z)
library(stats)

teen_clusters<-kmeans(interests_z, 5)

teen_clusters$size
# 그룹에 속하는 데이터의 갯수

teen_clusters$centers
# centers가 중요
# 1,2,3,4,5는 클러스터
# 스치는 centroid point의 좌표

teen_clusters$cluster
# 각각의 학생들이 어디 cluster에 포함되는지

teens$cluster<-teen_clusters$cluster
teens[1:5,c("cluster","gender","age","friends")]


#클러스터 단위로 나이 평균?
aggregate(data=teens, female~cluster, mean)


table(iris$Species, useNA="ifany") # NA 여부 확인 -> 없음
iris_label<-iris[5]
iris_c<-iris[-5] 
summary(iris$Sepal.Length, useNA="ifany")

iris_z<-as.data.frame(lapply(iris_c, scale))
iris_clusters<-kmeans(iris_z,3)
iris_clusters$size
iris_clusters$centers
iris$cluster<-iris_clusters$cluster
iris$cluster
iris[1:5,c('Species','cluster','Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')]
aggregate(data=iris, Sepal.Length~cluster, mean)
aggregate(data=iris, Sepal.Length~Species, mean)


iris$cluster_name<-ifelse(iris$cluster==1, 'versicolor',ifelse(iris$cluster==2,'setosa','virginica'))

head(iris)

CrossTable(x=iris$Species,
           y=iris$cluster_name,
           prop.chisq=FALSE)
# 확인결과 setosa는 잘 분류하지만
# versicolor, virginica는 제대로 분류하지 못하고 있음



CrossTable(x=wbcd_test_labels,  # 정답 데이터
           y=wbcd_test_pred,# 추측한 결과 데이터
           prop.chisq=FALSE)


curve(-x*log2(x)-(1-x)*log2(1-x),
      col="red",xlab="x",ylab="entropy",lwd=4)


