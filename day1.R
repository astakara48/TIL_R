a<-1 # a에 1할당
a
b<-2
(a+b)/2

v1<-c(1,2,5,8,9)
v1

v2<-c(1:5)
v2

v3<-seq(1,5)
v3
v4<-seq(1,10,by=3)
v4
v4+1

s1<-"a"
s2<-"text"
s3<-"hi"
s4<-c(s1,s2,s3)
s4
s4+1

mean(v1)
max(v1)
min(v1)

s4
paste(s4,collapse=",")

install.packages("ggplot2")
library(ggplot2)
x<-c("a","a","b","c")
qplot(x) # 빈도그래프

mpg
qplot(data=mpg, x=hwy)

qplot(data=mpg, x=drv, y=hwy)

qplot(data=mpg, x=drv, y=hwy, geom='line')
qplot(data=mpg, x=drv, y=hwy, geom='boxplot')
qplot(data=mpg, x=drv, y=hwy, geom='boxplot', color=drv)
?qplot

eng<-c(90,80,60,70)
math<-c(50,10,20,90)

df_mid=data.frame(eng, math)
df_mid
str(df_mid)
class<-c(1,1,2,2)
df_mid=data.frame(eng, math, class)
df_mid

mean(df_mid$eng)

df<-data.frame(eng=c(90,80,60,70), math=c(50,10,20,90), class=c(1,1,2,2))
df

install.packages("readxl")
library(readxl)

df<-read_excel("Data/excel_exam.xlsx")
df
df$english

novar_df<-read_excel("Data/excel_exam_novar.xlsx", col_names = F)
novar_df

df<-read.csv("Data/csv_exam.csv")
df

str(df)

write.csv(df, file="mydf.csv")

exam<-read.csv("Data/csv_exam.csv")
head(exam,10)
tail(exam,10)

View(exam)
dim(exam)
str(exam)
summary(exam)

str(mpg)
head(mpg)
View(mpg)
summary(mpg)

df<-data.frame(v1=c(1,2,1), v2=c(2,3,2))
df
install.packages("dplyr")
library(dplyr)
?rename
df<-rename(df, var1=v1)
df

df$v_sum<-df$var1+df$v2
df

str(mpg)

# total 컬럼추가 = hwy+cty 평균
mpg$total<-(mpg$hwy+mpg$cty)/2
mpg

summary(mpg$total)
mpg

mpg<-as.data.frame(mpg)
mpg
mpg$test<-ifelse(mpg$total>=20,"pass","fail")
head(mpg,20)
table(mpg$test)
qplot(mpg$test)

mpg$grade<-ifelse(mpg$total>=30,"A",ifelse(mpg$total>=20,"B","C"))
mpg
table(mpg$grade)
qplot(mpg$grade)


# 1+2 %>% *5 %>% -10 %>% +2 %>% 7  #파이프라인기호 or 필터기호호

exam<-read.csv("Data/csv_exam.csv")
exam
# exam에서 class 1인것만 추출
exam %>% filter(class!=1) %>% filter(math>=50)

# 2반이면서 영어점수가 80점 이상인 데이터 추출
exam %>% filter(class==2) %>% filter(english>=80)
exam %>% filter(class==2 & english>=80)
exam %>% filter(class==2 | english>=80)
exam %>% filter(class==1 | class==3 | class==5)
exam %>% filter(class %in% c(1,3,5)) # 벡터(1,3,5) 안에 있으면

exam %>% select(math)
exam %>% select(math, class)
exam %>% select(-math)

# class가 1인 행에 대해 english를 추출
exam %>% filter(class==1) %>% select(english)

#id, math 추출 앞부분 6행까지 추출
exam %>% select(english, math) %>% head(6)

exam %>% arrange(math)
exam %>% arrange(desc(math))

exam %>% arrange(class, desc(math))

exam %>% mutate(total=math+english+science) %>% head

#science가 60점 이상이면 pass, 미만이면 fail test열 추가
exam %>% mutate(test=ifelse(exam$science>=60,"pass","fail"))

#total=math+english+science
#total 오름차순 정렬, 상위 10개 출력
exam %>% mutate(total=math+english+science) %>% arrange(total) %>% head(10)
