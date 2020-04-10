exam<-read.csv("Data/csv_exam.csv")
exam
library(dplyr)
library(ggplot2)

exam %>% summarise(mean_math=sum(math))
exam
exam %>% 
  group_by(class) %>% 
  summarise(mean_math=mean(math))

exam %>% 
  group_by(class) %>% 
  summarise(mm=mean(math),
            sm=sum(math),
            md=median(math),
            cnt=n())


mpg %>% 
  group_by(manufacturer,drv) %>% 
  summarise(mc=mean(cty)) %>% 
  head(10)

# mpg 데이터를 회사별로 그룹화
# class가 suv 추출
mpg %>% 
  group_by(manufacturer) %>% 
  filter(class=='suv') %>% 
  mutate(tot=(cty+hwy)/2) %>% 
  summarise(mt=mean(tot)) %>% 
  arrange(desc(mt)) %>% 
  head(5)
#tot=cty와 hwy의 평균값

test1<-data.frame(id=c(1,2,3,4,5),
                  midterm=c(60,80,70,90,55))
test2<-data.frame(id=c(1,2,3,4,5),
                  final=c(70,80,40,80,75))
test1
total<-left_join(test1,test2,by="id")
total
exam

name<-data.frame(class=c(1,2,3,4,5),
                 teacher=c("kim",'lee','park','choi','cho'))
exam_new<-left_join(exam,name,by="class")
exam_new
total
test1<-data.frame(id=c(1,2,3,4,5),
                  midterm=c(60,80,70,90,55))
test2<-data.frame(id=c(6,7,8,9,10),
                  midterm=c(70,80,40,80,75))
ta<-bind_rows(test1,test2)
ta

exam %>% filter(english>=80)
exam %>% filter(class==1 & math>=50)
exam %>% filter(class %in% c(1,3,5))

exam %>% 
  select(id,math)

# test컬럼 추가
# english>=60 pass, fail
exam %>% mutate(test=ifelse(english>=60,"pass","fail")) %>% 
  arrange(test)

test1
test2
test1<-data.frame(id=c(1,2,3,4,5),
                  midterm=c(60,80,70,90,55))
test2<-data.frame(id=c(1,2,3,4,5),
                  final=c(70,80,40,80,75))
left_join(test1,test2,by='id')

df<-data.frame(sex=c("M","F",NA,"M","F"),
           score=c(5,4,3,5,NA))
df
is.na(df)
table(is.na(df$score))
mean(df$score)
sum(df$sum)

df %>% filter(is.na(score))
# score가 NA인 데이터만 출력
df_nomiss<-df %>% filter(!is.na(score))
df_nomiss
mean(df_nomiss$score)

df_nomiss<-df %>% filter(!is.na(sex)&!is.na(score))
df_nomiss
df_nomiss2<-na.omit(df)
df_nomiss2

mean(df$score,na.rm=T)
sum(df$score,na.rm=T)



exam=read.csv("Data/csv_exam.csv")
exam

exam[c(3,8,15),"math"]<-NA
exam

exam %>% summarise(mm=mean(math,na.rm=T),
                   sm=sum(math,na.rm=T),
                   med=median(math,na.rm=T))

#대체
exam$math<-ifelse(is.na(exam$math),55,exam$math) #math열 값이 NA이면 55를 대입, 아니면 그대로 저장
table(is.na(exam$math))
mean(exam$math)

df<-data.frame(sex=c(1,2,1,3,2,1),
               score=c(5,4,3,4,2,6))
table(df$sex)
table(df$score)

df$sex<-ifelse(df$sex==3,NA,df$sex)
df$score<-ifelse(df$score>=5,NA,df$score)

df %>%
  filter(!is.na(sex)&!is.na(score)) %>% 
  group_by(sex) %>% 
  summarise(ms=mean(score))

View(mpg$hwy)
boxplot(mpg$hwy)
boxplot(mpg$hwy)$stats
median(mpg$hwy)

mpg$hwy<-ifelse(mpg$hwy<12|mpg$hwy>37,NA,mpg$hwy)
table(is.na(mpg$hwy))

#drv를 기준으로 그룹화
#mean_hwy<-hwy의 평균, 글측값은 제외
mpg %>% group_by(drv) %>% summarise(mean_hwy=mean(hwy,na.rm=T))

table(is.na(df$score))







ggplot(data=mpg, aes(x=displ,y=hwy))

ggplot(data=mpg, aes(x=displ,y=hwy))+ # 배경을 설정
  geom_point()+ # 데이터를 어떻게 표현할지?
  xlim(3,6)+ # x축의 범위를 3,6으로
  ylim(10,30)


df_mpg<-mpg %>% group_by(drv) %>% 
  summarise(mean_hwy=(mean(hwy)))
df_mpg
ggplot(data=df_mpg,aes(x=drv,y=mean_hwy))+
  geom_col()

economics
ggplot(data=economics,aes(x=date,y=unemploy))+
  geom_line()



install.packages("foreign")
library(foreign) #spss 파일로드
library(dplyr) #전처리
library(readxl) #엑셀파일
library(ggplot2) # 시각화 

raw_welfare<-read.spss(file="Data/Koweps_hpc10_2015_beta1.sav",to.data.frame=T)
welfare<-raw_welfare

str(welfare)

View(welfare)
dim(welfare)
str(welfare)
summary(welfare)

welfare<-rename(welfare,
       sex=h10_g3,
       birth=h10_g4,
       marrige=h10_g10,
       religion=h10_g11, #종교
       code_job=h10_eco9, #직종
       income=p1002_8aq1, #임금
       code_region=h10_reg7) #지역코드
class(welfare$sex) #python에서 type
table(welfare$sex)

# 이상치 결측값 처리
welfare$sex=ifelse(welfare$sex==9,NA,welfare$sex)

table(is.na(welfare$sex))

welfare$sex<-ifelse(welfare$sex==1,"male","female")
# 1이면 "male", 2면 female
table(welfare$sex)
qplot(welfare$sex)

class(welfare$income)
summary(welfare$income)
qplot(welfare$income)+xlim(0,1000)

summary(welfare$income)

welfare$income<=ifelse(welfare$income %in% c(0,9999),NA,welfare$income)
table(is.na(welfare$income))
sex_income<-welfare %>%
  filter(!is.na(income)) %>%
  group_by(sex) %>% 
  summarize(mi=mean(income))
sex_income
ggplot(data=sex_income, aes(x=sex, y=mi))+geom_col()

summary(welfare$birth)
table(is.na(welfare$birth))

#9999 => NA
welfare$birth<-ifelse(welfare$birth==9999,NA,welfare$birth)
table(welfare$birth)
welfare$age<-2015-welfare$birth+1
summary(welfare$age)
qplot(welfare$age)

age_income<-welfare %>%
  filter(!is.na(income))%>%
  group_by(age) %>% 
  summarise(mi=mean(income))
head(age_income)
ggplot(data=age_income, 
       aes(x=age, y=mi))+geom_line()

welfare<-welfare %>% mutate(ageg=ifelse(age<30,"young",ifelse(age<=59,"middle","old")))
welfare$ageg
qplot(welfare$ageg)

# .연령대별(초년, 중년, 장년)월 수입평균 출력
ageg_income<-welfare %>% 
  filter(!is.na(welfare$income)) %>%
  group_by(ageg) %>%
  summarize(mi=mean(income))
ageg_income
ggplot(data=ageg_income, aes(x=ageg, y=mi))+geom_col()

ggplot(data=ageg_income, aes(x=ageg, y=mi))+geom_col()+scale_x_discrete(limits=c("young",'middle','old'))
# scale_x_dexrete(limits=c('')) 그래프배열 순서 지정

# 성별로 월급차이는 연령대별로 다를까?

sex_income<-welfare %>% filter(!is.na(welfare$income)) %>% group_by(ageg,sex) %>% summarize(mi=mean(income))
sex_income
ggplot(data=sex_income, aes(x=ageg,y=mi,fill=sex))+geom_col()+scale_x_discrete(limits=c("young","middle",'old'))

ggplot(data=sex_income, aes(x=ageg,y=mi,fill=sex))+geom_col(position = "dodge")+scale_x_discrete(limits=c("young","middle",'old'))

# 성별,연령별 월급에 대한 평균표
sex_age<-welfare %>% filter(!is.na(welfare$income)) %>% group_by(age,sex) %>% summarize(mi=mean(income))
ggplot(data=sex_age,aes(x=age,y=mi,col=sex))+geom_line()

#직업 코드별 인원수 확인
table(welfare$code_job)

welfare %>% filter(!is.na(welfare$code_job)) %>% group_by(code_job) %>% summarize(cnt=n())

library(readxl)
list_job<-read_excel("Data/Koweps_COdebook.xlsx",sheet=2,col_names=T)
list_job

welfare<-left_join(welfare,list_job,id="code_job")
welfare$job
welfare$code_job


#welfare에서 code_job이 na가 아닌 데이터에 대해서 code_job, job 열을 추출
welfare %>% filter(!is.na(welfare$code_job)) %>% select(code_job,job) %>% head(20)


#직업별로 월급에 대해서 평균 구하기 + 정렬
job_income<-welfare %>% 
  filter(!is.na(job)&!is.na(income)) %>% 
  group_by(job) %>% 
  summarize(mi=mean(income))

top10<-job_income %>% arrange(desc(mi)) %>% head(10)

ggplot(data=top10,aes(x=job,y=mi))+geom_col()+coord_flip()

ggplot(data=top10,aes(x=reorder(job,-mi),y=mi))+geom_col()+coord_flip()
#coord_flip() : 축을 바꾸는 함수



# 문제2번 성별에 따라 어떤 직업이 가장 많은지 조사
welfare<-left_join(welfare,list_job,id="code_job")
welfare$job
welfare$sex

sex_job<-welfare %>% 
  filter(!is.na(job)) %>% 
  group_by(sex,job) %>% summarize(cnt=n())

#남녀 통합 상위 10개 직업
sex_job %>% arrange(desc(cnt)) %>% head(10)

#남자 상위 10개 직업
male_job<-sex_job %>% filter(sex=='male')
male_job %>% arrange(desc(cnt)) %>% head(10)

#여자 상위 10개 직업
female_job<-sex_job %>% filter(sex=='female')
female_job %>% arrange(desc(cnt)) %>% head(10)

