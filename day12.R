install.packages("psych")
library(psych)

insurance<-read.csv("Data/insurance.csv")
str(insurance)
# 회귀모델을 구축하기 전에 정규성 확인
# 종속변수가 정규분포를 따르는 경우 모델이 잘 만들어 진다.
summary(insurance)
hist(insurance$expenses)
str(insurance)

table(insurance$region)

# 변수 상관 관계(상관 행렬, cor함수 사용)
cor(insurance[c("age","bmi",'children','expenses')])

pairs(insurance[c("age","bmi",'children','expenses')])
pairs.panels(insurance[c("age","bmi",'children','expenses')])

# 모델 생성
ins_model<-lm(expenses~age+children+bmi+sex+smoker+region, data=insurance)
ins_model<-lm(expenses~., data=insurance) # 1~ . 1제외하고 전부
ins_model

summary(ins_model)
#  Residuals:
#    Min       1Q   Median       3Q      Max 
#  -11302.7  -2850.9   -979.6   1383.9  29981.7 
#  
#    Coefficients:
#                 Estimate Std.    Error  t value   Pr(>|t|)    
#    (Intercept)     -11941.6      987.8 -12.089  < 2e-16 ***
#    age                256.8       11.9  21.586  < 2e-16 ***
#    sexmale           -131.3      332.9  -0.395 0.693255    
#    bmi                339.3       28.6  11.864  < 2e-16 ***
#    children           475.7      137.8   3.452 0.000574 ***
#    smokeryes        23847.5      413.1  57.723  < 2e-16 ***
#    regionnorthwest   -352.8      476.3  -0.741 0.458976    
#    regionsoutheast  -1035.6      478.7  -2.163 0.030685 *  
#    regionsouthwest   -959.3      477.9  -2.007 0.044921 *  
# p값이 작다 -> 회귀 계수가 0이 아닐 가능성이 높다 => 종속변수 값을 정하는데 영향을 많이 준다.
# R-squared(결정계수, r제곱값)
# 모델이 종속변수 값을 얼마나 잘 설명하는가?
# R-squared = 0.7509 => 75% 설명하고 있다. // 1에 가까울수록 잘 설명하고 있다.
# 독립/종속 변수 : 선형이라고 가정

# 비선형 관계 : 높은 차수 항을 모델 추가
insurance$age2<-insurance$age^2 # age에 ^2값 출력력
#lm(expense~age+age2)

insurance$bmi30<-ifelse(insurance$bmi>=30,1,0)

insurance_model2<-lm(expenses~age+age2+children+bmi+sex+bmi30*smoker+region, data=insurance)
summary(insurance_model2)
install.packages("xlsx")
install.packages("gdata")
library(xlsx)
library(gdata)
air_0.2_.xlsx
raw_data<-read_excel("air_0.2_.xlsx")
str(raw_data)
summary(raw_data)
raw_data$Date[1]
lm()

