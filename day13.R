install.packages("jsonlite")
library(jsonlite)

data<-fromJSON("02_21/sample.json")
str(data)
data<-data.frame(data)
data
names(data)<-c("id","like",'share','comment','unique','msg','time')
str(data)
dataJson<-toJSON(data)
dataJson
write(dataJson,"data.json")

# 엑셀 데이터 읽기
install.packages("readxl")
library(readxl)

cust_profile<-read_excel("02_21/cust_profile.xlsx", 
           sheet = "cust_profile",  # 어떤 시트를 읽을지?
           range = "B3:E8", # 읽을려고 하는 범위
           col_names=TRUE,
           na="NA",
           skip=2) # 맨 위에 2줄 생략

cust_profile


# txt 파일 읽기
dataset_1<-read.table("02_21/dataset_1.txt",
                      header=T,
                      sep=",",
                      stringsAsFactors = F,
                      na.strings = "")

dataset_1


# xml 데이터 읽기
install.packages("XML")
library(XML)
res<-xmlToDataFrame("02_21/test.xml")
res

### 다른 방법
res2<-xmlParse(file="02_21/test.xml")
res2
rt<-xmlRoot(res2)
rt[[1]]
rt[[1]][[2]]
