install.packages("stringr")
library(stringr)
rwiki<-"R is a programming language and software environment for statistical computing and graphics supported by the R Foundation for Statistical Computing. The R language is widely used among statisticians and data miners for developing statistical software and data analysis. Polls, surveys of data miners, and studies of scholarly literature databases show that R's popularity has increased substantially in recent years.
R is a GNU package. The source code for the R software environment is written primarily in C, Fortran, and R. R is freely available under the GNU General Public License, and pre-compiled binary versions are provided for various operating systems. While R has a command line interface, there are several graphical front-ends available."
str_extract(rwiki,"software environment")
#regexpr(), regmatches() 와 유사함
str_extract_all(rwiki,"software environment")
#str_extract_all 전체를 검색
str_extract_all(rwiki,"software environment",simplify=TRUE)

str_extract_all(rwiki,"[[:upper:]]{1}")
# [[:upper:]]{1}:대문자가 한번만 등장하는 경우를 검색
str_extract_all(rwiki,"[[:alpha:]]{0,}")
# [[:alpha:]]{0,}:알파벳 문자 0개 이상
myextract<-str_extract_all(rwiki,"[[:upper:]]{1}[[:alpha:]]{0,}")
# 첫번째 문자가 대문자로 시작하는 단어 출력
table(myextract)

str_locate_all(rwiki,"software environment")
#base: regexpr, gregexpr
#list:

#첫번째 글자가 대문자로 시작하는 단어들의 위치를 모두 조사
mylocate<-str_locate_all(rwiki,"[[:upper:]]{1}[[:alpha:]]{0,}")
dim(mylocate[[1]])

mydata<-data.frame(mylocate[[1]])
mydata
mydata$myword<-myextract[[1]]
mydata
#myword.length 열 추가 // myword의 길이
mydata$myword.length<-mydata$end-mydata$start+1
mydata

temp<-str_replace_all(rwiki,"software environment","software_environment")
str_extract_all(temp,"software_environment|software|environment")
table(str_extract_all(rwiki,"software_environment|software|environment"))

rwiki
# R, C // R->R_computer.language_로 변경
temp<-str_replace_all(rwiki,"R","R_computer.language")
temp<-str_replace_all(temp,"C","C_computer.language")
temp
# temp에서 _computer.language_ 표현이 붙은 부분에는 어떤 단어들이 있고, 빈도가 어떤지?
table(str_extract_all(temp,"[[:alpha:]]{1}_computer.language"))

#텍스트 데이터의 문단을 구분(줄바꿈 문자)
rwikipara<-str_split(rwiki,"\n")
rwikipara

#문단별로 문장을 구분(.)
rwikisent<-str_split(rwikipara[[1]],'\\. ') #\\. 을 해야 구두점으로 인식함

#str_split_fixed 함수
my2sentences<-unlist(rwikisent)[c(4,7)]
my2sentences[1]

#각 문장의 단어수를 출력
mylength1<-str_count(my2sentences[1],"[[:alpha:]]")
mylength2<-str_count(my2sentences[2],"[[:alpha:]]")
mylength1
mylength2

mylength1<-length(unlist(str_split(my2sentences[1]," ")))
mylength2<-length(unlist(str_split(my2sentences[2]," ")))

#str_split_fixed : 문장을 지정한 길이로 잘라주는 함수
myfixed.short<-str_split_fixed(my2sentences," ",5)
myfixed.long<-str_split_fixed(my2sentences," ",13)
myfixed.long

# rwikisent 문장*단어 형태로 
length.sentences<-rep(NA,length(unlist(rwikisent)))

for(i in 1:length(length.sentences)){
  length.sentences[i]<-length(unlist(str_split(unlist(rwikisent)[i]," ")))
}
length.sentences

max.length.sentences<-max(length.sentences)

sent.word.matrix<-str_split_fixed(unlist(rwikisent)," ",max.length.sentences)

sent.word.matrix
mydata<-data.frame(sent.word.matrix)
mydata

rownames(mydata) #sent.1 sent.2... sent.7
colnames(mydata) #word.1....word.21
paste("abc",1:5,sep=".")

rownames(mydata)<-paste("sent",1:length(unlist(rwikisent)),sep=".")
colnames(mydata)<-paste("word",1:max.legnth.sentences,sep=".")

mydata[,1]       

#R
str_count(rwiki,"R")
str_count(unlist(rwikipara),"R")
str_count(rwikipara[[1]],"R")
rwikisent          

# R 이라는 단어가 등장한 후에 stat으로 시작하는 단어가 등장하는 빈도를 조사
unlist(rwikisent)
#모든문자 : .

str_count(unlist(rwikisent),"R.{1,}stat[[:lower:]]{1,}") # R로 시작하는게 1개 이상 나오며 stat이 있는 경우

#s, S구분이 필요 없는 경우
str_count(unlist(rwikisent),"R.{1,}(s|S)tat[[:lower:]]{1,}")

str_extract_all(unlist(rwikisent[1]),"R.{1,}(s|S)tat[[:alpha:]]{1,}")

# R와 stat 사이에 R이라는 표현이 있으면 안된다는 걸 표현
str_count(unlist(rwikisent),"R[[:lower:][A-Q][S-Z][:digit:][:space:]]{1,}(s|S)tat[[:alpha:]]{1,}")
# [^R] R 제외하고

str_count(unlist(rwikisent),"R{1}[^R]{1,}(s|S)tat[[:alpha:]]{1,}")

#substr(), str_sub()
str_sub(unlist(rwikisent[1]),1,30)

str_dup("software",3) # 문자열이 1개로 나옴
rep("software",3)
str_dup("software",3)==paste(rep("software",3),collapse="")

rwikisent
str_length(unlist(rwikisent))
#str_length : 각각의 벡터열에 몇개의 글자수가 있는지 세는 함수
nchar(unlist(rwikisent))

name<-c("Joe","Jack","Jackie","Jefferson")
donation<-c("$1","$111","11111","1111111")
mydata<-data.frame(name,donation)
mydata

name2<-str_pad(mydata$name, width=15, side="right")
# 공백 문자의 side = right로 설정
donation2<-str_pad(mydata$name, width=15, side="both", pad="~")
name2
donation2
mydata2<-data.frame(name2,donation2)
mydata2

str_length(mydata2$name2)

#패딩된 공백문자를 제거
name3<-str_trim(mydata2$name2, side="right")
name3

#양쪽에 패딩(~)기호를 모두 제거
str_trim(mydata2$donation2, side="both")
str_remove_all(mydata2$donation2,"~")

donation3<-str_trim(str_replace_all(mydata2$donation2,"~"," "))
mydata3<-data.frame(name3, donation3)
mydata3
all(mydata3==mydata)


mytext<-c("software environment",
  "software  environment",
  "software\tenvironment")
mytext
#white space(공란) 제거
sapply(str_split(mytext," "),length)
# sapply는 return값이 vector
lapply(str_split(mytext," "),length)

mytext.nowhitespace<-str_replace_all(mytext,"[[:space:]]{1,}"," ")
mytext.nowhitespace

sapply(str_split(mytext.nowhitespace," "),length)
lapply(str_split(mytext.nowhitespace," "),length)

mytext<-"The 45th President of the United States, Donald Trump, states that he knows how to play trump with the former president"

myword<-unlist(str_extract_all(mytext,boundary("word")))
table(myword)
table(tolower(myword))

myword<-str_replace(myword, "Trump","Trump_unique_")
myword<-str_replace(myword, "States","States_unique_")
table(myword)

mytext<-c("He is one of statisticians agreeing that R is the No. 1 statistical software.","He is one of statisticians agreeing that R is the No. one statistical software.")
mytext
mytext2<-str_split(str_replace_all(mytext,"[[:digit:]]{1,}[[:space:]]{1,}","")," ")
str_c(mytext2[[1]],collapse = " ")
str_c(mytext2[[2]],collapse = " ")

#숫자 자료임을 표시
mytext3<-str_split(str_replace_all(mytext,"[[:digit:]]{1,}[[:space:]]{1,}","_number_")," ")
mytext3

mytext<-"Kim et al. (2020) argued that the state of"
str_split(mytext,"\\. ")
# 성 et al. (년도) => 하나의 단어로 교체
#                  => _reference_

mytext<-c("She is an actor", "She is the actor")
# a an the 불용어 처리
mystopword<-"(\\ban )|(\\bthe )"
str_replace_all(mytext,mystopword,"")

library(tm)
stopwords("en")
stopwords("smart")

mytext<-c("I am a boy. You are a boy. He might be a boy.")
mystemmer.func<-function(mytextobj){
  #am, are, is, was, were, be => be
  str_replace_all(mytextobj,"(\\bam)|(\\bare)|(\\bis)|(\\bwas)|(\\bwere)|(\\be)","be")
}
mytext.stem<-mystemmer.func(mytext)
mytext.stem

"오늘 강남에서 맛있는 스파게티를 먹었다."
"강남에서 먹었던 오늘의 스파게티는 맛있었다."
# 글자 n-gram 기반 유사도
# n=2
# 오늘, 늘 , 강, ..., 다.
# 강남, 남에, 에서, .... 다.
install.packages("tidytext")
library(tidytext)
install.packages("tidyr")
library(tidyr)
install.packages("textdata")
library(textdata)
# 감성어휘 사전 -> 감성분석

# get_sentiments("afinn") -5~+5
get_sentiments("bing")

mynrc<-data.frame(get_sentiments("nrc"))
mynrc
table(mynrc$sentiment)



# n=2 / n=3 유사도 출력
# 레벤슈타인 거리
# 감성사전 기반 감정 분석 => 임의의 영문장 입력 => 감정이 나오게 출력
s1<-"오늘 강남에서 맛있는 스파게티를 먹었다."
s2<-"강남에서 먹었던 오늘의 스파게티는 맛있었다."

w1 <- strsplit(s1, "", fixed = TRUE)
w2 <- strsplit(s2, "", fixed = TRUE)

v1<-c()
v2<-c()

v1
for(i in 1:length(w1[[1]])){
  #paste(w1[[1]][(i):(i+1)],collapse = "")
  v1<-c(v1,paste(w1[[1]][i:(i+1)],collapse = ""))
}

v1
for(i in 1:length(w2[[1]])){
  #paste(w1[[1]][(i):(i+1)],collapse = "")
  v2<-c(v2,paste(w2[[1]][i:(i+1)],collapse = ""))
}
v2
length(v1)
v1<-c(v1[1:length(v1)-1])
v1
length(v2)
v2<-c(v2[1:length(v2)-1])
v2
cnt<-0
for(i in 1:length(v1)){
  for(j in 1:length(v2)){
    ifelse((v1[i]==v2[j])==TRUE,cnt<-cnt+1,cnt<-cnt)
  }
}

# 2n 그램일 경우 유사도
cnt/length(v2)


v1<-c()
v2<-c()

v1
for(i in 1:length(w1[[1]])){
  #paste(w1[[1]][(i):(i+1)],collapse = "")
  v1<-c(v1,paste(w1[[1]][i:(i+2)],collapse = ""))
}

v1
for(i in 1:length(w2[[1]])){
  #paste(w1[[1]][(i):(i+1)],collapse = "")
  v2<-c(v2,paste(w2[[1]][i:(i+2)],collapse = ""))
}
v1
length(v1)
v2
v1<-v1[1:(length(v1)-2)]
v1
v2<-v2[1:(length(v2)-2)]
v2
cnt<-0
for(i in 1:length(v1)){
  for(j in 1:length(v2)){
    ifelse((v1[i]==v2[j])==TRUE,cnt<-cnt+1,cnt<-cnt)
  }
}

# 3n그램 유사도
cnt/length(v2)


mynrc<-data.frame(get_sentiments("bing"))
str(mynrc) ## word / sentiment
table(mynrc$sentiment)
length(mynrc[[1]])
mynrc[[1]][1]

adele<-"Hello, it’s me 
I was wondering if after all these years you’d like to meet
To go over everything 
They say that time’s supposed to heal ya 
But I ain’t done much healing
Hello, can you hear me
I’m in California dreaming about who we used to be 
When we were younger and free
I’ve forgotten how it felt before the world fell at our feet

There’s such a difference between us 
And a million miles

Hello from the other side
I must have called a thousand times 
To tell you I’m sorry for everything that I’ve done 
But when I call you never seem to be home
Hello from the outside 
At least I can say that I’ve tried
To tell you I’m sorry for breaking your heart 
But it don’t matter it clearly doesn’t tear you apart anymore

Hello, how are you
It’s so typical of me to talk about myself I’m sorry
I hope that you’re well 
Did you ever make it out of that town where nothing ever happened

Its no secret that the both of us 
Are running out of time 

So hello from the other side
I must have called a thousand times 
To tell you I’m sorry for everything that I’ve done 
But when I call you never seem to be home
Hello from the outside 
At least I can say that I’ve tried
To tell you I’m sorry for breaking your heart 
But it don’t matter it clearly doesn’t tear you apart anymore

Highs highs highs highs
Lows lows lows lows

Highs highs highs highs
Lows lows lows lows

Highs highs highs highs
Lows lows lows lows

Highs highs highs highs
Lows lows lows lows
Hello from the other side
I must have called a thousand times 
To tell you I’m sorry for everything that I’ve done 
But when I call you never seem to be home
Hello from the outside 
At least I can say that I’ve tried
To tell you I’m sorry for breaking your heart 
But it don’t matter it clearly doesn’t tear you apart anymore"
adele<-str_replace_all(adele,"[[:space:]]{1,}"," ")
adele<-tolower(adele)
v1<-unlist(str_extract_all(adele,"[[:alpha:]]{1,}"))
negative<-0
positive<-0
v1[2]
for(i in 1:length(v1)){
  for(j in 1:length(mynrc[[1]])){
    ifelse((v1[i]==mynrc$word[[j]])==TRUE,
           ifelse(mynrc$sentiment[[j]]=='positive',positive<-positive+1,negative<-negative+1),
           0)
  }
}
positive
negative
ifelse(positive>negative,"positive","negative")
