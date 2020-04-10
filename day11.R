# TF-IDF = TF*IDF
# TF : 문서에 등장한 단어의 개수
# IDF : N/(1+DF)
# DF : 단어가 등장한 문서의 개수
# TF-IDF 값이 커지기 위해서는 TF가 커지면서 IDF가 커져야 함
# IDF가 커지기 위해서는 N이 커지거나 DF가 작아져야 한다.

# R 웹 스크래핑 = rvest
install.packages("rvest")
library(rvest)
library(dplyr) # == require(패키지명)

# read_html()
# html_node() / html_nodes(),
# html_text()

url_tvcast="https://tv.naver.com/jtbc.youth"
html_tvcast<-read_html(url_tvcast, encoding='utf-8')
html_tvcast

# class가 title인 부분 안에 있는 a태그의 태용을 추출
tvcast_res<-html_tvcast %>% html_nodes(".title a") %>% html_text() 
# html_node() : 매칭된 요소 하나 추출(id 검색)
# html_nodes() : 모든 요소 추출(class, tag)

class(tvcast_res)

tvcast_df<-html_tvcast %>% html_nodes(".title a") %>% html_text() %>% data.frame()
class(tvcast_df)

# https://en.wikipedia.org/wiki/Student%27s_t-distribution
url_t="https://en.wikipedia.org/wiki/Student%27s_t-distribution"
html_t<-read_html(url_t, encoding='utf-8')
html_t

html_t %>% html_nodes(".wikitable") %>% html_table()

# html_text() : 텍스트 추출
# html_name() : attribute 명을 추출
# html_childeren() : 하위 요소 추출
# html_tag() : 태그명 추출
# html_attrs() : 속성을 추출

library(stringr)
library(tm)
library(KoNLP)
library(httpuv)
library(rgdal)
library(geojsonio)
library(rgeos)
library(Sejong)
library(tau)
library(hash)
library(RSQLite)

my.text.location<-"c:/jsy/논문data/ymbaek_논문"
mypaper<-VCorpus(DirSource(my.text.location))
mypaper
mykorean<-mypaper[[19]]$content
my_text<-str_replace_all(mykorean,"[[:lower:]]{1,}","")
my_text<-str_replace_all(my_text, "\\(|\\)","")
my_text<-str_replace_all(my_text, "‘","")
my_text<-str_replace_all(my_text, "’","")
my_text<-str_replace_all(my_text, " · ", " ")

noun.mytext<-extractNoun(my_text)
noun.mytext
table(noun.mytext)

# 참여
# 참석    -> 참가 
# 참가

# DTM => TFIDF 구성

# tf는 높고 idf가 낮은 단어 추출



# 연습문제
# 네이버, 외국포털, 다음, 언론사, ....
# 기사 추출 -> tfidf 구성 -> 상관계수 출력
# 기사 키워드 추출
# ex) 경제신문 기사 -> 오늘(문서), 어제(문서), 그제, ...... doc1~dc10
# corpus 생성 -> dtm -> tfidf -> 상관계수
# https://edition.cnn.com/search?size=10&q=parasite&from=10&page=2
fomos<-"http://www.fomos.kr"
fomos_url<-"http://www.fomos.kr/esports/news_list?news_cate_id=13&theme_cate_id=&page=1"
fomos_list<-read_html(fomos_url, encoding='utf-8')
fomos_article_list<-fomos_list %>% html_nodes("div") %>% html_nodes(".para") %>% html_nodes("a") %>% html_attr("href")


for(i in 1: length(fomos_article_list)){
  fomos_href<-fomos_article_list[i]
  test_url<-paste(fomos,fomos_href,sep="")
  test_list<-read_html(test_url,encoding="utf-8")
  fomos_article_body<-test_list %>% html_nodes("div") %>% html_nodes(".view_area") %>% html_nodes('.view_text') %>% html_text()
  data_url<-"c:/jsy/fomos/"
  file_name<-"fomos_article"
  save_file<-paste(paste(paste(data_url,file_name,sep=''),i,sep=''),".txt",sep='')
  write.table(fomos_article_body, save_file,
              sep = ",",row.names = FALSE,quote = FALSE,append = TRUE, na = "NA") 
}
my_article_location<-"c:/jsy/fomos"
my_article_corpus<-VCorpus(DirSource(my_article_location))
for(i in 1:length(my_article_corpus)){
  my_test<-my_article_corpus[[i]]$content
  my_test<-str_replace_all(my_test,"<|>","")
  my_test<-str_replace_all(my_test,"[[:digit:]]{1,}","")
  my_test<-str_replace_all(my_test,"[[:upper:]]{1,}","")
  my_test<-str_replace_all(my_test,"\\+","")
  my_test<-str_replace_all(my_test,"[\t]","")
  my_test<-str_replace_all(my_test, "\\(|\\)","")
  my_test<-str_replace_all(my_test,"[[:lower:]]{1,}","")
  my_test<-str_replace_all(my_test,"[[:upper:]]{1,}","")
  my_test<-str_replace_all(my_test,"@","")
  my_test<-str_replace_all(my_test,"\\.{1,}","")
  my_test<-str_replace_all(my_test,"[[/]]{1,}","")
  my_test<-str_replace_all(my_test,",","")
  my_test<-str_replace_all(my_test,"\"","")
  my_test<-str_replace_all(my_test,":","")
  my_test<-str_replace_all(my_test,"\"","")
  my_test<-str_replace_all(my_test,"!","")
  my_test<-str_replace_all(my_test,"'","")
  my_test<-str_replace_all(my_test,"\"","")
  my_test<-str_replace_all(my_test,"\"","")
  my_test<-str_replace_all(my_test,"\n","")
  my_test<-str_replace_all(my_test, "‘","")
  my_test<-str_replace_all(my_test, "’","")
  abd<-""
  for(j in 1:length(my_test)){
    abd<-paste(abd,my_test[j])
  }
  my_test<-str_replace_all(abd,"[[:space:]]{2,}"," ")
  abd<-""
  my_article_corpus[[i]]$content<-my_test
}
extractNoun(my_article_corpus[[1]]$content)

dtm.e<-DocumentTermMatrix(my_article_corpus)
dtm.e$dimnames
rownames(dtm.e[,])
# 단어
colnames(dtm.e[,])
dim(dtm.e)
inspect(dtm.e[1:8,])

2+6+6+2+6+2+6+5+7+2