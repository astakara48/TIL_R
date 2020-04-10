install.packages("arules")
library(arules)
groceries<-read.transactions("Data/groceries.csv",sep=",") # 데이터 개수가 들쑥말쑥할때는 read.transactions
summary(groceries)
#169:거래내역에서 전체 상품의 종류의 갯수
#1~32:거래당 구매상품의 갯수
#ex) sizes에서 1이면 => 2159건이 물건 1개만 구매했다.

inspect(groceries[1:5])

itemFrequency(groceries[,150:169]) # 아이템의 구매 비율

itemFrequencyPlot(groceries,support=0.1) # support = 0.1 지지도가 0.1 이상인 상품만 출력

itemFrequencyPlot(groceries, topN=20) # topN=20 상위 20개 출력
image(groceries[1:5])
image(sample(groceries,100)) #sample => sampling하는 함수 // 임의의 100개의 데이터를 뽑아서 출력력
# apriori(groceries)
groceryRules<-apriori(groceries,parameter = list(support=0.006, confidence=0.25, minlen=2)) 
#supoort=0.006 => 지지도가 0.006, confidence=0.25 => 신뢰도가 0.25
# minlen = 2개 미만의 아이템을 갖는 규칙은 제거하겠다.

summary(groceryRules) #lift max에 집중

inspect(sort(groceryRules, by='lift'))
# 3.956477의 의미는 대략 4
# 허브를 산 사람들이 채소를 살 가능성이 // 채소를 산 일반고객보다 4배가 더 높다.

berryRules<-subset(groceryRules, items %in% c("berries","yogurt")) # berries가 들어간 규칙은 전부 찾아라
inspect(berryRules)

write(groceryRules, file='groceryRules.csv',sep=",")
grdf<-as(groceryRules,"data.frame")
str(grdf)
help(Epub)

data(Epub) # load

summary(epub, parameter=list(support(0.00)))



# 위에서 했던거 전부 해보기 // sort할떄 by를 다 해보기
inspect(Epub[1:10])
epub<-Epub # 936 columns
summary(epub)
itemFrequencyPlot(epub,support=0.1) # 지지도가 0.1을 넘는게 없음 
itemFrequencyPlot(epub,support=0.01)
itemFrequencyPlot(epub, topN=20)
image(sample(epub,600))
epubRules<-apriori(epub, parameter=list(support=0.0005, confidence=0.18, minlen=2))
summary(epubRules)

inspect(sort(epubRules, by='confidence'))

epubdf<-subset(epubRules, items %in% c("doc_971","doc_c69"))
str(epubdf)
epub_df<-as(epubdf,"data.frame")
write(epubdf, file='epubRules.csv',sep=",")



# 아이리스 data (3개 그룹으로 나누어보기)
# 3개 그룹(정확도...?)
iris[5] # label
iris_c<-iris



