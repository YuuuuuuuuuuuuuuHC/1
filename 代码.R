catalogs <- read.csv("D:/工作/数据科学实战/个人hw4/catalogs.csv", encoding="UTF-8")
head(catalogs)

########

library(ggplot2)
catalogs$second
unique(catalogs$second)       #查看second里的种类
sum(catalogs$first == "生鲜")           ##前20524条
sum(catalogs$first == "食品饮料、保健食品")     ##后48970条
catalogs_first_1 <- catalogs[c(1:20524),]
catalogs_first_2 <- catalogs[c(20525:69494),]
ggplot(data=catalogs_first_1, mapping=aes(x="second",fill=second))+
  geom_bar(stat="count",width=0.5,position='stack')+
  coord_polar("y", start=0)+
geom_text(stat="count",aes(label = scales::percent(..count../20524)), size=4, position=position_stack(vjust = 0.5))    ###饼图1

ggplot(data=catalogs_first_2, mapping=aes(x="second",fill=second))+
  geom_bar(stat="count",width=0.5,position='stack')+
  coord_polar("y", start=0)+
  geom_text(stat="count",aes(label = scales::percent(..count../48970)), size=4, position=position_stack(vjust = 0.5))   ###饼图2

######(任务三的直方图在后面给出dtm矩阵之后再画）

library(jiebaRD)
library(jiebaR)
library(wordcloud2)
engine = worker(user = "D:/工作/数据科学实战/个人hw4/userdict.txt")
segment("D:/工作/数据科学实战/个人hw4/catalogs.csv",engine)
word <- scan("D:/工作/数据科学实战/个人hw4/catalogs.segment.2021-03-20_13_50_58.csv",sep='n',what='',encoding="UTF-8")
word <- qseg[word]
word <- freq(word)
#wordcloud(word$char,word$freq,min.freq = 10,colors = c("blue","red","grey"),scale=c(8,0.5),random.order=F)  ####用wordcloud包，不美观，就没在报告里展示    




catalogs_1 <- catalogs_first_1[,1]
catalogs_1 <- as.data.frame(catalogs_1)

write.csv(catalogs_1,file = "D:/工作/数据科学实战/个人hw4/catalogs_1.csv" )
engine = worker(user = "D:/工作/数据科学实战/个人hw4/userdict.txt")
segment("D:/工作/数据科学实战/个人hw4/catalogs_1.csv",engine)
segment_result <- read.csv("D:/工作/数据科学实战/个人hw4/catalogs_1.segment.2021-03-20_08_08_58.csv", encoding="UTF-8")

##接下来的步骤中的txt文件都是我在程序外操作的，将csv文件转变为txt文件，包括catalogs_fruit,catalogs_fruit_apple,用excel的筛选功能可以实现
##然后用wordcloud2包画了三个图

txt <- scan("D:/工作/数据科学实战/个人hw4/segment_result.txt", sep = "\n", what = "", encoding = "UTF-8")
df <- freq(engine[txt])
head(df)
library(sqldf)
sort(df$freq,decreasing = TRUE) ##top100 = 505
df <- sqldf('select * from df where freq>= 505  ')
library(wordcloud2)
wordcloud2(df)

catalogs_4_2 <- read.csv("D:/工作/数据科学实战/个人hw4/catalogs_fruit.csv", encoding="UTF-8")
segment("D:/工作/数据科学实战/个人hw4/catalogs_fruit.csv",engine)
txt <- scan("D:/工作/数据科学实战/个人hw4/catalogs_fruit.txt", sep = "\n", what = "", encoding = "UTF-8")
df <- freq(engine[txt])
sort(df$freq,decreasing = TRUE) ##top100 = 159
df <- sqldf('select * from df where freq >= 159 ')
wordcloud2(df)


catalogs_4_3 <- read.csv("D:/工作/数据科学实战/个人hw4/catalogs_apple.csv", encoding="UTF-8")
segment("D:/工作/数据科学实战/个人hw4/catalogs_apple.csv",engine)
txt <- scan("D:/工作/数据科学实战/个人hw4/apple.txt", sep = "\n", what = "", encoding = "UTF-8")
df <- freq(engine[txt])
sort(df$freq,decreasing = TRUE) ##top100 = 40
df <- sqldf('select * from df where freq  ')
wordcloud2(df)


#######


word <- scan("D:/工作/数据科学实战/个人hw4/catalogs_1.segment.2021-03-18_20_15_56.csv",sep='n',what='',encoding="UTF-8")  #和前面的word变量完全一样，只是再定义一下
word <- qseg[word]
word <- freq(word)
wordcloud(word$char,word$freq,min.freq = 10,colors = c("blue","red","grey"),scale=c(8,0.5),random.order=F)




catalogs <- read.csv("D:/工作/数据科学实战/个人hw4/catalogs.csv", encoding="UTF-8")
sub <- sample(1:nrow(catalogs),round(nrow(catalogs)*0.7))
length(sub)
data_train<-catalogs[sub,]#取7/10的数据做训练集
data_test<-catalogs[-sub,]#取7/10的数据做测试集








library(text2vec)
library(data.table) 
catalogs <- read.csv("D:/工作/数据科学实战/个人hw4/catalogs_.csv", encoding="UTF-8")
setDT(catalogs) 
setkey(catalogs, id)
set.seed(2016L)  
all_ids = catalogs$id 
train_ids = sample(all_ids, 48646)  
test_ids = setdiff(all_ids, train_ids)  
train = catalogs[J(train_ids)]  
test = catalogs[J(test_ids)] 

prep_fun = tolower  #代表词语划分到什么程度
tok_fun = word_tokenizer 
it_train = itoken(train$X.U.FEFF.name,   
                  preprocessor = prep_fun,   
                  tokenizer = tok_fun,   
                  ids = train$id,   
                  progressbar = FALSE,
                  )
#步骤2.分词#消除停用词

stop_word <- c(word$char[nchar(word$char)==1],word$char[grepl('[0,9]+',word$char)],word$char[grepl('[0-9][a-zA-Z]',word$char)],word$char[grepl('[a-zA-Z]',word$char)],1:100000)


vocab = create_vocabulary(it_train, stopwords = stop_word) 
pruned_vocab = prune_vocabulary(vocab,   
                                term_count_min = 10,   #词频，低于10个都删掉
                                #doc_proportion_max = 0.75,  
                                #doc_proportion_min = 0.001,
                                vocab_term_max = 5000) ###后面发现停词有问题，比如“2500g”这种没有被停用，所以先取5000个，后面停用之后再取成1000


top20 <- sqldf('select * from pruned_vocab where term_count >= 4699 ')
#步骤3.设置形成语料文件
vectorizer = vocab_vectorizer(pruned_vocab)
#步骤4.构建DTM矩阵
dtm_train = create_dtm(it_train, vectorizer)
dtm_train[1,500:600]
#再消除停用词（这一步的目的是发现上一步分词的时候有一些词语没有被停用，比如“2500g”，所以用dtm的词语里再停用）
dcn <- colnames(dtm_train)      ### dcn 是dtm_train columns names
stop_word <- c(stop_word,dcn[nchar(dcn)==1],dcn[grepl('[0,9]+',dcn)],dcn[grepl('[0-9][a-zA-Z]',dcn)],dcn[grepl('[a-zA-Z]',dcn)])
vocab_1 = create_vocabulary(it_train, stopwords = stop_word) 
pruned_vocab_1 = prune_vocabulary(vocab_1,   
                                term_count_min = 10,   #词频，低于10个都删掉
                                doc_proportion_max = 0.75,  
                                
                                vocab_term_max = 1000) 
vectorizer = vocab_vectorizer(pruned_vocab_1)
dtm_train = create_dtm(it_train, vectorizer)


####为了画出任务三中的直方图，我们选择在这里画。
catalogs_fresh <- catalogs[catalogs$first=="生鲜"]
it_fresh = itoken(catalogs_fresh$X.U.FEFF.name,   
                        preprocessor = prep_fun,   
                        tokenizer = tok_fun,   
                        ids = catalogs_fresh$id,   
                        progressbar = FALSE,
)
  ###不用有停用词，直接出dtm
vocab_fresh = create_vocabulary(it_fresh) 
pruned_vocab_fresh = prune_vocabulary(vocab_fresh,   
                                #term_count_min = 10,   #词频，低于10个都删掉
                                #doc_proportion_max = 0.75,  
                                #doc_proportion_min = 0.001,
                                #vocab_term_max = 5000
                                ) 
vectorizer_fresh = vocab_vectorizer(pruned_vocab_fresh)
dtm_fresh = create_dtm(it_fresh, vectorizer_fresh)
fresh_hist_data <- apply(dtm_fresh,1, sum)
hist(fresh_hist_data)
mean(fresh_hist_data)
var(fresh_hist_data)




#步骤5.转化为01矩阵
dtm_train_01 <- 1*(dtm_train >= 1)
apply(dtm_train_01,1,sum)


#还需要把生鲜类的fresh矩阵变成dtm的形式
it_train_fresh = itoken(train_fresh$X.U.FEFF.name,   
                  preprocessor = prep_fun,   
                  tokenizer = tok_fun,   
                  ids = train_fresh$id,   
                  progressbar = FALSE,
)
vocab = create_vocabulary(it_train_fresh, stopwords = stop_word) 
pruned_vocab = prune_vocabulary(vocab,   
                                term_count_min = 10,   #词频，低于10个都删掉
                                doc_proportion_max = 0.75,  
                                doc_proportion_min = 0.001,
                                vocab_term_max = 1000) 
vectorizer = vocab_vectorizer(pruned_vocab)






dtm_train_fresh = create_dtm(it_train_fresh,vectorizer)



#步骤6.转化为因子矩阵
convert_counts <- function(x){
     x <- ifelse(x>0,1,0)
     x <- factor(x, levels=c(0,1),labels=c("No","Yes"))
     return(x)
    }
dtm_train <- apply(dtm_train, MARGIN=2, convert_counts)
dim(dtm_train)
dtm_train_fresh <- apply(dtm_train_fresh,MARGIN=2,convert_counts)
dim(dtm_train_fresh)
dim(train_fresh)
library(e1071)  
train_fresh <- train[train$first == "生鲜"]
sms_classifier <- naiveBayes(dtm_train_fresh,train_fresh$second)




test_fresh <- test[test$first == "生鲜"]
it_test_fresh = itoken(test_fresh$X.U.FEFF.name,   
                        preprocessor = prep_fun,   
                        tokenizer = tok_fun,   
                        ids = test_fresh$id,   
                        progressbar = FALSE,
)
vocab_test = create_vocabulary(it_test_fresh, stopwords = stop_word) 
pruned_vocab_test = prune_vocabulary(vocab_test,   
                                term_count_min = 10,   #词频，低于10个都删掉
                                doc_proportion_max = 0.75,  
                                doc_proportion_min = 0.001,
                                vocab_term_max = 1000) 
vectorizer_test = vocab_vectorizer(pruned_vocab_test)
dtm_test_fresh = create_dtm(it_test_fresh, vectorizer_test)

dtm_test_fresh <- apply(dtm_test_fresh, MARGIN=2, convert_counts)


sms_prediction <- predict(sms_classifier, dtm_test_fresh)


library(gmodels)
CrossTable(sms_prediction,test_fresh$second,prop.chisq=TRUE,prop.t=FALSE, dnn=c("predicted","actual"))




















