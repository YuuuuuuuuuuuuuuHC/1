catalogs <- read.csv("D:/工作/数据科学实战/个人hw4/catalogs.csv", encoding="UTF-8")
head(catalogs)

########

library(ggplot2)
catalogs$second
unique(catalogs$second)       #Check the types in second
sum(catalogs$first == "生鲜")           ##first 20524 rows
sum(catalogs$first == "食品饮料、保健食品")     ##last 48970 rows
catalogs_first_1 <- catalogs[c(1:20524),]
catalogs_first_2 <- catalogs[c(20525:69494),]

ggplot(data=catalogs_first_1, mapping=aes(x="second",fill=second))+  #draw pie graphs
  geom_bar(stat="count",width=0.5,position='stack')+
  coord_polar("y", start=0)+
geom_text(stat="count",aes(label = scales::percent(..count../20524)), size=4, position=position_stack(vjust = 0.5))    ###饼图1

ggplot(data=catalogs_first_2, mapping=aes(x="second",fill=second))+
  geom_bar(stat="count",width=0.5,position='stack')+
  coord_polar("y", start=0)+
  geom_text(stat="count",aes(label = scales::percent(..count../48970)), size=4, position=position_stack(vjust = 0.5))   ###饼图2



library(jiebaRD)    # This a package for segmenting words
library(jiebaR)     
library(wordcloud2) # This is a package for drawing word cloud.
engine = worker(user = "D:/工作/数据科学实战/个人hw4/userdict.txt")
segment("D:/工作/数据科学实战/个人hw4/catalogs.csv",engine)
word <- scan("D:/工作/数据科学实战/个人hw4/catalogs.segment.2021-03-20_13_50_58.csv",sep='n',what='',encoding="UTF-8")
word <- qseg[word]
word <- freq(word)
#wordcloud(word$char,word$freq,min.freq = 10,colors = c("blue","red","grey"),scale=c(8,0.5),random.order=F)  
####The code above used 'wordcloud' package but not beautiful, so I use 'wordcloud2' package instead.




catalogs_1 <- catalogs_first_1[,1]
catalogs_1 <- as.data.frame(catalogs_1)

write.csv(catalogs_1,file = "D:/工作/数据科学实战/个人hw4/catalogs_1.csv" )
engine = worker(user = "D:/工作/数据科学实战/个人hw4/userdict.txt")
segment("D:/工作/数据科学实战/个人hw4/catalogs_1.csv",engine)
segment_result <- read.csv("D:/工作/数据科学实战/个人hw4/catalogs_1.segment.2021-03-20_08_08_58.csv", encoding="UTF-8")

##Several next steps are implemented in excel. I convert csv to txt, including 'catalogs_fruit','catalogs_fruit_apple'
##The draw 3 pictures using package 'wordcloud2' 

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


word <- scan("D:/工作/数据科学实战/个人hw4/catalogs_1.segment.2021-03-18_20_15_56.csv",sep='n',what='',encoding="UTF-8")  
word <- freq(word)
wordcloud(word$char,word$freq,min.freq = 10,colors = c("blue","red","grey"),scale=c(8,0.5),random.order=F)




catalogs <- read.csv("D:/工作/数据科学实战/个人hw4/catalogs.csv", encoding="UTF-8")
sub <- sample(1:nrow(catalogs),round(nrow(catalogs)*0.7))
length(sub)
data_train<-catalogs[sub,]#70% data for training
data_test<-catalogs[-sub,]#30% data for test








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

#step1 To what extent are representative words divided
prep_fun = tolower  
tok_fun = word_tokenizer 
it_train = itoken(train$X.U.FEFF.name,   
                  preprocessor = prep_fun,   
                  tokenizer = tok_fun,   
                  ids = train$id,   
                  progressbar = FALSE,
                  )
#step2.Word segmentation and elimination of stop words, use regular expression

stop_word <- c(word$char[nchar(word$char)==1],word$char[grepl('[0,9]+',word$char)],word$char[grepl('[0-9][a-zA-Z]',word$char)],word$char[grepl('[a-zA-Z]',word$char)],1:100000)


vocab = create_vocabulary(it_train, stopwords = stop_word) 
pruned_vocab = prune_vocabulary(vocab,   
                                term_count_min = 10,   #word frequency, delete those who <= 10
                                #doc_proportion_max = 0.75,  
                                #doc_proportion_min = 0.001,
                                vocab_term_max = 5000) ###I find some problems in segmention, so I choose 5000, I'll choose a smaller numger after debug


top20 <- sqldf('select * from pruned_vocab where term_count >= 4699 ')
#step3.Set to form corpus file
vectorizer = vocab_vectorizer(pruned_vocab)
#step4.Build Document Term Matrix (DTM) 
dtm_train = create_dtm(it_train, vectorizer)
dtm_train[1,500:600]
#Again, eliminate some words（The purpose of this step is to find that some words have not been deactivated during word segmentation in the previous step, such as "2500g", so they are deactivated in the words using DTM）
dcn <- colnames(dtm_train)      ### dcn is the columns names of dtm_train 
stop_word <- c(stop_word,dcn[nchar(dcn)==1],dcn[grepl('[0,9]+',dcn)],dcn[grepl('[0-9][a-zA-Z]',dcn)],dcn[grepl('[a-zA-Z]',dcn)])
vocab_1 = create_vocabulary(it_train, stopwords = stop_word) 
pruned_vocab_1 = prune_vocabulary(vocab_1,   
                                term_count_min = 10,   #word frequency, delete those who <= 10
                                doc_proportion_max = 0.75,  
                                
                                vocab_term_max = 1000) #use 1000, much smaller than 5000
vectorizer = vocab_vectorizer(pruned_vocab_1)
dtm_train = create_dtm(it_train, vectorizer)


####draw a histgram
catalogs_fresh <- catalogs[catalogs$first=="生鲜"]
it_fresh = itoken(catalogs_fresh$X.U.FEFF.name,   
                        preprocessor = prep_fun,   
                        tokenizer = tok_fun,   
                        ids = catalogs_fresh$id,   
                        progressbar = FALSE,
)
  ###in this step, we don't need stop word.
vocab_fresh = create_vocabulary(it_fresh) 
pruned_vocab_fresh = prune_vocabulary(vocab_fresh,   
                                #term_count_min = 10,   #word frequency, delete those who <= 10
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




#step5.convert to 0-1 matrix
dtm_train_01 <- 1*(dtm_train >= 1)
apply(dtm_train_01,1,sum)


#In addition, need to convert the 'fresh matrix' to dtm
it_train_fresh = itoken(train_fresh$X.U.FEFF.name,   
                  preprocessor = prep_fun,   
                  tokenizer = tok_fun,   
                  ids = train_fresh$id,   
                  progressbar = FALSE,
)
vocab = create_vocabulary(it_train_fresh, stopwords = stop_word) 
pruned_vocab = prune_vocabulary(vocab,   
                                term_count_min = 10,   #word frequency, delete those who <= 10
                                doc_proportion_max = 0.75,  
                                doc_proportion_min = 0.001,
                                vocab_term_max = 1000) 
vectorizer = vocab_vectorizer(pruned_vocab)






dtm_train_fresh = create_dtm(it_train_fresh,vectorizer)



#step6.Convert to factor matrix
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
                                term_count_min = 10,   #word frequency, delete those who <= 10
                                doc_proportion_max = 0.75,  
                                doc_proportion_min = 0.001,
                                vocab_term_max = 1000) 
vectorizer_test = vocab_vectorizer(pruned_vocab_test)
dtm_test_fresh = create_dtm(it_test_fresh, vectorizer_test)

dtm_test_fresh <- apply(dtm_test_fresh, MARGIN=2, convert_counts)


sms_prediction <- predict(sms_classifier, dtm_test_fresh)


library(gmodels)
CrossTable(sms_prediction,test_fresh$second,prop.chisq=TRUE,prop.t=FALSE, dnn=c("predicted","actual"))




















