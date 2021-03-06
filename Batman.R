#EXECUTE THE CODE INCLUDED IN COMMECT BOXES ONE BY ONE
#INSTALL THE NECESSARY PACKAGES
install.packages("twitteR")
install.packages("ROAuth")
install.packages("plyr")
install.packages("stringr")
install.packages("ggplot2")

library(twitteR)
library(ROAuth)
library(plyr)
library(stringr)
library(ggplot2)

download.file(url="https://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL = "https://api.twitter.com/oauth/access_token"
authURL = "https://api.twitter.com/oauth/authorize"
consumerKey = "GCC2gCPVKrTChIN2EQwGhR5L3"
consumerSecret = "XiVU0uVbPrTKhW0hHwEhjsyiilCxXGpFXRn5XNw2QnNJh1hjVG"


Cred <- OAuthFactory$new(consumerKey=consumerKey, 
                         consumerSecret=consumerSecret,
                         requestURL=requestURL,
                         accessURL=accessURL,
                         authURL=authURL)

Cred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl") )

save(Cred, file="twitter authentication.Rdata")
load("twitter authentication.Rdata")

setup_twitter_oauth(consumerKey, consumerSecret, "800624299035893760-I0mHVsVOF8pw22P5j8IJIBg8XquJsvI", "6LGyYKP71m69Ep6CJ7yx1PTyPAXb6blbdaNssCnBw2RVX")
# 1. RUN TILL THIS POINT AND COMPLETE THE AUTHENTICATION

#######################################################################################

Batman.list <- searchTwitter('#Batman', n=1000)
length(Batman.list)
#Summarize information about a list of tweets in a data frame
Batman.df = twListToDF(Batman.list)
write.csv(Batman.df, file='C:/Users/Atish/Desktop/R/Batman.csv', row.names = F)

# 2. AFTER AUTHENTICATION, RUN THIS BLOCK SEPARATELY.
#######################################################################################

# 3. FOLLOWING CODE CAN BE RUN AT ONCE OR SEPARATELY. RUNNING SEPARATELY WOULD GENERATE EACH
#    GRAPH ONE BY ONE

#Load sentiment word lists
hu.liu.pos = scan('C:/Users/Atish/Desktop/R/positive-words.txt', what='character', comment.char = ';')
hu.liu.neg = scan('C:/Users/Atish/Desktop/R/negative-words.txt', what='character', comment.char = ';')

#Add words to list
pos.words = c(hu.liu.pos, 'DarkKnight','Ledger')
neg.words = c(hu.liu.neg, 'wait', 'waiting', 'epicfail', 'fail','lost')
#Sentiment function
library(plyr)
library(stringr)
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  #Vector of sentences are present. Plyr will handle a list
  scores = laply(sentences, function(sentence, pos.words, neg.words)
  {
    # clean up sentences with R's regex-driven global subtitute, gsub():    
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    sentence = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", sentence)
    sentence = gsub("@\\w+", "", sentence)
    sentence = gsub("http\\w+", "", sentence)
    sentence = gsub("[ \t]{2,}", "", sentence)
    sentence = gsub("^\\s+|\\s+$", "", sentence)
    #and convert to lower case:   
    sentence = tolower(sentence)
    
    #split into words
    words.list = str_split(sentence, '\\s+')
    
    #sometimes a list is one level hierarchy
    words = unlist(words.list) 
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    #match() method returns the location of matched term
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)   
    score = sum(pos.matches) - sum(neg.matches)   
    return(score)
  }, pos.words, neg.words, .progress = .progress )
  scores.df = data.frame(score = scores, text = sentences)
  return(scores.df)
}


#Import csv file
DatasetBatman <- read.csv('C:/Users/Atish/Desktop/R/Batman.csv')
DatasetBatman$text <- as.factor(DatasetBatman$text)

#Score all tweets
Batman.scores = score.sentiment(DatasetBatman$text, pos.words, neg.words, .progress = 'text')
path <- "C:/Users/Atish/Desktop/R/"
write.csv(Batman.scores, file=paste(path, "BatmanScores.csv", sep = ""), row.names = TRUE)

View(Batman.scores)
qplot(Batman.scores$score, xlab = "Score of Tweets")

#####################################################################################
# 4. UNCOMMENT FOLLOWING K1 STATEMENTS AND UNCOMMENT LINE NO 123 AND COMMENT LINE NO 122
#   AND RUN THIS BLOCK AGAIN TO GET REFINED WORDCLOUD.

K1 <- gsub("@\\w+", "", K1)
K1 <- gsub("http\\w+", "", K1)
K1 <- gsub("https\\w+", "", K1)
K1 <- gsub("[ |\t]{2,}", "", K1)
K1 <- gsub("^ ", "", K1)
K1 <- gsub(" $", "", K1)
K1 <- iconv(K1, 'UTF-8', 'ASCII')

#install.packages("tm")
#install.packages("wordcloud")
library(tm)
library(wordcloud)

Batman="C:/Users/Atish/Desktop/R/Batman/"
?source()
#K=Corpus(DirSource(Batman),VectorSource(K1),readerControl = list(language = "eng"))
K=Corpus(DirSource(Batman),readerControl = list(language = "eng"))
inspect(K)
toSpace = content_transformer( function(x, pattern) gsub(pattern," ",x) )
K = tm_map( K, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
K = tm_map( K, toSpace, "htt...*")
K <- tm_map(K, tolower)
K <- tm_map(K, stripWhitespace)
K <- tm_map(K, removePunctuation)
K <- tm_map(K, removeWords, c("new","now","forsale","http", "www", "this", "was","are","rt","you","download","twitter","android", "the", "amp","and","not","all","onsale","for","have","but","that"))
K <- tm_map(K, PlainTextDocument)
K <- tm_map(K, removeNumbers)
K <- tm_map(K, removeWords, stopwords('english'))
K <- tm_map(K, stemDocument)
inspect(K)
tdm <- TermDocumentMatrix(K)
tdm
m1 <- as.matrix(tdm)
m1
v1<- sort(rowSums(m1),decreasing = TRUE)
v1
d1<- data.frame(word = names(v1), freq=v1)
d1
set.seed(142)  
#dark2 <- brewer.pal(6, "Dark2")  
wordcloud(d1$word,d1$freq,max.words="200",colors = rainbow(50),min.freq=10,random.order=F)
  
####################################################################################

dtm <- DocumentTermMatrix(K)   
tdm <- TermDocumentMatrix(K)
freq <- colSums(as.matrix(dtm))   
length(freq)   
ord <- order(freq)   
m <- as.matrix(dtm)   
dim(m)   
write.csv(m, file="C:/Users/Atish/Desktop/R/DocumentTermMatrix_Batman.csv")
dtms <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.   
head(table(freq), 20)   
tail(table(freq), 20)   
freq <- colSums(as.matrix(dtms))   
freq   
findFreqTerms(dtm, lowfreq=3) 

library(ggplot2)   
wf <- data.frame(word=names(freq), freq=freq)   
p <- ggplot(subset(wf, freq>110), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p
##################################################################################
#library(cluster)   
#d <- dist(t(dtms), method="euclidian")   # First calculate distance between words
#fit <- hclust(d=d, method="ward")   
#plot.new()
#plot(fit, hang=-1)
#groups <- cutree(fit, k=5)   # "k=" defines the number of clusters you are using   
#rect.hclust(fit, k=5, border="red") # draw dendogram with red borders around the 5 clusters   


#library(fpc)   
#library(cluster)  
#dtms <- removeSparseTerms(dtm, 0.15) # Prepare the data (max 15% empty space)   
#d <- dist(t(dtms), method="euclidian")   
#kfit <- kmeans(d, 5)   
#clusplot(xlab = "Score of Tweets",as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)