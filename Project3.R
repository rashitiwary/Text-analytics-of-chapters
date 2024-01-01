
install.packages("tm")
install.packages("dplyr")
install.packages("tidyr")
install.packages("purrr")
install.packages("tidytext")
install.packages("ggplot2")
install.packages("wordcloud")
install.packages("quanteda")
install.packages("syuzhet")
install.packages("stringr")

install.packages("readr")
install.packages("tidy")
install.packages("quanteda.textstats")
library(quanteda.textstats)


library(tidytext)
library(tidy)
library(purrr)
library(readr)
library(tm)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(wordcloud)
library(quanteda)
library(syuzhet)
library(NLP)
library(magrittr)

#setting the path
setwd("C:/Users/nikhi/Documents/Documents/Bigdata Project 3 data")
file_path <- "APrincessOfMars.txt"
book_lines <- readLines(file_path, encoding = "UTF-8")
getwd()


# Create a directory to store the chapter files
dir.create("chapters")


chapterlist <- c("CHAPTER I","CHAPTER II","CHAPTER III","CHAPTER IV","CHAPTER V","CHAPTER VI","CHAPTER VII","CHAPTER VIII","CHAPTER IX","CHAPTER X","CHAPTER XI","CHAPTER XII")

for (chapterIdx in seq_len(length(clist)-1)) {
  file_path <- "APrincessOfMars.txt"
  book_lines <- readLines(file_path, encoding = "UTF-8")
  
  indx_chx <- which(book_lines == chapterlist[chapterIdx])
  indx_chy <- which(book_lines == chapterlist[chapterIdx+1])
  
  book_chapter <- book_lines[(indx_chx + 1):(indx_chy - 1)]
  write.table(book_chapter, file = paste0("chapters/book_chapter", as.character(chapterIdx), ".txt"), sep = "\t", row.names=FALSE, col.names=FALSE, quote=FALSE)
}

 

setwd("C:/Users/nikhi/Documents/Documents/Bigdata Project 3 data/chapters")
Textbook<-VCorpus(DirSource(".",ignore.case=FALSE, mode="text"))
str(Textbook)
Textbook
btext<-Textbook[[1]]
btext
btext[1]
inspect(Textbook)

# Finding the 10 longest sentences in each chapter
longest_sentences <- tidy(Textbook)%>%unnest_tokens(sentence,text,token = "regex", pattern = "(?<!\\b\\p{L}r)\\.")%>%
  select(id,sentence) %>%mutate(sentence_length = nchar(sentence)) %>%arrange(desc(sentence_length)) 

# Finding the 10 longest words in each chapter
words_longest<- tidy(Textbook)%>%unnest_tokens(word,text)%>%
  select(id,word) %>%mutate(word_length = nchar(word)) %>%arrange(desc(word_length)) 

for (i in 1:11){
  file_path <- paste0("book_chapter", as.character(i), ".txt")
  print(words_longest%>% filter(id == file_path))
  print(longest_sentences%>% filter(id == file_path))
}  

#computing the DocumentTermMatrix
TextbookDTM<-DocumentTermMatrix(Textbook)
TextbookDTM
inspect(TextbookDTM)
str(TextbookDTM)

#computing the TermDocumentMatrix
TextbookTDM<-TermDocumentMatrix(Textbook)
TextbookTDM
inspect(TextbookTDM)
str(TextbookTDM)

Textbookdf<-data.frame(btext[1])
Textbookdf[1]

btext[1]

#defining function to remove numbers and punctuation
removeNumPunct<-function(x) gsub("[^[:alpha:][:space:]]*","",x)
removeNumPunct

#removing numbers and punctuation 
Textbookcl<-tm::tm_map(Textbook,content_transformer(removeNumPunct))
Textbookcl
str(Textbookcl)
inspect(Textbookcl)

#Converting document to lowercase
TextbookLow<-tm_map(Textbookcl,content_transformer(tolower))
TextbookLow
str(TextbookLow)
inspect(TextbookLow)

#computing the DocumentTermMatrix
TextbookDTM<-DocumentTermMatrix(TextbookLow)
TextbookDTM
inspect(TextbookDTM)
str(TextbookDTM)

as.matrix(TextbookDTM)

mystopwords<-c(tm::stopwords("english"))
mystopwords

#Removing the stopwords
Textbookstop<-tm::tm_map(TextbookLow,tm::removeWords,mystopwords)
tm::inspect(Textbookstop[[1]])

#creating TDM again after removal of the stop words.
TextbookstopTDM<-tm::TermDocumentMatrix(Textbookstop)
TextbookstopTDM
inspect(TextbookstopTDM)
str(TextbookstopTDM)

freqterms<-findFreqTerms(TextbookstopTDM,lowfreq = 5)
freqterms

#finding the length of a string in freqTerms
nchar(freqterms[17])
freqterms[17]

#term frequency using termFreq
Textbooktf<-tm::termFreq(Textbookstop[[1]])
Textbooktf

#inspecting TDM again
tm::inspect(TextbookstopTDM)

#Plotting the dendrogram
Textbookdf<-as.data.frame(TextbookstopTDM[[1]])
TextbookDist<-dist(Textbookdf)
# Perform hierarchical clustering using Ward's method
TextbookDG<-hclust(TextbookDist,method="ward.D2")
str(TextbookDG)

plot(TextbookDG)



#Plotting the dendogram by removing words with high sparsity
TextbookstopTDM_Mat=as.matrix(TextbookstopTDM)

# Set sparsity threshold 
sparsity_thres <- 0.83

# Calculate sparsity for each word
sparsity <- rowSums(TextbookstopTDM_Mat > 0) / ncol(TextbookstopTDM_Mat)

# Select words with sparsity below the threshold
words_select<- which(sparsity > sparsity_thres)

# Create a new TDM with the selected words
TextbookFilteredTDM <- TextbookstopTDM_Mat[words_select,]


Textbookdf1 <- as.data.frame(TextbookFilteredTDM)
TextbookDist1 <- dist(Textbookdf1)
TextbookDG1 <- hclust(TextbookDist1, method = "ward.D2")
str(TextbookDG1)

# Ploting the dendrogram
plot(TextbookDG1)






#Using tibbles with n=10(words of length)
TextbookstopTDM_changed <- tidy(Textbookstop)%>%
  unnest_tokens(word,text)%>%
  count(id,word) %>%
  mutate(word_length = nchar(word)) %>%
  arrange(desc(word_length)) %>%filter(word_length>10) %>% cast_tdm(word, id, n)


#Plotting the dendrogram for n=10 n means words of length
Textbookdf<-as.data.frame(TextbookstopTDM_changed[[1]])
TextbookDist<-dist(Textbookdf)
TextbookDG2<-hclust(TextbookDist,method="ward.D2")
str(TextbookDG2)

plot(TextbookDG2)


#Using tibbles with n=12(words of length)
TextbookstopTDM_changed <- tidy(Textbookstop)%>%
  unnest_tokens(word,text)%>%
  count(id,word) %>%
  mutate(word_length = nchar(word)) %>%
  arrange(desc(word_length)) %>%filter(word_length>12) %>% cast_tdm(word, id, n)


#Plotting the dendrogram for n=12 n means words of length
Textbookdf<-as.data.frame(TextbookstopTDM_changed[[1]])
TextbookDist<-dist(Textbookdf)
TextbookDG3<-hclust(TextbookDist,method="ward.D2")
str(TextbookDG3)

plot(TextbookDG3)


#Using tibbles with n=13(words of length)
TextbookstopTDM_changed <- tidy(Textbookstop)%>%
  unnest_tokens(word,text)%>%
  count(id,word) %>%
  mutate(word_length = nchar(word)) %>%
  arrange(desc(word_length)) %>%filter(word_length>13) %>% cast_tdm(word, id, n)


#Plotting the dendrogram for n=13 n means words of length
Textbookdf<-as.data.frame(TextbookstopTDM_changed[[1]])
TextbookDist<-dist(Textbookdf)
TextbookDG4<-hclust(TextbookDist,method="ward.D2")
str(TextbookDG4)

plot(TextbookDG4)

#WordCloud
words<-names(Textbooktf)
words


pal<-brewer.pal(9,"BuGn")
str(pal)

TextbookWC<-wordcloud(words, Textbooktf, colors=pal [-(1:4)])
str(TextbookWC)


#Wordcloud with a different pallet
pal2<-brewer.pal(9,"Spectral")

TextbookWC2<-wordcloud(words, Textbooktf, colors=pal2)


#Working with QUANTEDA package
TextbookText<-Textbookcl[[1]]
TextbookText$content[1:15]

#Applying Tokenization
TextbookTokens<-quanteda::tokens(TextbookText$content[1:15])
str(TextbookTokens)

TextbookDFM<-quanteda::dfm(TextbookTokens)
str(TextbookDFM)
TextbookDFM

#frequency of terms in the dfm
TextbookDocFreq<-quanteda::docfreq(TextbookDFM)
str(TextbookDocFreq)
TextbookDocFreq

#assigning weights to the words
Textbookweights<-quanteda::dfm_weight(TextbookDFM)
str(Textbookweights)
Textbookweights


#computing the tf-idf score
TextbookTFIDF<-quanteda::dfm_tfidf(TextbookDFM, scheme_tf = "count", scheme_df = "inverse")
str(TextbookTFIDF)



#Working with syuzhet package
TextbookTextdf<-as.data.frame(TextbookText$content)
TextbookTextdf


TextbookAsString<-get_text_as_string("book_chapter1.txt")
TextbookAsString

#Implementing get_sentences, this function parses the text string into individual sentences
BS<-get_sentences(TextbookAsString)
BS

str(BS)

#Implementing get_sentiment, this function which will assess the sentiment of each word or sentence. 
BSSentiment<-get_sentiment(BS,"syuzhet")
BSSentiment

BSBing<-get_sentiment(BS,"bing")
BSBing

BSSentimentNRC<-get_sentiment(BS,"nrc")
BSSentimentNRC


BSDictionary<-get_sentiment_dictionary()
BSDictionary


BSDictionaryBing<-get_sentiment_dictionary("bing")
BSDictionaryBing


BSDictionaryNRC<-get_sentiment_dictionary("nrc")
BSDictionaryNRC

#Sum the values of the sentiment vector 
BSSum<-sum(BSSentiment)
BSSum

BSBingSum<-sum(BSBing)
BSBingSum

BSNRCSUM<-sum(BSSentimentNRC)
BSNRCSUM

#Computing means
BSMean<-mean(BSSentiment)
BSMean

BSBingMean<-mean(BSBing)
BSBingMean

BSNRCMean<-mean(BSSentimentNRC)
BSNRCMean

plot(
  BSSentiment, 
  type="l", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

plot(BSSentiment, main=" Textbook Plot Trajectory", xlab="Narrative", ylab="Emotional Valence",col="green")

plot(BSBing, main=" Textbook Plot Trajectory:Bing", xlab="Narrative", ylab="Emotional Valence", col="blue")

plot(BSSentimentNRC, main=" Textbook Plot Trajectory NRC Sentiment", xlab="Narrative", ylab="Emotional Valence", col="blue")


BSSentimentPctValue<-get_percentage_values(BSSentiment, bins=10)
structure(BSSentimentPctValue)
          
plot(BSSentimentPctValue, main=" Textbook Plot Trajectory PCT Value 10 bins", xlab="Narrative", ylab="Emotional Valence",col="orange")

#With 20 bins
BSSentimentPctValue<-get_percentage_values(BSSentiment, bins=20)
structure(BSSentimentPctValue)

plot(BSSentimentPctValue, main=" Textbook Plot Trajectory PCT Value 20 bins", xlab="Narrative", ylab="Emotional Valence",col="orange")

NRCSentiment<-get_nrc_sentiment(BS)
NRCSentiment


barplot(
  sort(colSums(prop.table(NRCSentiment[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Sample text", xlab="Percentage"
)

#Creating the table for sentiment analysis for all chapters

computeSentiment <- function(text) {
  sentences <- get_sentences(text)
  sentimentValues <- get_sentiment(sentences, "syuzhet")
  sentimentValuesBing<-get_sentiment(sentences, "bing")
  sentimentValuesNRC<-get_sentiment(sentences, "nrc")
  sentimentSum <- sum(sentimentValues)
  sentimentSumBing <- sum(sentimentValuesBing)
  sentimentSumNRC <- sum(sentimentValuesNRC)
  sentimentMean <- mean(sentimentValues)
  sentimentMeanBing <- mean(sentimentValuesBing)
  sentimentMeanNRC <- mean(sentimentValuesNRC)
  
  list(syuzhetSum = sentimentSum, syuzhetMean = sentimentMean, bingSum = sentimentSumBing, bingMean = sentimentMeanBing, nrcSum = sentimentSumNRC, nrcMean = sentimentMeanNRC )
}

AllChapters <- list.files(path = "C:/Users/nikhi/Documents/Documents/Bigdata Project 3 data/chapters/", pattern = "book_chapter.*\\.txt$", full.names = TRUE)

# Compute Sentiment Values, Mean and Sum Values
results <- lapply(AllChapters, function(chapter) {
  text <- get_text_as_string(chapter)
  computeSentiment(text)
})

# Creating a Table
chapter_names <- basename(AllChapters)

table <- tibble(
  Chapter = chapter_names,
  SentimentSum_Syuzhet = sapply(results, function(res) res$syuzhetSum),
  SentimentMean_Syuzhet = sapply(results, function(res) res$syuzhetMean),
  SentimentSum_Bing = sapply(results, function(res) res$bingSum),
  SentimentMean_Bing = sapply(results, function(res) res$bingMean),
  SentimentSum_NRC = sapply(results, function(res) res$nrcSum),
  SentimentMean_NRC = sapply(results, function(res) res$nrcMean)
)

print(table)


#Implementing sentiment analysis for chapter 2

TextbookAsString_chapter2<-get_text_as_string("book_chapter2.txt")
TextbookAsString_chapter2

`#Implementing get_sentences, this function parses the text string into individual sentences
BS2<-get_sentences(TextbookAsString_chapter2)
BS2

str(BS2)

#Implementing get_sentiment, this function which will assess the sentiment of each word or sentence. 
BSSentiment2<-get_sentiment(BS2,"syuzhet")
BSSentiment2

BSBing2<-get_sentiment(BS2,"bing")
BSBing2

BSSentimentNRC2<-get_sentiment(BS2,"nrc")
BSSentimentNRC2


BSDictionary2<-get_sentiment_dictionary()
BSDictionary2


BSDictionaryBing2<-get_sentiment_dictionary("bing")
BSDictionaryBing2

#Sum the values of the sentiment vector 
BSSum2<-sum(BSSentiment2)
BSSum2

BSBingSum2<-sum(BSBing2)
BSBingSum2

#Computing means
BSMean2<-mean(BSSentiment2)
BSMean2

BSBingMean2<-mean(BSBing2)
BSBingMean2

plot(
  BSSentiment2, 
  type="l", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

plot(BSSentiment2, main=" Textbook Plot Trajectory chapter 2", xlab="Narrative", ylab="Emotional Valence",col="green")

plot(BSBing2, main=" Textbook Plot Trajectory chapter 2 :Bing", xlab="Narrative", ylab="Emotional Valence", col="blue")

plot(BSSentimentNRC2, main=" Textbook Plot Trajectory NRC Sentiment chapter 2", xlab="Narrative", ylab="Emotional Valence", col="blue")


BSSentimentPctValue2<-get_percentage_values(BSSentiment2, bins=10)
structure(BSSentimentPctValue2)

plot(BSSentimentPctValue2, main=" Textbook Plot Trajectory chapter 2 PCT Value 10 bins", xlab="Narrative", ylab="Emotional Valence",col="orange")

#With 20 bins
BSSentimentPctValue2<-get_percentage_values(BSSentiment2, bins=20)
structure(BSSentimentPctValue2)

plot(BSSentimentPctValue2, main=" Textbook Plot Trajectory chapter 2 PCT Value 20 bins", xlab="Narrative", ylab="Emotional Valence",col="orange")


#Implementing sentiment analysis for chapter 3


TextbookAsString_chapter3<-get_text_as_string("book_chapter3.txt")
TextbookAsString_chapter3

#Implementing get_sentences, this function parses the text string into individual sentences
BS3<-get_sentences(TextbookAsString_chapter3)
BS3

str(BS3)

#Implementing get_sentiment, this function which will assess the sentiment of each word or sentence. 
BSSentiment3<-get_sentiment(BS3,"syuzhet")
BSSentiment3

BSBing3<-get_sentiment(BS3,"bing")
BSBing3

BSSentimentNRC3<-get_sentiment(BS3,"nrc")
BSSentimentNRC3


BSDictionary3<-get_sentiment_dictionary()
BSDictionary3

BSDictionaryBing3<-get_sentiment_dictionary("bing")
BSDictionaryBing3

#Sum the values of the sentiment vector 
BSSum3<-sum(BSSentiment3)
BSSum3

BSBingSum3<-sum(BSBing3)
BSBingSum3

#Computing means
BSMean3<-mean(BSSentiment3)
BSMean3

BSBingMean3<-mean(BSBing3)
BSBingMean3

plot(
  BSSentiment3, 
  type="l", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

plot(BSSentiment3, main=" Textbook Plot Trajectory chapter 3", xlab="Narrative", ylab="Emotional Valence",col="green")

plot(BSBing3, main=" Textbook Plot Trajectory chapter 3 :Bing", xlab="Narrative", ylab="Emotional Valence", col="blue")

plot(BSSentimentNRC3, main=" Textbook Plot Trajectory NRC Sentiment chapter 3", xlab="Narrative", ylab="Emotional Valence", col="blue")


BSSentimentPctValue3<-get_percentage_values(BSSentiment3, bins=10)
structure(BSSentimentPctValue3)

plot(BSSentimentPctValue3, main=" Textbook Plot Trajectory chapter 3 PCT Value 10 bins", xlab="Narrative", ylab="Emotional Valence",col="orange")

#With 20 bins
BSSentimentPctValue3<-get_percentage_values(BSSentiment3, bins=20)
structure(BSSentimentPctValue3)

plot(BSSentimentPctValue3, main=" Textbook Plot Trajectory chapter 3 PCT Value 20 bins", xlab="Narrative", ylab="Emotional Valence",col="orange")



#Implementing sentiment analysis for chapter 4
TextbookAsString_chapter4<-get_text_as_string("book_chapter4.txt")
TextbookAsString_chapter4

#Implementing get_sentences, this function parses the text string into individual sentences
BS4<-get_sentences(TextbookAsString_chapter4)
BS4

str(BS4)

#Implementing get_sentiment, this function which will assess the sentiment of each word or sentence. 
BSSentiment4<-get_sentiment(BS4,"syuzhet")
BSSentiment4

BSBing4<-get_sentiment(BS4,"bing")
BSBing4

BSSentimentNRC4<-get_sentiment(BS4,"nrc")
BSSentimentNRC4

BSDictionary4<-get_sentiment_dictionary()
BSDictionary4

BSDictionaryBing4<-get_sentiment_dictionary("bing")
BSDictionaryBing4

#Sum the values of the sentiment vector 
BSSum4<-sum(BSSentiment4)
BSSum4

BSBingSum4<-sum(BSBing4)
BSBingSum4

#Computing means
BSMean4<-mean(BSSentiment4)
BSMean4

BSBingMean4<-mean(BSBing4)
BSBingMean4

plot(
  BSSentiment4, 
  type="l", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

plot(BSSentiment4, main=" Textbook Plot Trajectory chapter 4", xlab="Narrative", ylab="Emotional Valence",col="green")

plot(BSBing4, main=" Textbook Plot Trajectory chapter 4 :Bing", xlab="Narrative", ylab="Emotional Valence", col="blue")

plot(BSSentimentNRC4, main=" Textbook Plot Trajectory NRC Sentiment chapter 4", xlab="Narrative", ylab="Emotional Valence", col="blue")


BSSentimentPctValue4<-get_percentage_values(BSSentiment4, bins=10)
structure(BSSentimentPctValue4)

plot(BSSentimentPctValue4, main=" Textbook Plot Trajectory chapter 4 PCT Value 10 bins", xlab="Narrative", ylab="Emotional Valence",col="red")

#With 20 bins
BSSentimentPctValue4<-get_percentage_values(BSSentiment4, bins=20)
structure(BSSentimentPctValue4)

plot(BSSentimentPctValue4, main=" Textbook Plot Trajectory chapter 4 PCT Value 20 bins", xlab="Narrative", ylab="Emotional Valence",col="red")



#Implementing sentiment analysis for chapter 5

TextbookAsString_chapter5<-get_text_as_string("book_chapter5.txt")
TextbookAsString_chapter5

# Implementing get_sentences, this function parses the text string into individual sentences
BS5<-get_sentences(TextbookAsString_chapter5)
BS5

str(BS5)

# Implementing get_sentiment, this function which will assess the sentiment of each word or sentence. 
BSSentiment5<-get_sentiment(BS5,"syuzhet")
BSSentiment5

BSBing5<-get_sentiment(BS5,"bing")
BSBing5

BSSentimentNRC5<-get_sentiment(BS5,"nrc")
BSSentimentNRC5

BSDictionary5<-get_sentiment_dictionary()
BSDictionary5

BSDictionaryBing5<-get_sentiment_dictionary("bing")
BSDictionaryBing5

# Sum the values of the sentiment vector 
BSSum5<-sum(BSSentiment5)
BSSum5

BSBingSum5<-sum(BSBing5)
BSBingSum5

# Computing means
BSMean5<-mean(BSSentiment5)
BSMean5

BSBingMean5<-mean(BSBing5)
BSBingMean5

plot(
  BSSentiment5, 
  type="l", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

plot(BSSentiment5, main=" Textbook Plot Trajectory chapter 5", xlab="Narrative", ylab="Emotional Valence",col="green")

plot(BSBing5, main=" Textbook Plot Trajectory chapter 5 :Bing", xlab="Narrative", ylab="Emotional Valence", col="blue")

plot(BSSentimentNRC5, main=" Textbook Plot Trajectory NRC Sentiment chapter 5", xlab="Narrative", ylab="Emotional Valence", col="blue")

BSSentimentPctValue5<-get_percentage_values(BSSentiment5, bins=10)
structure(BSSentimentPctValue5)

plot(BSSentimentPctValue5, main=" Textbook Plot Trajectory chapter 5 PCT Value 10 bins", xlab="Narrative", ylab="Emotional Valence",col="orange")

# With 20 bins
BSSentimentPctValue5<-get_percentage_values(BSSentiment5, bins=20)
structure(BSSentimentPctValue5)

plot(BSSentimentPctValue5, main=" Textbook Plot Trajectory chapter 5 PCT Value 20 bins", xlab="Narrative", ylab="Emotional Valence",col="orange")

#Implementing sentiment analysis for chapter 6

TextbookAsString_chapter6 <- get_text_as_string("book_chapter6.txt")
TextbookAsString_chapter6

# Implementing get_sentences, this function parses the text string into individual sentences
BS6 <- get_sentences(TextbookAsString_chapter6)
BS6

str(BS6)

# Implementing get_sentiment, this function which will assess the sentiment of each word or sentence.
BSSentiment6 <- get_sentiment(BS6, "syuzhet")
BSSentiment6

BSBing6 <- get_sentiment(BS6, "bing")
BSBing6

BSSentimentNRC6 <- get_sentiment(BS6, "nrc")
BSSentimentNRC6

BSDictionary6 <- get_sentiment_dictionary()
BSDictionary6

BSDictionaryBing6 <- get_sentiment_dictionary("bing")
BSDictionaryBing6

# Sum the values of the sentiment vector
BSSum6 <- sum(BSSentiment6)
BSSum6

BSBingSum6 <- sum(BSBing6)
BSBingSum6

# Computing means
BSMean6 <- mean(BSSentiment6)
BSMean6

BSBingMean6 <- mean(BSBing6)
BSBingMean6

plot(
  BSSentiment6,
  type = "l",
  main = "Example Plot Trajectory",
  xlab = "Narrative Time",
  ylab = "Emotional Valence"
)

plot(
  BSSentiment6,
  main = " Textbook Plot Trajectory chapter 6",
  xlab = "Narrative",
  ylab = "Emotional Valence",
  col = "green"
)

plot(
  BSBing6,
  main = " Textbook Plot Trajectory chapter 6 :Bing",
  xlab = "Narrative",
  ylab = "Emotional Valence",
  col = "blue"
)

plot(
  BSSentimentNRC6,
  main = " Textbook Plot Trajectory NRC Sentiment chapter 6",
  xlab = "Narrative",
  ylab = "Emotional Valence",
  col = "blue"
)

BSSentimentPctValue6 <- get_percentage_values(BSSentiment6, bins = 10)
structure(BSSentimentPctValue6)

plot(
  BSSentimentPctValue6,
  main = " Textbook Plot Trajectory chapter 6 PCT Value 10 bins",
  xlab = "Narrative",
  ylab = "Emotional Valence",
  col = "orange"
)

# With 20 bins
BSSentimentPctValue6 <- get_percentage_values(BSSentiment6, bins = 20)
structure(BSSentimentPctValue6)

plot(
  BSSentimentPctValue6,
  main = " Textbook Plot Trajectory chapter 6 PCT Value 20 bins",
  xlab = "Narrative",
  ylab = "Emotional Valence",
  col = "orange"
)



#Implementing sentiment analysis for chapter 7

TextbookAsString_chapter7<-get_text_as_string("book_chapter7.txt")
TextbookAsString_chapter7

#Implementing get_sentences, this function parses the text string into individual sentences
BS7<-get_sentences(TextbookAsString_chapter7)
BS7

str(BS7)

#Implementing get_sentiment, this function which will assess the sentiment of each word or sentence. 
BSSentiment7<-get_sentiment(BS7,"syuzhet")
BSSentiment7

BSBing7<-get_sentiment(BS7,"bing")
BSBing7

BSSentimentNRC7<-get_sentiment(BS7,"nrc")
BSSentimentNRC7


BSDictionary7<-get_sentiment_dictionary()
BSDictionary7


BSDictionaryBing7<-get_sentiment_dictionary("bing")
BSDictionaryBing7

#Sum the values of the sentiment vector 
BSSum7<-sum(BSSentiment7)
BSSum7

BSBingSum7<-sum(BSBing7)
BSBingSum7

#Computing means
BSMean7<-mean(BSSentiment7)
BSMean7

BSBingMean7<-mean(BSBing7)
BSBingMean7

plot(
  BSSentiment7, 
  type="l", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

plot(BSSentiment7, main=" Textbook Plot Trajectory chapter 7", xlab="Narrative", ylab="Emotional Valence",col="green")

plot(BSBing7, main=" Textbook Plot Trajectory chapter 7 :Bing", xlab="Narrative", ylab="Emotional Valence", col="blue")

plot(BSSentimentNRC7, main=" Textbook Plot Trajectory NRC Sentiment chapter 7", xlab="Narrative", ylab="Emotional Valence", col="blue")


BSSentimentPctValue7<-get_percentage_values(BSSentiment7, bins=10)
structure(BSSentimentPctValue7)

plot(BSSentimentPctValue7, main=" Textbook Plot Trajectory chapter 7 PCT Value 10 bins", xlab="Narrative", ylab="Emotional Valence",col="orange")

#With 20 bins
BSSentimentPctValue7<-get_percentage_values(BSSentiment7, bins=20)
structure(BSSentimentPctValue7)

plot(BSSentimentPctValue7, main=" Textbook Plot Trajectory chapter 7 PCT Value 20 bins", xlab="Narrative", ylab="Emotional Valence",col="orange")


#Implementing sentiment analysis for chapter 8

TextbookAsString_chapter8 <- get_text_as_string("book_chapter8.txt")
TextbookAsString_chapter8

#Implementing get_sentences, this function parses the text string into individual sentences
BS8 <- get_sentences(TextbookAsString_chapter8)
BS8

str(BS8)

#Implementing get_sentiment, this function which will assess the sentiment of each word or sentence. 
BSSentiment8 <- get_sentiment(BS8, "syuzhet")
BSSentiment8

BSBing8 <- get_sentiment(BS8, "bing")
BSBing8

BSSentimentNRC8 <- get_sentiment(BS8, "nrc")
BSSentimentNRC8

BSDictionary8 <- get_sentiment_dictionary()
BSDictionary8

BSDictionaryBing8 <- get_sentiment_dictionary("bing")
BSDictionaryBing8

#Sum the values of the sentiment vector 
BSSum8 <- sum(BSSentiment8)
BSSum8

BSBingSum8 <- sum(BSBing8)
BSBingSum8

#Computing means
BSMean8 <- mean(BSSentiment8)
BSMean8

BSBingMean8 <- mean(BSBing8)
BSBingMean8

plot(
  BSSentiment8, 
  type="l", 
  main="Example Plot Trajectory", 
  xlab="Narrative Time", 
  ylab="Emotional Valence"
)

plot(BSSentiment8, main="Textbook Plot Trajectory chapter 8", xlab="Narrative", ylab="Emotional Valence", col="green")

plot(BSBing8, main="Textbook Plot Trajectory chapter 8 :Bing", xlab="Narrative", ylab="Emotional Valence", col="blue")

plot(BSSentimentNRC8, main="Textbook Plot Trajectory NRC Sentiment chapter 8", xlab="Narrative", ylab="Emotional Valence", col="blue")

BSSentimentPctValue8 <- get_percentage_values(BSSentiment8, bins=10)
structure(BSSentimentPctValue8)

plot(BSSentimentPctValue8, main="Textbook Plot Trajectory chapter 8 PCT Value 10 bins", xlab="Narrative", ylab="Emotional Valence", col="orange")

#With 20 bins
BSSentimentPctValue8 <- get_percentage_values(BSSentiment8, bins=20)
structure(BSSentimentPctValue8)

plot(BSSentimentPctValue8, main="Textbook Plot Trajectory chapter 8 PCT Value 20 bins", xlab="Narrative", ylab="Emotional Valence", col="orange")

#Implementing sentiment analysis for chapter 9

TextbookAsString_chapter9<-get_text_as_string("book_chapter9.txt")
TextbookAsString_chapter9

#Implementing get_sentences, this function parses the text string into individual sentences
BS9<-get_sentences(TextbookAsString_chapter9)
BS9

str(BS9)

#Implementing get_sentiment, this function which will assess the sentiment of each word or sentence. 
BSSentiment9<-get_sentiment(BS9,"syuzhet")
BSSentiment9

BSBing9<-get_sentiment(BS9,"bing")
BSBing9

BSSentimentNRC9<-get_sentiment(BS9,"nrc")
BSSentimentNRC9


BSDictionary9<-get_sentiment_dictionary()
BSDictionary9


BSDictionaryBing9<-get_sentiment_dictionary("bing")
BSDictionaryBing9

#Sum the values of the sentiment vector 
BSSum9<-sum(BSSentiment9)
BSSum9

BSBingSum9<-sum(BSBing9)
BSBingSum9

#Computing means
BSMean9<-mean(BSSentiment9)
BSMean9

BSBingMean9<-mean(BSBing9)
BSBingMean9

plot(
  BSSentiment9, 
  type="l", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

plot(BSSentiment9, main=" Textbook Plot Trajectory chapter 9", xlab="Narrative", ylab="Emotional Valence",col="green")

plot(BSBing9, main=" Textbook Plot Trajectory chapter 9 :Bing", xlab="Narrative", ylab="Emotional Valence", col="blue")

plot(BSSentimentNRC9, main=" Textbook Plot Trajectory NRC Sentiment chapter 9", xlab="Narrative", ylab="Emotional Valence", col="blue")


BSSentimentPctValue9<-get_percentage_values(BSSentiment9, bins=10)
structure(BSSentimentPctValue9)

plot(BSSentimentPctValue9, main=" Textbook Plot Trajectory chapter 9 PCT Value 10 bins", xlab="Narrative", ylab="Emotional Valence",col="orange")

#With 20 bins
BSSentimentPctValue9<-get_percentage_values(BSSentiment9, bins=20)
structure(BSSentimentPctValue9)

plot(BSSentimentPctValue9, main=" Textbook Plot Trajectory chapter 9 PCT Value 20 bins", xlab="Narrative", ylab="Emotional Valence",col="orange")

#Implementing sentiment analysis for chapter 10

TextbookAsString_chapter10<-get_text_as_string("book_chapter10.txt")
TextbookAsString_chapter10

#Implementing get_sentences, this function parses the text string into individual sentences
BS10<-get_sentences(TextbookAsString_chapter10)
BS10

str(BS10)

#Implementing get_sentiment, this function which will assess the sentiment of each word or sentence. 
BSSentiment10<-get_sentiment(BS10,"syuzhet")
BSSentiment10

BSBing10<-get_sentiment(BS10,"bing")
BSBing10

BSSentimentNRC10<-get_sentiment(BS10,"nrc")
BSSentimentNRC10

BSDictionary10<-get_sentiment_dictionary()
BSDictionary10

BSDictionaryBing10<-get_sentiment_dictionary("bing")
BSDictionaryBing10

#Sum the values of the sentiment vector 
BSSum10<-sum(BSSentiment10)
BSSum10

BSBingSum10<-sum(BSBing10)
BSBingSum10

#Computing means
BSMean10<-mean(BSSentiment10)
BSMean10

BSBingMean10<-mean(BSBing10)
BSBingMean10

plot(
  BSSentiment10, 
  type="l", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

plot(BSSentiment10, main=" Textbook Plot Trajectory chapter 10", xlab="Narrative", ylab="Emotional Valence",col="green")

plot(BSBing10, main=" Textbook Plot Trajectory chapter 10 :Bing", xlab="Narrative", ylab="Emotional Valence", col="blue")

plot(BSSentimentNRC10, main=" Textbook Plot Trajectory NRC Sentiment chapter 10", xlab="Narrative", ylab="Emotional Valence", col="blue")


BSSentimentPctValue10<-get_percentage_values(BSSentiment10, bins=10)
structure(BSSentimentPctValue10)

plot(BSSentimentPctValue10, main=" Textbook Plot Trajectory chapter 10 PCT Value 10 bins", xlab="Narrative", ylab="Emotional Valence",col="orange")

#With 20 bins
BSSentimentPctValue10<-get_percentage_values(BSSentiment10, bins=20)
structure(BSSentimentPctValue10)

plot(BSSentimentPctValue10, main=" Textbook Plot Trajectory chapter 10 PCT Value 20 bins", xlab="Narrative", ylab="Emotional Valence",col="orange")


#Implementing sentiment analysis for chapter 11

TextbookAsString_chapter11<-get_text_as_string("book_chapter11.txt")
TextbookAsString_chapter11

#Implementing get_sentences, this function parses the text string into individual sentences
BS11<-get_sentences(TextbookAsString_chapter11)
BS11

str(BS11)

#Implementing get_sentiment, this function which will assess the sentiment of each word or sentence. 
BSSentiment11<-get_sentiment(BS11,"syuzhet")
BSSentiment11

BSBing11<-get_sentiment(BS11,"bing")
BSBing11

BSSentimentNRC11<-get_sentiment(BS11,"nrc")
BSSentimentNRC11


BSDictionary11<-get_sentiment_dictionary()
BSDictionary11


BSDictionaryBing11<-get_sentiment_dictionary("bing")
BSDictionaryBing11

#Sum the values of the sentiment vector 
BSSum11<-sum(BSSentiment11)
BSSum11

BSBingSum11<-sum(BSBing11)
BSBingSum11

#Computing means
BSMean11<-mean(BSSentiment11)
BSMean11

BSBingMean11<-mean(BSBing11)
BSBingMean11

plot(
  BSSentiment11, 
  type="l", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

plot(BSSentiment11, main=" Textbook Plot Trajectory chapter 11", xlab="Narrative", ylab="Emotional Valence",col="green")

plot(BSBing11, main=" Textbook Plot Trajectory chapter 11 :Bing", xlab="Narrative", ylab="Emotional Valence", col="blue")

plot(BSSentimentNRC11, main=" Textbook Plot Trajectory NRC Sentiment chapter 11", xlab="Narrative", ylab="Emotional Valence", col="blue")


BSSentimentPctValue11<-get_percentage_values(BSSentiment11, bins=10)
structure(BSSentimentPctValue11)

plot(BSSentimentPctValue11, main=" Textbook Plot Trajectory chapter 11 PCT Value 10 bins", xlab="Narrative", ylab="Emotional Valence",col="orange")

#With 20 bins
BSSentimentPctValue11<-get_percentage_values(BSSentiment11, bins=20)
structure(BSSentimentPctValue11)

plot(BSSentimentPctValue11, main=" Textbook Plot Trajectory chapter 11 PCT Value 20 bins", xlab="Narrative", ylab="Emotional Valence",col="orange")


#Implementing functions of Quanteda package
#1. topfeatures() shows the most frequent features in a dfm. In the below figure we are showing the 10 most frequent 
#words in the dfm. 
top_features <- topfeatures(TextbookDFM, n = 10)
top_features

#2.This function removes terms from a document-feature matrix that appear in fewer than min_docfreq documents or have a term frequency of less than min_termfreq.
TextbookTrimDFM <- dfm_trim(TextbookDFM, min_termfreq = 5, min_docfreq = 2)
TextbookTrimDFM
TextbookDFM

#3. kwic() function is used to retrieve keywords in context. It returns a list 
#of results with the keyword and its context for each occurrence in the corpus.
TextbookTokens_1<-quanteda::tokens(TextbookText$content)
kwic(TextbookTokens_1,pattern="Arizona")
kwic(TextbookTokens_1,pattern="moon")

#4.textstat_simil() - calculates the cosine similarity between pairs of documents in a document-feature matrix (dfm).
# calculate pairwise cosine similarities between documents
my_similarity <- textstat_simil(TextbookDFM, method = "cosine")
my_similarity


#5.textstat_summary() - provides summary statistics for a dfm, including word counts, feature frequencies, and text length measures.

# get summary statistics for a dfm
my_summary <- textstat_summary(TextbookDFM)
my_summary




#IMPLEMENTING FUNCTIONS FROM package syuzhet


#1.get_nrc_sentiment(): This function returns the sentiment of a given text based on 
#the NRC lexicon as a data frame with columns for each sentiment category.
NRCSentiment<-get_nrc_sentiment(BS)
NRCSentiment


barplot(
  sort(colSums(prop.table(NRCSentiment[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Sample text", xlab="Percentage"
)

#2.get_transformed_values: Shape smoothing and normalization using a Fourier based transformation
#and low pass filtering is achieved using the get_transformed_values function 

ft_values <- get_transformed_values(
  BSSentiment, 
  low_pass_size = 3, 
  x_reverse_len = 100,
  padding_factor = 2,
  scale_vals = TRUE,
  scale_range = FALSE
)


plot(
  ft_values, 
  type ="l", 
  main ="Joyce's Portrait using Transformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "blue"
)




#3.The get_dct_transform is similar to get_transformed_values function, but it applies 
#the simpler discrete cosine transformation (DCT) in place of the fast Fourier transform. 
#It’s main advantage is in its better representation of edge values in the smoothed version of the sentiment vector.

dct_values <- get_dct_transform(
  BSSentiment, 
  low_pass_size = 5, 
  x_reverse_len = 100,
  scale_vals = F,
  scale_range = T
)
plot(
  dct_values, 
  type ="l", 
  main ="Joyce's Portrait using Transformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red"
)


#IMPLEMENTING FUNCTIONS FROM package TM

#1.removeSparseTerms() is a function in the tm package that removes terms (words) 
#from a term-document matrix that occur in fewer than a certain proportion of documents. '

#In this case, the second argument 0.2 specifies that terms that appear in less than 20% of the documents will be removed.
Textbooksparse<-removeSparseTerms(TextbookstopTDM, 0.2)
inspect(Textbooksparse)
str(Textbooksparse)

#2.The dictionary parameter in the DocumentTermMatrix function is used to specify a custom dictionary of terms to be used for the creation of the document-term matrix.
Textbookdict<-DocumentTermMatrix(Textbook,list(dictionary = c("arizona", "moon", "mars")))
inspect(Textbookdict)
str(Textbookdict)


#3. stemDocument(): Applies a word stemmer to text documents.
Textbookstemdoc<-tm::tm_map(Textbook,content_transformer(stemDocument))
Textbookstemdoc
str(Textbookstemdoc)
inspect(Textbookstemdoc)

inspect(Textbook)
#The number of characters in each document Textbookstemdoc and Textbook after cleaning was reduced significantly. Therefore, a significant difference has been made 
#by stemming the words.
#Stemming involves removing suffixes and prefixes from words, so that variations of the same word are treated as the same word, despite having different endings or forms.

#4. stripWhitespace(): Removes leading and trailing whitespace from text documents.

Textbookstripwhitespc<-tm::tm_map(Textbook,content_transformer(stripWhitespace))
Textbookstripwhitespc
str(Textbookstripwhitespc)
inspect(Textbookstripwhitespc)

inspect(Textbook)
##The number of characters in each document Textbookstripwhitespc and Textbook  after cleaning was reduced significantly. Therefore, a significant difference has been made 
#by removing white spaces


#IMPLEMENTING FUNCTIONS FROM package WORDCLOUD

#1 Scale the size of the words by a factor of 3
TextbookWC3 <- wordcloud(words, Textbooktf, colors = pal[-(1:4)], scale = c(3, 1))

#2. Only include words that appear at least 10 times
TextbookWC <- wordcloud(words, Textbooktf, colors = pal[-(1:4)], min.freq = 10)

#3. Include a maximum of 50 words
TextbookWC <- wordcloud(words, Textbooktf, colors = pal[-(1:4)], max.words = 50)

#4. Randomly order the words in the cloud
TextbookWC <- wordcloud(words, Textbooktf, colors = pal[-(1:4)], random.order = TRUE)


