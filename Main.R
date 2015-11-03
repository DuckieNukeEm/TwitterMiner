# Authenticatng
  #do it yourself
# Setting the working directory
  source("c:/scripts/R/TWitterMiner.R")
  wd <- getwd()
  wdcode <- file.path(wd, 'R')
  wddata <- file.path(wd, 'Data')
  
# activating the librarys
  pkg <- c("twitteR", "plyr","stringr","tm","wordcloud","RColorBrewer","ggplot2")
  inst <- pkg %in% installed.packages()  
  if(length(pkg[!inst]) > 0) install.packages(pkg[!inst])  
  lapply(pkg,library,character.only=TRUE)  
  rm(inst,pkg)  
  
# Setting control Variables
  verbose = TRUE
  wordcloude = FALSE
  TweetCounts = 1500
  tweets.tweets <- list()
  tweetChar <- c("@wm","@Walmart", "@wal-mart")  

#authenticating to R
  source(file.path(wdcode, "TwitterAuth.R"))
  
# Loading the R functions, switching to verb_print to define sections  
if(verbose){print("Loading R functions")}
  source(file.path(wdcode, 'TwitterFunctions.R') )

vprint("Loading the data sources now")
    pos_list = scan(file.path(wddata, "hu_positive_words.txt"), what='character', comment.char=';')
    neg_list = scan(file.path(wddata, "hu_negative_words.txt"), what='character', comment.char=';')

    
vprint("scrapping R now")
      for(i in tweetChar)
      { verb_print(i)
        tweets.tweets <- c(tweets.tweets, searchTwitter(i, n=TweetCounts))}
      
    #verb_print(paste(c("Scrapped ", length(wm.tweets),". ",length(wm.tweets)/(1500*2), "% efficent" ), sep ="")

      
vprint("Cleaning the tweets")      
      tweets.tweets <- strip_retweets(tweets.tweets,strip_manual=TRUE,strip_mt=TRUE)
      
      
vprint("Converting tweets to text")
      tweets.text = laply(tweets.tweets, function(t) t$getText() )
      
vprint("Scoring Tweets")      
      tweets.score <- score.sentiment(tweets.text, pos_list, neg_list, .progress='text')
      
vprint("histogram time")      
      hist(tweets.score$score)

vprint("tweets_to_score function engaged")   
    score2<- data.frame(score = as.integer(), text =as.character(), code = as.character())
    for(i in tweetChar)
    { verb_print(i)
      score2 <- rbind(score2, tweet_to_score(i, pos.word =  pos_list, neg.word = neg_list, .progress='text'))}

if(wordcloude){
vprint("trying a wordcloud thingy")    
#cleaning sentence
tweets.text <- lapply(tweets.text, clean_sentence, remove.words = c("walmart","wal","@WalMart","WalMart", "@Walmart","Walmart","WM","wm","Wal-Mart","Mart","mart"))
#In tm package, the documents are managed by a structure called Corpus
tweets.corpus = Corpus(VectorSource(tweets.text))

#Create a term-document matrix from a corpus
tweets.tdm = TermDocumentMatrix(tweets.corpus,control = list(removePunctuation = TRUE,stopwords = c("WM", "walmart","WalMart","Wal-Mart","WalMart", stopwords("english")), removeNumbers = TRUE, tolower = F))

#Convert as matrix
tweets.tdm = as.matrix(tweets.tdm)

#Get word counts in decreasing order
tweets.word_freqs = sort(rowSums(tweets.tdm), decreasing=TRUE) 

#Create data frame with words and their frequencies
dm = data.frame(word=names(tweets.word_freqs), freq=tweets.word_freqs)

#Plot wordcloud
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

}