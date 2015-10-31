# Authenticatng
  #do it yourself
# Setting the working directory
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
  TweetCounts = 1500
  tweets.tweets <- list()
  tweetChar <- c("@wm","@Walmart", "@wal-mart")  
# Loading the R functions, switching to verb_print to define sections  
if(verbose){print("Loading R functions")}
  source(file.path(wdcode, 'TwitterFunctions.R') )

verb_print("Loading the data sources now")
    pos_list = scan(file.path(wddata, "hu_positive_words.txt"), what='character', comment.char=';')
    neg_list = scan(file.path(wddata, "hu_negative_words.txt"), what='character', comment.char=';')

    
verb_print("scrapping R now")
      for(i in tweetChar)
      { verb_print(i)
        tweets.tweets <- c(tweets.tweets, searchTwitter(i, n=TweetCounts))}
      
    #verb_print(paste(c("Scrapped ", length(wm.tweets),". ",length(wm.tweets)/(1500*2), "% efficent" ), sep ="")

      
verb_print("Cleaning the tweets")      
      tweets.tweets <- strip_retweets(tweets.tweets,strip_manual=TRUE,strip_mt=TRUE)
      
      
verb_print("Converting tweets to text")
      tweets.text = laply(tweets.tweets, function(t) t$getText() )
      
verb_print("Scoring Tweets")      
      tweets.score <- score.sentiment(tweets.text, pos_list, neg_list, .progress='text')
      
verb_print("histogram time")      
      hist(tweets.score$score)

verb_print("tweets to score now")   
    score2<- data.frame(score = as.integer(), text =as.character(), code = as.character())
      for(i in tweetChar)
    { verb_print(i)
      score2 <- rbind(score2, tweet_to_score(i, pos.word =  pos_list, neg.word = neg_list, .progress='text'))}

verb_print("trying a wordcloud thingy")    

#In tm package, the documents are managed by a structure called Corpus
myCorpus = Corpus(VectorSource(tweets.text))

#Create a term-document matrix from a corpus
tdm = TermDocumentMatrix(myCorpus,control = list(removePunctuation = TRUE,stopwords = c("new", "year", stopwords("english")), removeNumbers = TRUE, tolower = FALSE))

#Convert as matrix
m = as.matrix(tdm)

#Get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE) 

#Create data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)

#Plot wordcloud
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))

      