#######################################################################
#This file contains the functions that will be needed to mine Twitter
#######################################################################

#This is the print function for the verbose
verb_print <- function(...){
 if(verbose) {print(...)}
  }

#transfomes words

#This function will score a sentence
calc_score <- function(sentence, pos.words, neg.words) {
  # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
  #removing icons
    sentence = iconv(sentence, "latin1", "ASCII", sub="")
  # and convert to lower case:
    sentence = try(tolower(sentence), silent = T)
  
  # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
  # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
  
  # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
  
  # match() returns the position of the matched term or NA
  # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
  
  # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
  
  return(score)
}

#This functions returns a data.frame for sentence(s)
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores = laply(sentences,calc_score, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}


#this function goes all the wany and generates the score
tweet_to_score<- function(x, pos.words, neg.words, .progress ='none')
{tweets.tweets <-  searchTwitter(i, n=TweetCounts)
 tweets.tweets <-  strip_retweets(tweets.tweets,strip_manual=TRUE,strip_mt=TRUE)
 tweets.tweets = laply(tweets.tweets, function(t) t$getText() )
 tweets.tweets <- score.sentiment(tweets.tweets, pos.words, neg.words, .progress)
 tweets.tweets$code <- x
 return(tweets.tweets)
 
}