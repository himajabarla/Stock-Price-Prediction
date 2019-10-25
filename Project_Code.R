rm(list=ls())
setwd("C:/Users/himaj/OneDrive/Desktop/FIN 6392 - Financial Technology and Blockchain/Project1")

install.packages("TTR")
install.packages("mefa")
install.packages("quantmod")
install.packages("PerformanceAnalytics")
install.packages("SnowballC")
install.packages("tm")
install.packages("twitteR")
install.packages("syuzhet")
install.packages("RWeka")
install.packages("RWekajars")
install.packages("rJava")
# Load Requried Packages
library(SnowballC)
library(tm)
library(twitteR)
library(syuzhet)
library(TTR)
library(mefa)
library(dplyr)
library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)
library(wordcloud)
library(dplyr)
library(stringr)
library(RWeka); library(RWekajars); library(rJava)

sym.vec <- c("^GSPC","T","LUV","XOM")
getSymbols(sym.vec,from="2014-02-12",to="2019-04-02")

##### AT&T #####

# DAILY TRADING DATA #
names(T) <- c("Open", "High","Low","Close","Volume","Adjusted")
T$logReturn <- CalculateReturns(T[,"Adjusted",drop=F],method="log")
T$MarketReturn <- CalculateReturns(GSPC[,"GSPC.Adjusted",drop=F],method="log")
Tchart <- T
T <- data.frame(Date=as.Date(index(T)),coredata(T))
T <- cbind(Company='AT&T',Ticker='T',T,logReturn_future=lead(T$logReturn))
SMA20 <- SMA(T[,"Close"],n=20)
EMA14 <- EMA(T[,"Close"],n=14)
RSI14 <- RSI(T[,"Close"],n=14)
MACD <- MACD(T[,"Close"],nFast=12,nSlow=26,nSig=9,maType=SMA)
T <- data.frame(T,SMA20,EMA14,RSI14,MACD)
T <- na.omit(T); rownames(T) <- NULL


chartSeries(Tchart,subset='2014-04::2019-04',
            type='candlesticks',theme=chartTheme('white',up.col='green',dn.col='red'),
            TA=c(addBBands(n=20,sd=2,),addSMA(n=50,col="blue"),addSMA(n=10,col="black"), 
                 addRSI(n=14),addVo(),addMACD(),addWPR())) 
chartSeries(Tchart,TA="addMACD()")
candleChart(Tchart,multi.col=TRUE,theme="white",subset='2014-04::2019-04')
lineChart(Tchart,line.type='h',TA=NULL,subset='2014-04::2019-04')

# QUARTERLY FUNDAMENTAL DATA #
t <- read.csv('http://www.stockpup.com/data/T_quarterly_financial_data.csv')
names(t) <- gsub(" ",".",names(t))
t$Quarter.end <- as.Date(t$Quarter.end,origin="1899-12-30")
t <- head(t,20)
t <- cbind(Ticker2='T',t)
t <- rep(t,64)
t <- t[order(t$Ticker2,t$Quarter.end),]
rownames(t) <- NULL
t <- t[-c(64,254,255,256,320,510,511,512,704,767,768,832,896,960,1022,1023,1024,1152,1216,1278,1279,1280),]
rownames(t) <- NULL
t <- t[c("Revenue","EPS.basic","Dividend.per.share","Capital.expenditures","ROE","ROA",
         "Dividend.payout.ratio","Long.term.debt.to.equity.ratio","Equity.to.assets.ratio",
         "P.B.ratio","P.E.ratio","Net.margin","Free.cash.flow.per.share")]

# ALPHA MODEL #
att <- cbind(T,t)
write.csv(att,file="T.csv")
att <- read.csv(file="T.csv")

att_model1 <- lm(logReturn ~ MarketReturn, data=att); summary(att_model1)
att_model2 <- lm(logReturn_future ~ Close + logReturn + SMA20 + EMA14 + RSI14 + macd + signal + P.B.ratio + P.E.ratio + ROE + ROA
                                  + Dividend.payout.ratio + Long.term.debt.to.equity.ratio + Equity.to.assets.ratio, data=att); summary(att_model2)



##### Southwest Airlines #####

# DAILY TRADING DATA #
names(LUV) <- c("Open", "High","Low","Close","Volume","Adjusted")
LUV$logReturn <- CalculateReturns(LUV[,"Adjusted",drop=F],method="log")
LUV$MarketReturn <- CalculateReturns(GSPC[,"GSPC.Adjusted",drop=F],method="log")
LUVchart <- LUV
LUV <- data.frame(Date=as.Date(index(LUV)),coredata(LUV))
LUV <- cbind(Company='Southwest Air',Ticker='LUV',LUV,logReturn_future=lead(LUV$logReturn))
SMA20 <- SMA(LUV[,"Close"],n=20)
EMA14 <- EMA(LUV[,"Close"],n=14)
RSI14 <- RSI(LUV[,"Close"],n=14)
MACD <- MACD(LUV[,"Close"],nFast=12,nSlow=26,nSig=9,maType=SMA)
LUV <- data.frame(LUV,SMA20,EMA14,RSI14,MACD)
LUV <- na.omit(LUV); rownames(LUV) <- NULL

chartSeries(LUVchart,subset='2014-04::2019-04',
            type='candlesticks',theme=chartTheme('white',up.col='green',dn.col='red'),
            TA=c(addBBands(n=20,sd=2,),addSMA(n=50,col="blue"),addSMA(n=10,col="black"), 
                 addRSI(n=14),addVo(),addMACD(),addWPR())) 
chartSeries(LUVchart,TA="addMACD()")
candleChart(LUVchart,multi.col=TRUE,theme="white",subset='2014-04::2019-04')
lineChart(LUVchart,line.type='h',TA=NULL,subset='2014-04::2019-04')

# QUARTERLY FUNDAMENTAL DATA #
luv <- read.csv('http://www.stockpup.com/data/LUV_quarterly_financial_data.csv')
names(luv) <- gsub(" ",".",names(luv))
luv$Quarter.end <- as.Date(luv$Quarter.end,origin="1899-12-30")
luv <- luv[-c(1),]
rownames(luv) <- NULL
luv <- head(luv,20)
luv <- rep(luv,64)
luv <- luv[order(luv$Quarter.end),]
rownames(luv) <- NULL
luv <- luv[-c(64,254,255,256,320,510,511,512,704,767,768,832,896,960,1022,1023,1024,1152,1216,1278,1279,1280),]
rownames(luv) <- NULL
luv <- luv[c("Revenue","EPS.basic","Dividend.per.share","Capital.expenditures","ROE","ROA",
             "Dividend.payout.ratio","Long.term.debt.to.equity.ratio","Equity.to.assets.ratio",
             "P.B.ratio","P.E.ratio","Net.margin","Free.cash.flow.per.share")]

# ALPHA MODEL #
southwest <- cbind(LUV,luv)
write.csv(southwest,file="LUV.csv")
southwest <- read.csv(file="LUV.csv")

sw_model1 <- lm(logReturn ~ MarketReturn, data=southwest); summary(sw_model1)
sw_model2 <- lm(logReturn_future ~ Close + logReturn + SMA20 + EMA14 + RSI14 + macd + signal + P.B.ratio + P.E.ratio + ROE + ROA
                 + Revenue + Capital.expenditures + Net.margin + Dividend.per.share + EPS.basic + Free.cash.flow.per.share
                 + Dividend.payout.ratio + Long.term.debt.to.equity.ratio + Equity.to.assets.ratio, data=southwest); summary(sw_model2)

##### Exxon Mobil #####

# DAILY TRADING DATA #
names(XOM) <- c("Open", "High","Low","Close","Volume","Adjusted")
XOM$logReturn <- CalculateReturns(XOM[,"Adjusted",drop=F],method="log")
XOM$MarketReturn <- CalculateReturns(GSPC[,"GSPC.Adjusted",drop=F],method="log")
XOMchart <- XOM
XOM <- data.frame(Date=as.Date(index(XOM)),coredata(XOM))
XOM <- cbind(Company='Exxon Mobil',Ticker='XOM',XOM,logReturn_future=lead(XOM$logReturn))
SMA20 <- SMA(XOM[,"Close"],n=20)
EMA14 <- EMA(XOM[,"Close"],n=14)
RSI14 <- RSI(XOM[,"Close"],n=14)
MACD <- MACD(XOM[,"Close"],nFast=12,nSlow=26,nSig=9,maType=SMA)
XOM <- data.frame(XOM,SMA20,EMA14,RSI14,MACD)
XOM <- na.omit(XOM); rownames(XOM) <- NULL

chartSeries(XOMchart,subset='2014-04::2019-04',
            type='candlesticks',theme=chartTheme('white',up.col='green',dn.col='red'),
            TA=c(addBBands(n=20,sd=2,),addSMA(n=50,col="blue"),addSMA(n=10,col="black"), 
                 addRSI(n=14),addVo(),addMACD(),addWPR())) 
chartSeries(XOMchart,TA="addMACD()")

candleChart(XOMchart,multi.col=TRUE,theme="white",subset='2014-04::2019-04')
lineChart(XOMchart,line.type='h',TA=NULL,subset='2014-04::2019-04')

# QUARTERLY FUNDAMENTAL DATA #
xom <- read.csv('http://www.stockpup.com/data/XOM_quarterly_financial_data.csv')
names(xom) <- gsub(" ",".",names(xom))
xom$Quarter.end <- as.Date(xom$Quarter.end,origin="1899-12-30")
xom <- head(xom,20)
xom <- rep(xom,64)
xom <- xom[order(xom$Quarter.end),]
rownames(xom) <- NULL
xom <- xom[-c(64,254,255,256,320,510,511,512,704,767,768,832,896,960,1022,1023,1024,1152,1216,1278,1279,1280),]
rownames(xom) <- NULL
xom <- xom[c("Revenue","EPS.basic","Dividend.per.share","Capital.expenditures","ROE","ROA",
             "Dividend.payout.ratio","Long.term.debt.to.equity.ratio","Equity.to.assets.ratio",
             "P.B.ratio","P.E.ratio","Net.margin","Free.cash.flow.per.share")]

# APLHA MODEL #
exxon <- cbind(LUV,luv)
write.csv(exxon,file="XOM.csv")
exxon <- read.csv(file="XOM.csv")

exxon_model1 <- lm(logReturn ~ MarketReturn, data=exxon); summary(exxon_model1)
exxon_model2 <- lm(logReturn_future ~ Close + logReturn + SMA20 + EMA14 + RSI14 + macd + signal + P.B.ratio + P.E.ratio + ROE + ROA
                                    + Revenue + Capital.expenditures + Net.margin + Dividend.per.share + EPS.basic + Free.cash.flow.per.share
                                    + Dividend.payout.ratio + Long.term.debt.to.equity.ratio + Equity.to.assets.ratio, data=exxon); summary(exxon_model2)
## Twitter Data ##
consumer_key <- "iTbfqsr5L9vxB3sHHIxi8kGaB"
consumer_secret <- "nMKCpCrNpLZMA2OYPUhgvxzHVfWemJIaZtkPCgbt3yHH7p7cmN"
access_token <- "1181595748443578368-7VlTpqbUYLzn8HQ86eCVLq8BXCdoA6"
access_secret <- "cUU1S816itfxHmv0mo9aEfinDpHj1QTIrYpKpwUIa7zL3"
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)

LUV_tweets1 <- searchTwitter("SouthwestAir",n=10000,since='2019-10-01',until='2019-10-23')
LUV_tweets2 <- searchTwitter("SouthwestAir",n=10000,since='2019-10-01',until='2019-10-14')
LUV_tweets.df <- twListToDF(LUV_tweets1)
min(LUV_tweets.df$created)

write.csv(LUV_tweets.df, file = "LUVtweets.csv",append=TRUE) #append, or use cat(), rbind()
tweets <- read.csv("LUVtweets.csv", stringsAsFactors = FALSE)

#count number of tweets
tweets$timestamp <- ymd_hms(tweets$created)
#tweets$timestamp <- with_tz(tweets$timestamp, "America/Chicago")
ggplot(data = tweets, aes(x = timestamp)) +
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") +
  xlab("Time") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")


list<-read.table("LUVtweets.csv", sep="\t")
head(list, n=5)
score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, pos.words, neg.words){
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- gsub('\\d+', "", sentence)
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}

pos <- scan('C:/Users/himaj/OneDrive/Desktop/FIN 6392 - Financial Technology and Blockchain/Week5&6/positive-words.txt', what='character', comment.char=';') #folder with positive dictionary
neg <- scan('C:/Users/himaj/OneDrive/Desktop/FIN 6392 - Financial Technology and Blockchain/Week5&6/negative-words.txt', what='character', comment.char=';') #folder with negative dictionary
pos.words <- c(pos, 'upgrade')
neg.words <- c(neg, 'hack', 'breach', 'cybersecurity')
Dataset <- list
Dataset$text <- as.factor(Dataset[,1])
scores <- score.sentiment(Dataset$text, pos.words, neg.words)
scores$score
 
library(syuzhet)
mySentiment <- get_nrc_sentiment(tweets$text)
head(mySentiment)
tweets <- cbind(tweets, mySentiment)
sentimentTotals <- data.frame(colSums(tweets[,c(19:27)],dim=1)) 
names(sentimentTotals) <- "count"
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
rownames(sentimentTotals) <- NULL
ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All Tweets")

fitTweets <- read.csv("LUVtweets.csv", stringsAsFactors = FALSE)
myCorpus <- Corpus(VectorSource(fitTweets$text))

myCorpus <- tm_map(myCorpus, tolower)
myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, removeNumbers)
inspect(myCorpus[1:3]) 
myStopwords <- c(stopwords('english'), "available", "via")
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
summary(myStopwords)

dictCorpus <- myCorpus

myCorpus <- tm_map(myCorpus, stemDocument)
inspect(myCorpus[1:3])
 
myDtm <- DocumentTermMatrix(myCorpus, control = list(minWordLength = 1))
myTdm <- TermDocumentMatrix(myCorpus, control=list(wordLengths=c(1,Inf)))

idx <- which(dimnames(myTdm)$Terms == "fit")
inspect(myTdm[idx+(0:5),1:10])

findFreqTerms(myTdm, lowfreq=10)
termFrequency <- rowSums(as.matrix(myTdm))
termFrequency <- subset(termFrequency, termFrequency>=10)
df <- data.frame(term=names(termFrequency), freq=termFrequency)
barplot(termFrequency, las=2)
findAssocs(myTdm,'fit',0.1)
findAssocs(myTdm, 'wearable', 0.1)

m <- as.matrix(myTdm)
wordFreq <- sort(rowSums(m),decreasing=TRUE)
set.seed(375)
grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=3, random.order=F,colors=grayLevels)

LUV_tweets.df2 <- gsub("http.*","",LUV_tweets.df$text)
LUV_tweets.df2 <- gsub("https.*","",LUV_tweets.df2)
LUV_tweets.df2 <- gsub("#.","",LUV_tweets.df2)
LUV_words.df <- as.vector(LUV_tweets.df2)
LUV_emotion.df <- get_nrc_sentiment(LUV_words.df)
LUV_emotion.df2 <- cbind(LUV_tweets.df2,LUV_emotion.df) 
LUV_senti.value <- get_sentiment(LUV_words.df)
LUV_most.positive <- LUV_words.df[LUV_senti.value == max(LUV_senti.value)]; LUV_most.positive
LUV_most.negative <- LUV_words.df[LUV_senti.value <= min(LUV_senti.value)]; LUV_most.negative
LUV_senti_category <- ifelse(LUV_senti.value<0,"Negative",ifelse(LUV_senti.value>0,"Positive","Neutral"))
LUV_senti_category2 <- cbind(LUV_tweets.df2,LUV_senti_category,LUV_senti.value)
table(LUV_senti_category)

LUV_tweets2 <- userTimeline("SouthwestAir",n=5000)
LUV_tweets.df <- twListToDF(LUV_tweets2)
LUV_tweets.df2 <- gsub("http.*","",LUV_tweets.df$text)
LUV_tweets.df2 <- gsub("https.*","",LUV_tweets.df2)
LUV_tweets.df2 <- gsub("#.","",LUV_tweets.df2)
LUV_tweets.df$created

LUV_words.df <- as.vector(LUV_tweets.df2)
LUV_emotion.df <- get_nrc_sentiment(LUV_words.df)
LUV_emotion.df2 <- cbind(LUV_tweets.df2,LUV_emotion.df) 
LUV_senti.value <- get_sentiment(LUV_words.df)
LUV_most.positive <- LUV_words.df[LUV_senti.value == max(LUV_senti.value)]; LUV_most.positive
LUV_most.negative <- LUV_words.df[LUV_senti.value <= min(LUV_senti.value)]; LUV_most.negative
LUV_senti_category <- ifelse(LUV_senti.value<0,"Negative",ifelse(LUV_senti.value>0,"Positive","Neutral"))
LUV_senti_category2 <- cbind(LUV_tweets.df2,LUV_senti_category,LUV_senti.value)
table(LUV_senti_category)

## Performance Analysis ##

Tchart <- na.omit(Tchart)
LUVchart <- na.omit(LUVchart)
XOMchart <- na.omit(XOMchart)


names(GSPC) <- c("Open", "High","Low","Close","Volume","Adjusted")
GSPC$logReturn <- CalculateReturns(GSPC[,"Adjusted",drop=F],method="log")
GSPCchart <- GSPC
GSPCchart <- na.omit(GSPCchart)
GSPCchart1 <- data.frame(Date=as.Date(index(GSPCchart)),coredata(GSPCchart))
GSPCchart$sma20 <- SMA(GSPCchart1[c('Close')],n=20)
GSPCchart$ema20 <- EMA(GSPCchart1[c('Close')],n=14)
GSPCchart$rsi <- RSI(GSPCchart1[c('Close')],n=14)
macd <- MACD(GSPCchart1[c('Close')], nFast=12, nSlow=26, nSig=9, maType=SMA)
GSPCchart <-cbind(GSPCchart,macd)
GSPCchart <- na.omit(GSPCchart)


Tchart1 <- data.frame(Date = as.Date(index(Tchart)), coredata(Tchart))
Tchart$sma20 <- SMA(Tchart1[c('Close')],n=20)
Tchart$ema20 <- EMA(Tchart1[c('Close')],n=14)
Tchart$rsi <- RSI(Tchart1[c('Close')],n=14)
macd <- MACD(Tchart1[c('Close')], nFast=12, nSlow=26, nSig=9, maType=SMA)
Tchart <-cbind(Tchart,macd)
Tchart <- na.omit(Tchart)


LUVchart1 <- data.frame(Date = as.Date(index(LUVchart)), coredata(LUVchart))
LUVchart$sma20 <- SMA(LUVchart1[c('Close')],n=20)
LUVchart$ema20 <- EMA(LUVchart1[c('Close')],n=14)
LUVchart$rsi <- RSI(LUVchart1[c('Close')],n=14)
macd1 <- MACD(LUVchart1[c('Close')], nFast=12, nSlow=26, nSig=9, maType=SMA)
LUVchart <-cbind(LUVchart,macd1)
LUVchart <- na.omit(LUVchart)


XOMchart1 <- data.frame(Date = as.Date(index(XOMchart)), coredata(XOMchart))
XOMchart$sma20 <- SMA(XOMchart1[c('Close')],n=20)
XOMchart$ema20 <- EMA(XOMchart1[c('Close')],n=14)
XOMchart$rsi <- RSI(XOMchart1[c('Close')],n=14)
macd2 <- MACD(XOMchart1[c('Close')], nFast=12, nSlow=26, nSig=9, maType=SMA)
XOMchart <-cbind(XOMchart,macd2)
XOMchart <- na.omit(XOMchart)

returns <- cbind(Tchart$logReturn,Tchart$MarketReturn,LUVchart$logReturn,LUVchart$MarketReturn,XOMchart$logReturn,XOMchart$MarketReturn)
names(returns) <- c("Treturn","Tmktret","LUVreturn","LUVmktret","XOMreturn","XOMmktret");returns <- returns[-1,]

ta <- cbind(Tchart$sma20,Tchart$ema20,Tchart$rsi,Tchart$macd,LUVchart$sma20,LUVchart$ema20,LUVchart$rsi,LUVchart$macd,XOMchart$sma20,XOMchart$ema20,XOMchart$rsi,XOMchart$macd,GSPCchart$sma20,GSPCchart$ema20,GSPCchart$rsi,GSPCchart$macd)
names(ta) <- c("Tsma","Tema","Trsi","Tmacd","LUVsma","LUVema","LUVrsi","LUVmacd","XOMsma","XOMema","XOMrsi","XOMmacd","GSPCsma","GSPCema","GSPCrsi","GSPCmacd")

sd.T <-sd(returns$Treturn)*sqrt(252) # 0.1751
sd.LUV <-sd(returns$LUVreturn)*sqrt(252) # 0.2872
sd.XOM <-sd(returns$XOMreturn)*sqrt(252) # 0.1868

ret.cov1 <-cov(returns$Treturn,returns$LUVreturn)*252 # 0.0104
ret.cov2 <-cov(returns$Treturn,returns$XOMreturn)*252 # 0.0121
ret.cov3 <-cov(returns$XOMreturn,returns$LUVreturn)*252 # 0.0101

wgt.T = 0.20; wgt.LUV=0.30; wgt.XOM=0.50 #asset allocation

# Three Asset Portfolio Risk
#??2= w12??12 + w22??22 + w32??32+ 2w1w2??1??2??1,2+ 2w2w3??2??3??2,3+ 2w1w3??1??3??1,3
#??2= w12??12 + w22??22 + w32??32+ 2w1w2COV(1,2)+ 2w2w3COV(2,3)+ 2w1w3COV(1,3)

port.var <- wgt.T^2*sd.T^2 + wgt.LUV^2*sd.LUV^2 + wgt.XOM^2*sd.XOM^2 + 2*ret.cov1*wgt.T*wgt.LUV + 2*ret.cov2*wgt.T*wgt.XOM + 2*ret.cov3*wgt.XOM*wgt.LUV
# portfolio variance = 0.0241

port.sd<-sqrt(port.var) #portfolio risk = 0.155

#Value-at-risk measure VaR and expected shortfall ES - the expected loss if the losses exceed VaR
#manual calculation VaR and ES

#T
T.mean <-mean(returns$Treturn)
T.risk <-sd(returns$Treturn)
VaR05.Gaussian.T <--(T.mean+T.risk*qnorm(0.05))*10000 #5% chance of losing $179.64 over the next day with $10K position
ES05.Gaussian.T <-10000*(T.mean+T.risk*dnorm(qnorm(0.05))/0.05) #5% chance the losses exceed VaR, and when it ocurs, on average, thenloss is $229.403
#LUV
LUV.mean <-mean(returns$LUVreturn)
LUV.risk <-sd(returns$LUVreturn)
VaR05.Gaussian.LUV <--(LUV.mean+LUV.risk*qnorm(0.05))*10000 #5% chance of losing $290.2 over the next day with $10K position
ES05.Gaussian.LUV <-10000*(LUV.mean+LUV.risk*dnorm(qnorm(0.05))/0.05) #5% chance the losses exceed VaR, and when it ocurs, on average, thenloss is $380.5
#XOM
XOM.mean <-mean(returns$XOMreturn)
XOM.risk <-sd(returns$XOMreturn)
VaR05.Gaussian.XOM <--(XOM.mean+XOM.risk*qnorm(0.05))*10000 #5% chance of losing $193.05 over the next day with $10K position
ES05.Gaussian.XOM <-10000*(XOM.mean+XOM.risk*dnorm(qnorm(0.05))/0.05) #5% chance the losses exceed VaR, and when it ocurs, on average, thenloss is $243.31


#T
Return.annualized(returns$Treturn) # 0.03122413
maxDrawdown(returns$Treturn) # 0.3235255
SharpeRatio.annualized(returns$Treturn) # 0.178284
SharpeRatio(returns$Treturn)
VaR(returns$Treturn, 0.05,method="gaussian") # -0.01795672
KellyRatio(returns$Treturn) # 0.7527922

#LUV
Return.annualized(returns$LUVreturn) # 0.1547738
maxDrawdown(returns$LUVreturn) # 0.3478414
SharpeRatio.annualized(returns$LUVreturn) # 0.5389614
SharpeRatio(returns$LUVreturn)
VaR(returns$LUVreturn, 0.05,method="gaussian") # -0.02900824
KellyRatio(returns$LUVreturn) # 1.124076

#XOM
Return.annualized(returns$XOMreturn) # -0.003788474
maxDrawdown(returns$XOMreturn) # 0.3300986
SharpeRatio.annualized(returns$XOMreturn) # -0.02027659
SharpeRatio(returns$XOMreturn)
VaR(returns$XOMreturn, 0.05,method="gaussian") # -0.01929786
KellyRatio(returns$XOMreturn) # 0.1957978


# the 2nd argument is the benchmark
InformationRatio(returns$Treturn,returns$Tmktret,scale=252) # -0.3662022
InformationRatio(returns$LUVreturn,returns$LUVmktret,scale=252) # 0.2088342
InformationRatio(returns$XOMreturn,returns$XOMmktret,scale=252) #  -0.6677711

chart.VaRSensitivity(returns$Treturn) #chart includes VaR, ES
chart.VaRSensitivity(returns$LUVreturn)
chart.VaRSensitivity(returns$XOMreturn)

charts.PerformanceSummary(returns$Treturn,methods="GaussianES")
charts.PerformanceSummary(returns$LUVreturn,methods="GaussianES")
charts.PerformanceSummary(returns$XOMreturn,methods="GaussianES")

