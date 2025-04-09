#https://cran.r-project.org/web/packages/tidytext/vignettes/tidytext.html
#Train model on 70% and test on 30% to determine how well it predicts

library(dplyr)
library(stringr)
library(tidytext)
library(stopwords)
library(pROC)
library(ggplot2)

#Download data from https://www.kaggle.com/datasets/emineyetm/fake-news-detection-datasets
true = read.csv("News _dataset/True.csv") %>% mutate(true=1)
fake = read.csv("News _dataset/Fake.csv") %>% mutate(true=0)
fake_split = read.csv("fake_split.csv")
true_split = read.csv("true_split.csv")
true$split = true_split$split
fake$split = fake_split$split

count(true,split)$n/nrow(true)
count(fake,split)$n/nrow(fake)

news = rbind(true,fake) %>% mutate(articlenum=row_number())

news <- news %>% 
  mutate(text2=paste(title,text)) %>%
  mutate(text2=str_replace_all(text2, "'", "")) %>%
  mutate(text2=str_replace_all(text2, "&", "")) %>%
  mutate(text2=str_replace_all(text2, "’", "")) %>%
  mutate(text2=str_replace_all(text2, "/", "")) %>%
  mutate(text2=str_replace_all(text2, "_", ""))

newstrain = news %>% filter(split=='train')
newstest = news %>% filter(split=='test')

count(newstrain,true)
count(newstest,true)

##########################################################

#identify words disproportionately in either true or fake articles
ntruewords=500
nexcludefake=2500

nfakewords=1000
nexcludetrue=6000

news1 <- newstrain %>% 
  select(articlenum, true, text2) %>%
  unnest_tokens(output = word, input = text2) %>%
  mutate(word=str_extract(word, "[a-z']+")) %>%
  subset(str_length(word)>=2)
truewords = subset(news1,true==1) %>% group_by(word) %>% summarize(n=n()) %>% 
  arrange(desc(n)) %>% mutate(ranktrue=row_number())
fakewords = subset(news1,true==0) %>% group_by(word) %>% summarize(n=n()) %>% 
  arrange(desc(n)) %>% mutate(rankfake=row_number())
truewords = truewords %>% head(nexcludetrue)
fakewords = fakewords %>% head(nexcludefake)
truewords1 = head(truewords,ntruewords) %>% anti_join(fakewords, by='word')
fakewords1 = head(fakewords,nfakewords) %>% anti_join(truewords, by='word')
truewords1 = subset(truewords1, word != 'reuters')

inlist = function(df,listofwords,suffix) {
df = df %>% mutate(inlistofwords=case_when(word %in% listofwords ~ 1, T~0))
df1 = df %>% group_by(articlenum) %>% summarize(n1=sum(inlistofwords),prop=n1/n()) %>% ungroup %>% as.data.frame()
colnames(df1) = c('articlenum', paste0('n_',suffix), paste0('prop_',suffix))
df1
}

stopwords = get_stopwords()
negativewords = get_sentiments("bing") %>% filter(sentiment == "negative")
positivewords = get_sentiments("bing") %>% filter(sentiment == "positive")

define_vars = function(news) {
news1 <- news %>% 
  select(articlenum, true, text2) %>%
  unnest_tokens(output = word, input = text2) %>%
  mutate(word=str_extract(word, "[a-z']+")) %>%
  subset(str_length(word)>=2)

prop1=inlist(news1, stopwords$word, 'stopwords')
prop2=inlist(news1, negativewords$word, 'negativewords')
prop3=inlist(news1, positivewords$word, 'positivewords')
prop4=inlist(news1, fakewords1$word, 'fakenewswords')
prop5=inlist(news1, truewords1$word, 'truenewswords')

news_final = merge(news,prop1,by='articlenum',all.x=T)
news_final = merge(news_final,prop2,by='articlenum',all.x=T)
news_final = merge(news_final,prop3,by='articlenum',all.x=T)
news_final = merge(news_final,prop4,by='articlenum',all.x=T)
news_final = merge(news_final,prop5,by='articlenum',all.x=T)

news_final = news_final %>% mutate(sentiment=n_positivewords-n_negativewords)

news_final
}

newstrain=define_vars(newstrain)
newstest=define_vars(newstest)



acc=function(y,pred){
  cutoffs=seq(0,1,by=0.01)
  accuracy = rep(NA, length(cutoffs))
  for (i in 1:length(cutoffs)) {
    cutoff = cutoffs[i]
    pred = data.frame(pred,y)
    pred1 = pred %>% mutate(ypred=case_when(pred>=cutoff~1,T~0))
    accuracy[i] = nrow(subset(pred1, y==ypred))/nrow(pred1)
  }
  accuracy = data.frame(cutoffs,accuracy) %>% arrange(desc(accuracy))
  head(accuracy,1)
}


m=glm(true~n_stopwords+prop_stopwords+n_negativewords+prop_negativewords+
        n_positivewords+prop_positivewords+n_fakenewswords+prop_fakenewswords+
        n_truenewswords+prop_truenewswords, data=newstrain, family='binomial')
ptest = predict.glm(m, newstest, type='response')
auc(newstest$true,ptest)
acc(newstest$true,ptest)


m=glm(true~n_stopwords+prop_stopwords, data=newstrain, family='binomial')
ptest = predict.glm(m, newstest, type='response')
auc(newstest$true,ptest)
m=glm(true~n_negativewords+prop_negativewords, data=newstrain, family='binomial')
ptest = predict.glm(m, newstest, type='response')
auc(newstest$true,ptest)
m=glm(true~n_positivewords+prop_positivewords, data=newstrain, family='binomial')
ptest = predict.glm(m, newstest, type='response')
auc(newstest$true,ptest)
m=glm(true~n_fakenewswords+prop_fakenewswords, data=newstrain, family='binomial')
ptest = predict.glm(m, newstest, type='response')
auc(newstest$true,ptest)
m=glm(true~n_truenewswords+prop_truenewswords, data=newstrain, family='binomial')
ptest = predict.glm(m, newstest, type='response')
auc(newstest$true,ptest)



