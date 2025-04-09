#Train model on all of data for Shiny app

library(dplyr)
library(stringr)
library(tidytext)
library(stopwords)
library(ggplot2)

true = read.csv("C:\\temp\\News _dataset\\True.csv") %>% mutate(true=1)
fake = read.csv("C:\\temp\\News _dataset\\Fake.csv") %>% mutate(true=0)
news = rbind(true,fake) %>% mutate(articlenum=row_number())

news <- news %>% 
  mutate(text2=paste(title,text)) %>%
  mutate(text2=str_replace_all(text2, "'", "")) %>%
  mutate(text2=str_replace_all(text2, "&", "")) %>%
  mutate(text2=str_replace_all(text2, "’", "")) %>%
  mutate(text2=str_replace_all(text2, "/", "")) %>%
  mutate(text2=str_replace_all(text2, "_", ""))

##########################################################

#identify words disproportionately in either true or fake articles
ntruewords=500
nexcludefake=2500

nfakewords=1000
nexcludetrue=6000

news1 <- news %>% 
  select(articlenum, true, text2) %>%
  unnest_tokens(output = word, input = text2) %>%
  mutate(word=str_extract(word, "[a-z']+")) %>%
  subset(str_length(word)>=2)
truewords = subset(news1,true==1) %>% group_by(word) %>% summarize(ntrue=n()) %>% 
  arrange(desc(ntrue)) %>% mutate(ranktrue=row_number())
fakewords = subset(news1,true==0) %>% group_by(word) %>% summarize(nfake=n()) %>% 
  arrange(desc(nfake)) %>% mutate(rankfake=row_number())
truewords = truewords %>% head(nexcludetrue)
fakewords = fakewords %>% head(nexcludefake)
truewords1 = head(truewords,ntruewords) %>% anti_join(fakewords, by='word') %>% rename(n=ntrue)
fakewords1 = head(fakewords,nfakewords) %>% anti_join(truewords, by='word') %>% rename(n=nfake)
truewords1 = subset(truewords1, word != 'reuters')

write.csv(truewords1$word, file='truewords.csv')
write.csv(fakewords1$word, file='fakewords.csv')

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

news=define_vars(news)

m=glm(true~n_stopwords+prop_stopwords+n_negativewords+prop_negativewords+
        n_positivewords+prop_positivewords+n_fakenewswords+prop_fakenewswords+
        n_truenewswords+prop_truenewswords, data=news, family='binomial')

save(m, file='logisticModel')


wordplot = function(df, title) {
df = df %>% arrange(n)
df$word = factor(df$word, levels=df$word)
p=ggplot(data=df, mapping=aes(x=word,y=n)) + geom_col(color='black', fill='blue') 
p + coord_flip() + labs(title=title)
}
p1 = wordplot(fakewords1, 'Words common in fake news articles but not real news articles')
p2 = wordplot(truewords1, 'Words common in real news articles but not fake news articles')


boxplot = function(df, title) {
df$true = factor(df$true, levels=0:1, labels=c('Fake news', 'Real news'))
p=ggplot(data=df, mapping=aes(x=sentiment, group=true, color=true)) + geom_boxplot()
p + coord_flip() + labs(title=title)
}
p3 = boxplot(news, "Sentiment")

t1 = news %>%
group_by(true) %>% summarize(Minimum=min(sentiment), 
                             Maximum=max(sentiment), Mean=mean(sentiment), 
                             Median=median(sentiment),
                             `Standard Deviation` = sqrt(var(sentiment))) %>%
  ungroup() %>% as.data.frame() %>% mutate(` `=case_when(true==0~'Fake news', T~'Real news'),
                                           Mean=round(Mean, digits=2),
                                           `Standard Deviation`=round(`Standard Deviation`,digits=2)) %>%
  select(` `, Mean, `Standard Deviation`, Median, Minimum, Maximum)


ggsave('plot1.jpg', p1)
ggsave('plot2.jpg', p2)
ggsave('plot3.jpg', p3)
write.csv(t1, 'table.csv')

