library(dplyr)
library(stringr)
library(tidytext)
library(stopwords)
library(pROC)
library(shiny)

stopwords = get_stopwords()
negativewords = get_sentiments("bing") %>% filter(sentiment == "negative")
positivewords = get_sentiments("bing") %>% filter(sentiment == "positive")
truewords = c("eu", "britain", "parliament", "ministry", "myanmar", "missile", "korean", "independence", "nov", "brexit", "tillerson", "kurdish", "rohingya", "militants", "declined", "regional", "merkel")
fakewords = c("pic", "gop", "wire", "https", "isn", "featured", "wasn", "rep", "screen", "youtube", "couldn", "wouldn", "literally", "hannity", "aren")

load('logisticModel')

inlist = function(df,listofwords,suffix) {
  df = df %>% mutate(inlistofwords=case_when(word %in% listofwords ~ 1, T~0))
  df1 = df %>% group_by(articlenum) %>% summarize(n1=sum(inlistofwords),prop=n1/n()) %>% ungroup %>% as.data.frame()
  colnames(df1) = c('articlenum', paste0('n_',suffix), paste0('prop_',suffix))
  df1
}

fakenews = function(text) {
  news <- data.frame(articlenum=1, text2=text) %>%
    mutate(text2=str_replace_all(text2, "'", "")) %>%
    mutate(text2=str_replace_all(text2, "&", "")) %>%
    mutate(text2=str_replace_all(text2, "’", "")) %>%
    mutate(text2=str_replace_all(text2, "/", "")) %>%
    mutate(text2=str_replace_all(text2, "_", ""))
  
  news1 <- news %>% 
    select(articlenum, text2) %>%
    unnest_tokens(output = word, input = text2) %>%
    mutate(word=str_extract(word, "[a-z']+")) %>%
    subset(str_length(word)>=2)
  
  prop1=inlist(news1, stopwords$word, 'stopwords')
  prop2=inlist(news1, negativewords$word, 'negativewords')
  prop3=inlist(news1, positivewords$word, 'positivewords')
  prop4=inlist(news1, fakewords, 'fakenewswords')
  prop5=inlist(news1, truewords, 'truenewswords')
  
  news_final = merge(news,prop1,by='articlenum',all.x=T)
  news_final = merge(news_final,prop2,by='articlenum',all.x=T)
  news_final = merge(news_final,prop3,by='articlenum',all.x=T)
  news_final = merge(news_final,prop4,by='articlenum',all.x=T)
  news_final = merge(news_final,prop5,by='articlenum',all.x=T)
  
  news_final = news_final %>% mutate(sentiment=n_positivewords-n_negativewords)
  p = predict.glm(m, news_final, type='response')
  res = list()
  res$df = news_final
  res$p = p
  res
}

ui <- fluidPage(
  textAreaInput('text', 'Enter news story', '', width='700px', height='350px'),
  actionButton('button', 'Submit'), 
  htmlOutput(outputId = "result")
)
server <- function(input, output, session) {
  re = eventReactive(input$button,{input$text})
  output$result <- renderText({
  	str = ''
	res = fakenews(re())
	if (is.na(res$p)) {prediction=""}
	else if (res$p>=0.5) {prediction = 'True Story'}
	else {prediction = 'Fake News'}
	str = paste0(str, "Prediction: ", prediction, '<br>')  
	str = paste0(str, "Probability of being real news: ", round(res$p, digits=2), '<br>')  
	if (res$df$sentiment<0) {sentiment='Negative'}
	else if (res$df$sentiment>0) {sentiment='Positive'}
	else {sentiment='Neutral'}
	str = paste0(str, "Sentiment: ", sentiment, '<br><br><br>') 	
	str = paste0(str, "# of fake news words: ", res$df$n_fakenewswords, '<br>')                                                                      
	str = paste0(str, "Proportion of fake news words: ", round(res$df$prop_fakenewswords, digits=2), '<br>')  
	str = paste0(str, "# of real news words: ", res$df$n_truenewswords, '<br>')                                                                      
	str = paste0(str, "Proportion of real news words: ", round(res$df$prop_truenewswords, digits=2), '<br>')  	
	str = paste0(str, "# of stop words: ", res$df$n_stopwords, '<br>')                                                                      
	str = paste0(str, "Proportion of stop words: ", round(res$df$prop_stopwords, digits=2), '<br>')                                               
	str = paste0(str, "# of negative words: ", res$df$n_negativewords, '<br>')                                                              
	str = paste0(str, "Proportion of negative words: ", round(res$df$prop_negativewords, digits=2), '<br>')                                       
	str = paste0(str, "# of positive words: ", res$df$n_positivewords, '<br>')                                                              
	str = paste0(str, "Proportion of positive words: ", round(res$df$prop_positivewords, digits=2), '<br>')   
	str
  })
}
shinyApp(ui = ui, server = server)

