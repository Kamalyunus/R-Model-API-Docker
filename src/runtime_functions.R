library(text2vec)
library(tokenizers)
library(textstem)
library(caret)
library(dplyr)

neg_words <- c("mustnt","couldnt","cannot","cant","shouldnt","shant","wouldnt","wont","didnt","dont","doesnt","hadnt","havent","hasnt","werent","wasnt","arent","isnt","aint","na","nothin")

stopwords_all <- unique(c(tm::stopwords('en'),tm::stopwords('SMART'),neg_words))

word_token<-function(x) {
  tokenize_words(x, lowercase = TRUE, stopwords = stopwords_all, strip_punct = TRUE,
                 strip_numeric = TRUE, simplify = FALSE)
}

prep_fun <- function(x) {
  gsub("[^[:alnum:]\\s]", replacement = " ", tolower(x))
}

thresh_p<-function(pred,truth,sequ,positive){
  pos_class<-ifelse(colnames(pred)[1]==positive,colnames(pred)[1],colnames(pred)[2])
  neg_class<-setdiff(colnames(preds_tfidf),pos_class)
  table<-data.frame()
  for (i in 1:length(sequ)){
    preds_class<-as.factor(ifelse(pred[[positive]]>=sequ[i],pos_class,neg_class))
    conf<-confusionMatrix(data = preds_class, reference = truth)
    temp<-cbind.data.frame(sequ[i],conf$overall[[1]],round(conf$byClass[[1]],2),round(conf$byClass[[2]],2))
    table<-rbind(temp,table)
  }
  colnames(table)<-c("threshold","accuracy",paste0(pos_class,"_as_",pos_class),paste0(neg_class,"_as_",neg_class))
  return(table[order(table$threshold),])
}