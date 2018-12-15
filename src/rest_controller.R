source("functions.R")
load("model_asset.RData")
p_thresh<-0.4

#* Log system time, request method and HTTP user agent of the incoming request
#* @filter logger
function(req){
  cat("System time:", as.character(Sys.time()), "\n",
      "Request method:", req$REQUEST_METHOD, req$PATH_INFO, "\n",
      "HTTP user agent:", req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
  plumber::forward()
}

#* @param text Enter text string
#* @post /predict
predict_comment<- function(text,res) {
  
  if(!is.character(text)){
    msg <- "please enter text"
    res$status <- 400 
    list(error=jsonlite::unbox(msg))
  } else {
    
    it_test <- text %>% 
      lemmatize_strings() %>% 
      prep_fun %>% 
      word_token %>% 
      itoken(progressbar=FALSE) %>% 
      create_dtm(vectorizer) %>% 
      transform(tfidf)
    
    prediction<-predict(gbmFit1, newdata = as.matrix(it_test),type="prob")
    res<-ifelse(prediction[["Downstream"]]>=p_thresh,"Downstream","Upstream")
    return(res)
  }
}
