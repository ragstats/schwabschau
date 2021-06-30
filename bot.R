
library(twitteR)
library(dplyr)
library(lubridate)
library(stringr)
library(purrr)
library(schwabr)

consumer_key <- Sys.getenv("consumer_key")

consumer_secret <- Sys.getenv("consumer_secret")

access_token <- Sys.getenv("token")

access_secret <- Sys.getenv("secret")

print("authenticate")

setup_twitter_oauth(consumer_key,consumer_secret,
                    access_token,access_secret)

print("get tweets")

ts <- twitteR::searchTwitter("from:tagesschau", n = 100) %>% 
  twListToDF()


print("schwabify")

ts_schwabs <- ts %>% 
  filter(created > lubridate::now() - lubridate::dhours(1)) %>% 
  rowwise() %>% 
  mutate(schwabtext = get_schwab(text)) %>% 
  ungroup() %>% 
  mutate(link = stringr::str_extract(text, "http[^[:space:]]*"),
         schwabtext = stringr::str_replace(schwabtext, "hddb[^[:space:]]*", link))

ts_rows <- nrow(ts_schwabs) 

if(ts_rows==0){
  
    print("nothing to tweet")
  
} else {
  print(paste0("tweet out", ts_rows, " tweets."))
  
 ts_schwabs %>% 
  split(1:nrow(.)) %>% 
  purrr::walk(~{
    
    print(.x$schwabtext)
    
    Sys.sleep(5)
    
    twitteR::tweet(text = .x$schwabtext, inReplyTo = .x$id, bypassCharLimit = T)
    
  })
}



