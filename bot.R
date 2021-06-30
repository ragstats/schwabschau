
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

setup_twitter_oauth(consumer_key,consumer_secret,
                    access_token,access_secret)

ts <- twitteR::searchTwitter("from:tagesschau", n = 100) %>% 
  twListToDF()

ts_schwabs <- ts %>% 
  filter(created > lubridate::now() - lubridate::dhours(0.9)) %>% 
  rowwise() %>% 
  mutate(schwabtext = get_schwab(text)) %>% 
  ungroup() %>% 
  mutate(link = stringr::str_extract(text, "http[^[:space:]]*"),
         schwabtext = stringr::str_replace(schwabtext, "hddb[^[:space:]]*", link))

ts_schwabs %>% 
  split(1:nrow(.)) %>% 
  purrr::walk(~{
    
    Sys.sleep(5)
    
    twitteR::tweet(text = .x$schwabtext, inReplyTo = .x$id, bypassCharLimit = T)
    
  })
