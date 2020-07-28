## code to prepare `collect_news` dataset goes here
##-----------------------------------------------------------------------
library(tidyverse)
library(rtweet)
library(glue)
library(qdapRegex)
library(tidytext)
library(syuzhet)
library(lubridate)
date <- Sys.Date()

#Keywords Collect Tweets
##-----------------------------------------------------------------------
tweet_keywords <- c('vaccineren',
                    'vaccineren autism',
                    'vaccineren mmr',
                    'vaccineren measles',
                    'vaccineren corona',
                    'vaccineren coronavirus',
                    'vaccineren covid',
                    'vaccinatie autism',
                    'vaccinatie corona',
                    'Vaccinatie covid',
                    'Vaccinatie autism',
                    'vaxxhappened',
                    'CoronaVaccin',
                    'plandemic',
                    'autism vaccine',
                    'vrijheid vaccine',
                    'mmr vaccine',
                    'measles autism',
                    'Vaccinatie',
                    'antivax',
                    'vaccination autism',
                    'vaccination corona',
                    'antivax corona',
                    'antivax covid',
                    'coronavaccin',
                    'viruswaanzin',
                    'ikvaccineer',
                    'ikvaccineer',
                    'ikvaccineerniet',
                    'Gezondheidsraad',
                    'MyBodyMyChoice'
)

tweets_in_dutch <- data.frame()
tweets_in_news <- data.frame()
tweets_in_verified <- data.frame()
for (keyword in tweet_keywords) {
  Sys.sleep(20)
  tweet_tmp <- rtweet::search_tweets(keyword,n = 3000,retryonratelimit = TRUE,lang = "nl")
  tweet_tmp$searched_keyword <- keyword
  tweets_in_dutch <- bind_rows(tweets_in_dutch,tweet_tmp)
  
  
}

tweets_in_dutch %>%
  save_as_csv(glue('../vaccinationinNL/tmp/tweets_in_dutch{date}','.csv'), prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")

# preprocess tweets
##-------------------------------------------------------------------------------------------------------------------------------------



tweet_files <- list.files('../vaccinationinNL/tmp') %>%
  .[str_detect(.,'tweets_in_dutch')]
tweet_files <- paste('../vaccinationinNL/tmp/',tweet_files,sep = '')
tweet_dfs <-purrr::map(tweet_files,rtweet::read_twitter_csv)




tweets_all <- data.frame()
for (df in tweet_dfs) {
  tweets_all <- bind_rows(tweets_all,df)
}


## process anc clean text

filtered_tweets <- tweets_all %>%
  distinct_all %>%
  filter(str_detect(text,'vaccin') | str_detect(text,'Vaccin')| str_detect(text,'vax') | str_detect(text,'Vax')) %>%
  mutate(text_cleaned = str_replace_all(text,"@\\w+", ""),
         text_cleaned = str_replace_all(text_cleaned,"RT @[a-z,A-Z]*: ", ""),
         text_cleaned = str_replace_all(text_cleaned,"http\\w+", ""),
         text_cleaned = str_replace_all(text_cleaned,"#[a-z,A-Z]*", ""),
         text_cleaned = rm_url(text_cleaned),
         retweet_text_cleaned = str_replace_all(retweet_text,"@\\w+", ""),
         retweet_text_cleaned = str_replace_all(retweet_text_cleaned,"RT @[a-z,A-Z]*: ", ""),
         retweet_text_cleaned = str_replace_all(retweet_text_cleaned,"http\\w+", ""),
         retweet_text_cleaned = str_replace_all(retweet_text_cleaned,"#[a-z,A-Z]*", ""),
         retweet_text_cleaned = rm_url(retweet_text_cleaned),
         quoted_text_cleaned = str_replace_all(quoted_text,"@\\w+", ""),
         quoted_text_cleaned = str_replace_all(quoted_text_cleaned,"RT @[a-z,A-Z]*: ", ""),
         quoted_text_cleaned = str_replace_all(quoted_text_cleaned,"http\\w+", ""),
         quoted_text_cleaned = str_replace_all(quoted_text_cleaned,"#[a-z,A-Z]*", ""),
         quoted_text_cleaned = rm_url(quoted_text_cleaned),
         created_at = lubridate::ymd_hms(created_at),
         quoted_created_at = lubridate::ymd_hms(quoted_created_at),
         retweet_created_at = lubridate::ymd_hms(quoted_created_at),
         by_day = lubridate::floor_date(created_at,  "day"),
         by_hour= lubridate::floor_date(created_at,  "hour"),
         by_day_quoted=  lubridate::floor_date(quoted_created_at,  "day"),
         by_hour_quoted= lubridate::floor_date(quoted_created_at,  "hour"),
         by_day_retweet = lubridate::floor_date(retweet_created_at,  "day"),
         by_hour_retweet= lubridate::floor_date(retweet_created_at,  "hour")
  )


filtered_tweets%>%
  select(screen_name,
         text,
         source,
         mentions_screen_name,
         retweet_screen_name,
         quoted_screen_name,
         created_at,
         by_day,
         by_hour,
         by_day_retweet,
         by_hour_retweet,
         by_day_quoted,
         by_hour_quoted,
         hashtags,
         is_retweet,
         is_quote,
         followers_count,
         favorite_count,
         retweet_count,
         quote_count,
         reply_count,
         symbols,
         location,
         urls_url,
         favourites_count,
         friends_count,
         retweet_text,
         quoted_text,
         verified,
         text_cleaned,
         retweet_text_cleaned,
         quoted_text_cleaned) %>% 
  write_csv(glue('../vaccinationinNL/tmp/tweets_preprocessed/filtered_tweets_v_{date}','.csv') )


#Sentiment Analysis
#---------------------------------------------------------------------------------

filtered_tweets <- read_csv(glue('../vaccinationinNL/tmp/tweets_preprocessed/filtered_tweets_v_{date}','.csv') )
###-------------------------------------------
filtered_tweets_sentiment_tweet_text <- filtered_tweets %>%
  bind_cols(get_nrc_sentiment(as.vector(
    filtered_tweets$text_cleaned),
    language = 'dutch')) 

filtered_tweets_sentiment_tweet_text %>% 
  write_csv(glue('../vaccinationinNL/tmp/tweets_preprocessed/filtered_tweets_sentiment_tweet_text_v_{date}','.csv') )
### ------------------------------------

distinct_filtered_tweets <- filtered_tweets %>%
  distinct(text,.keep_all = TRUE)

distinct_filtered_tweets_sentiment_text <- distinct_filtered_tweets %>% 
  bind_cols(get_nrc_sentiment(as.vector(
    distinct_filtered_tweets$text_cleaned),
    language = 'dutch')) 


# distinct_tweets_sentiment_text%>% 
#   write_csv(glue('../vaccinationinNL/tmp/tweets_preprocessed/distinct_tweets_sentiment_text_v_{date}','.csv') )
#------------------------




### Sentiment of retweets 
#### --------------------
retweets <- filtered_tweets %>%
  filter(is_retweet) %>% 
  distinct(retweet_text,.keep_all = TRUE  )

retweets_sentiment <- retweets %>% 
  bind_cols(get_nrc_sentiment(as.vector(
    retweets$retweet_text_cleaned),
    language = 'dutch')) 

retweets_sentiment_small <- retweets_sentiment %>% 
  select(screen_name,by_day,by_day_retweet,retweet_text,retweet_screen_name, anger:positive)
 # retweets_sentiment%>% 
 #  write_csv(glue('../vaccinationinNL/tmp/tweets_preprocessed/retweets_sentiment_v_{date}','.csv') )

### Sentiment of Quotes 
#### ----------------------------------------------------
quotes <- filtered_tweets %>%
  filter(is_quote) %>% 
  distinct(quoted_text,.keep_all = TRUE)


quotes_sentiment <- quotes %>% 
  bind_cols(get_nrc_sentiment(as.vector(
    quotes$quoted_text_cleaned),
    language = 'dutch')) 

quotes_sentiment_small <- quotes_sentiment %>% 
  select(screen_name,by_day,by_day_quoted,quoted_text,quoted_screen_name,anger:positive)

# quotes_sentiment %>% 
#    write_csv(glue('../vaccinationinNL/tmp/tweets_preprocessed/quotes_sentiment_v_{date}','.csv') )


# compute statistics
#------------------------------------------------------------------------------------------------

distinct_filtered_tweets_sentiment_text_count <-  distinct_filtered_tweets_sentiment_text %>% 
  unnest_tokens(input =  text_cleaned,output = word) %>% 
  inner_join(get_sentiment_dictionary('nrc',language ='dutch')) %>% 
  count(sentiment)

distinct_filtered_tweets_sentiment_words <- distinct_filtered_tweets_sentiment_text %>% 
  unnest_tokens(input =  text_cleaned,output = word) %>% 
  inner_join(get_sentiment_dictionary('nrc',language ='dutch'))



### Bots
#---------------------------------------------------------------------------------------------------
# distinct_tweets_sentiment_words %>% 
#   write_csv(glue('../vaccinationinNL/tmp/tweets_preprocessed/distinct_tweets_sentiment_words_v_{date}','.csv') )
filtered_tweets_sentiment_tweet_text <- read_csv(glue('../vaccinationinNL/tmp/tweets_preprocessed/filtered_tweets_sentiment_tweet_text_v_{date}','.csv') )

bots_info <- read_csv('../vaccinationinNL/tmp/bot/bots_df_info_news2020-07-25.csv')
bots_info <- bots_info %>% 
  rename(screen_name_bot = screen_name)

bot_distinct_filtered_tweets_sentiment_text <- distinct_filtered_tweets_sentiment_text %>% 
  left_join(bots_info,by =c( 'screen_name'='screen_name_bot'))

bot_retweets_sentiment <- retweets_sentiment %>% 
  left_join(bots_info,by =c( 'retweet_screen_name'='screen_name_bot'))

bot_quotes_sentiment <- quotes_sentiment %>% 
  left_join(bots_info,by =c( 'quoted_screen_name'='screen_name_bot'))

bot_filltered_tweets_sentiment <-filtered_tweets_sentiment_tweet_text %>% 
  left_join(bots_info,by =c( 'screen_name'='screen_name_bot'))



#merge sentiments_bots
#-------------------------------------------------------
bot_tweets_retweets_quotes_sentiment_merged <- bind_rows(bot_distinct_filtered_tweets_sentiment_text,bot_retweets_sentiment,bot_quotes_sentiment) 
# sentiment_merged%>% 
#   write_csv(glue('../vaccinationinNL/tmp/tweets_preprocessed/sentiment_merged_v_{date}','.csv') )





## Hashtags
##------------------------------------------------------------------

twitter_hashtags_bot_cuont <- bot_distinct_filtered_tweets_sentiment_text %>% 
  select(hashtags,prob_bot) %>% 
  filter(!is.na(hashtags)) %>% 
  separate_rows(hashtags, sep = ' ') 

### topic modeling
##-----------------------------------------------------------
d_sparse <- distinct_filtered_tweets_sentiment_text %>% 
  mutate(n_words = map_dbl(text_cleaned, str_count),
         text_cleaned = rm_twitter_url(text_cleaned),
        text_cleaned = rm_white_punctuation(text_cleaned),
         text_cleaned = str_remove_all(text_cleaned,'t.co')) %>%
  filter(n_words > 10) %>% 
  rowid_to_column() %>% 
  unnest_tokens(input =  text_cleaned ,output = word,drop  = FALSE) %>% 
  
  anti_join(dutch_stop_words) %>% 
  anti_join(tibble(word = stopwords()) ) %>% 
 # add_count(word) %>%
  #select(word,n) %>% 
  #arrange(n)
  #filter(n > 10) %>%
  #select(-n) %>%
  count(rowid,word,sort = TRUE) %>% 
  #filter(n > 30) %>% 
  cast_sparse(rowid,word,n) 


m_topic_tweets <- stm(d_sparse, K = 16, verbose = TRUE, max.em.its = 75)



d_beta_tweet <- tidy(m_topic_tweets)
# select only one of the terms that map to the same lemmatised term
d_top_terms_tweet <- d_beta_tweet %>% 
  group_by(topic, term) %>% 
  summarise(beta = max(beta)) %>% 
  
  # select the top words by topic
  group_by(topic) %>%
  top_n(100, beta) %>%
  mutate(top_word = beta == max(beta)) %>% 
  arrange(topic) %>%
  ungroup() %>% 
  rename(term = term)


usethis::use_data(distinct_filtered_tweets_sentiment_text, overwrite = TRUE)
usethis::use_data(retweets_sentiment_small, overwrite = TRUE)
usethis::use_data(quotes_sentiment_small, overwrite = TRUE)
usethis::use_data(distinct_filtered_tweets_sentiment_text_count, overwrite = TRUE)
usethis::use_data(distinct_filtered_tweets_sentiment_words, overwrite = TRUE)
usethis::use_data(filtered_tweets, overwrite = TRUE)
usethis::use_data(bot_tweets_retweets_quotes_sentiment_merged, overwrite = TRUE)
#usethis::use_data(twitter_hashtags_cuont, overwrite = TRUE)
#usethis::use_data(bot_filltered_tweets, overwrite = TRUE)
usethis::use_data(bot_filltered_tweets_sentiment, overwrite = TRUE)
usethis::use_data(d_top_terms_tweet, overwrite = TRUE)
#usethis::use_data(twitter_hashtags_bot_cuont, overwrite = TRUE)

