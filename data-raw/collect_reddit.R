## code to prepare `collect_reddit` dataset goes here
library(tidyverse)
library(RedditExtractoR)
library(glue)
library(lubridate)
library(qdapRegex)
library(tidytext)
library(syuzhet)
library(stopwords)
library(quanteda)
library(stm)
dutch_stop_words <- data.frame()
stop_words <- c(stopwords('nl'),stopwords('nl',source = 'nltk'),stopwords('nl',source = 'stopwords-iso'))
dutch_stop_words <- as.data.frame(stop_words) %>% rename(word = stop_words)

data <- Sys.Date()
keywords <- c('vaccineren',
              'Vaccinatie',
              'antivax',
              'coronavaccin',
              'viruswaanzin',
              'ikvaccineer',
              'ikvaccineerniet',
              'Gezondheidsraad'
)



reddit_df <- as.data.frame()

for (keyword in keywords) {
  tmp_reddit <- get_reddit(
    search_terms = keyword,
    #page_threshold = 5,
    #cn_threshold = 10
  )
  print(keyword)
  reddit_df <- bind_rows(reddit_df,tmp_reddit)
  Sys.sleep(150)
}

##Preprocessing
## -------------------------------------------------

reddit_df <-  read_csv(glue('../vaccinationinNL/tmp/reddit_data{date}','.csv'))

reddit_df <- reddit_df %>% 
  mutate(post_date = dmy(post_date),
         comm_date = dmy(comm_date),
  post_text_cleaned = str_replace_all(post_text,"@\\w+", ""),
  post_text_cleaned = str_replace_all(post_text_cleaned,"RT @[a-z,A-Z]*: ", ""),
  post_text_cleaned = str_replace_all(post_text_cleaned,"http\\w+", ""),
  post_text_cleaned = str_replace_all(post_text_cleaned,"#[a-z,A-Z]*", ""),
  post_text_cleaned = rm_url(post_text_cleaned),
  comment_cleaned =  str_replace_all(comment,"@\\w+", ""),
  comment_cleaned = str_replace_all(comment_cleaned,"RT @[a-z,A-Z]*: ", ""),
  comment_cleaned = str_replace_all(comment_cleaned,"http\\w+", ""),
  comment_cleaned = str_replace_all(comment_cleaned,"#[a-z,A-Z]*", ""),
  comment_cleaned = rm_url(comment_cleaned),
  by_day = lubridate::floor_date(comm_date,  "day")) %>% 
  select(-link,-URL,-domain) %>% 
  filter(comm_date>'2019-05-24') %>% 
  #bind_rows(getcontent) %>% 
  distinct_all() 

count_reddit_df_sentiment_comment <- reddit_df_sentiment_comment %>% 
  unnest_tokens(word,comment) %>% 
  inner_join(get_sentiment_dictionary('nrc',language ='dutch')) %>% 
  count(sentiment)
  

# reddit_df%>% 
#   write_csv(glue('../vaccinationinNL/tmp/reddit_data{date}','.csv'))

## Sentiment Analysis
#----------------------------------------------------------------------------


reddit_df_sentiment_comment <- reddit_df %>% 
  bind_cols(get_nrc_sentiment(as.vector(
  reddit_df$comment_cleaned),
  language = 'dutch'))





#Topic Modeling
#--------------------------------

# d_sparse <- reddit_df_sentiment_comment %>% 
#   mutate(comment_cleaned = str_replace_all(comment,"@\\w+", ""),
#          comment_cleaned = str_replace_all(comment_cleaned,"RT @[a-z,A-Z]*: ", ""),
#          comment_cleaned = str_replace_all(comment_cleaned,"http\\w+", ""),
#          comment_cleaned = str_replace_all(comment_cleaned,"#[a-z,A-Z]*", ""),
#          comment_cleaned = rm_url(comment_cleaned)) %>% 
#       mutate(n_words = map_dbl(comment_cleaned, str_count)) %>%
#         filter(n_words > 100) %>% 
#   rowid_to_column() %>% 
#   unnest_tokens(input =  comment_cleaned ,output = word,drop  = FALSE) %>% 
#   
#   anti_join(dutch_stop_words) %>% 
#   anti_join(   tibble(word = stopwords()) ) %>% 
#   add_count(word) %>%
#   filter(n > 30) %>%
#   select(-n) %>%
#   count(rowid,word,sort = TRUE) %>% 
#   #filter(n > 30) %>% 
#   cast_sparse(rowid,word,n) 
# 
# 
# m_topic <- stm(d_sparse, K = 8, verbose = TRUE, max.em.its = 75)

 #reddit_df_sentiment_comment <-  read_csv(glue('../vaccinationinNL/tmp/reddit_df_sentiment_comment{date}','.csv'))
#reddit_df_sentiment_comment <-  read_csv('../vaccinationinNL/tmp/reddit_df_sentiment_comment2020-07-25.csv')
# reddit_df_sentiment_comment %>% 
#   write_csv(glue('../vaccinationinNL/tmp/reddit_df_sentiment_comment{date}','.csv'))


usethis::use_data(reddit_df_sentiment_comment, overwrite = TRUE)
usethis::use_data(count_reddit_df_sentiment_comment, overwrite = TRUE)
