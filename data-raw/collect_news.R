## code to prepare `collect_news` dataset goes here
##-----------------------------------------------------------------------
library(tidyverse)
library(glue)
library(qdapRegex)
library(tidytext)
library(syuzhet)
library(lubridate)
library(newsanchor)
library(stopwords)
library(quanteda)
library(stm)

dutch_stop_words <- data.frame()
stop_words <- c(stopwords('nl'),stopwords('nl',source = 'nltk'),stopwords('nl',source = 'stopwords-iso'))
dutch_stop_words <- as.data.frame(stop_words) %>% rename(word = stop_words)

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

)


news_item_results_df <- data.frame()
news_item_metadata <- data.frame()

for (keyword in search_items) {
  
  df_tmp <- get_everything_all(keyword,api_key = api_key,language = 'nl',from = '2020-07-23' )
  tmp_result_df<- df_tmp$results_df
  tmp_metadata_df <-df_tmp$metadata
  tmp_result_df$searched_keyword <- keyword
  tmp_metadata_df$searched_keyword <- keyword
  news_item_results_df <- bind_rows(news_item_results_df,tmp_result_df)
  news_item_metadata <- bind_rows(news_item_metadata,tmp_metadata_df)

  Sys.sleep(10)
}
date <- Sys.Date()

news_item_results_df %>%
  write_csv(glue('../vaccinationinNL/tmp/news_item_results_df{date}','.csv'))


news_item_metadata %>%
  write_csv(glue('../vaccinationinNL/tmp/news_item_metadata{date}','.csv'))



#  Preprocessing
###---------------------------------------------------

news_files <- list.files('../vaccinationinNL/tmp') %>%
  .[str_detect(.,'news_item_results_df')]
news_files <- paste('../vaccinationinNL/tmp/',news_files,sep = '')
news_dfs <-purrr::map(news_files,read_csv)
all_news <- data.frame()
for (df in news_dfs) {
  all_news <- bind_rows(all_news,df)
  
  
}



all_news$searched_keyword <- NULL

all_news<-all_news  %>% 
  mutate(published_at = lubridate::ymd_hms(published_at),
         by_day = lubridate::floor_date(published_at,  "day")) %>% 
  distinct_all(.keep_all = TRUE) 



#Sentiment Analysis
#------------------------------------------------------
all_news_sentiment_description <- all_news %>% 
  bind_cols(get_nrc_sentiment(as.vector(
    all_news$description),
    language = 'dutch')) %>% 
  filter(!is.na(description)) %>% 
  filter(str_detect(str_to_lower(title),'vaccin') |str_detect(str_to_lower(description),'vaccin')|str_detect(str_to_lower(content),'vaccin'))

all_news_sentiment_content <- all_news %>% 
  mutate(content = str_remove_all(content,'Deze website maakt gebruik van cookies'),
         content = str_remove_all(content,'chars'),
         content = str_remove_all(content,'cookies'),
         content = qdapRegex::rm_number(content)) %>% 
  bind_cols(get_nrc_sentiment(as.vector(
    all_news$content),
    language = 'dutch')) %>% 
  filter(!is.na(content)) %>% 
  filter(str_detect(str_to_lower(title),'vaccin') |str_detect(str_to_lower(description),'vaccin')|str_detect(str_to_lower(content),'vaccin')) 

sentiment_Words_content <- all_news  %>% 
  unnest_tokens(input =  content,output = word,drop  = FALSE) %>% 
  inner_join(get_sentiment_dictionary('nrc',language ='dutch'))


sentiment_Words_description_stop_words  <- all_news_sentiment_description  %>% 
  unnest_tokens(input =  description ,output = word,drop  = FALSE) %>% 
  rowid_to_column() %>% 
  filter(word != 'chars') %>% 
  anti_join(dutch_stop_words) 

count_sentiment_Words_description_stop_words <- sentiment_Words_description_stop_words%>% 
  inner_join(get_sentiment_dictionary('nrc',language ='dutch')) %>% 
  count(sentiment)



all_news_sentiment_content_tidy_stop_word <- all_news_sentiment_content %>% 
  rowid_to_column() %>% 
  unnest_tokens(word,content) %>% 
  anti_join(dutch_stop_words) 


all_news_sentiment_description_tidy_stop_word <-  all_news_sentiment_description %>% 
  rowid_to_column() %>% 
  unnest_tokens(word,description) %>% 
  anti_join(dutch_stop_words) 
##STM
#---------------------------------------------------


d_tidy <- sentiment_Words_description_stop_words %>% 
  add_count(word) %>% 
  select(-n)
  
d_sparse <- all_news_sentiment_description_tidy_stop_word %>% 
  count(rowid,word,sort = TRUE) %>% 
  cast_sparse(rowid,word,n)

n_outlets <- n_distinct(d_tidy$name)
n_docs <- n_distinct(d_tidy$row_id)
n_words <- nrow(d_tidy)
n_authors <- n_distinct(d_tidy$author)

m_topic <- stm(d_sparse, K = 8, verbose = TRUE, max.em.its = 75)

d_beta <- tidy(m_topic)
  # select only one of the terms that map to the same lemmatised term
  d_top_terms <- d_beta %>% 
  group_by(topic, term) %>% 
  summarise(beta = max(beta)) %>% 
  
  # select the top words by topic
  group_by(topic) %>%
  top_n(100, beta) %>%
  mutate(top_word = beta == max(beta)) %>% 
  arrange(topic) %>%
  ungroup() %>% 
  rename(term = term)

d_gamma <- tidy(m_topic, matrix = "gamma", document_names = rownames(d_sparse))

# compute topic prevalence
d_gamma_terms <- 
  d_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(d_top_terms, by = "topic")

m_corr <- stm::topicCorr(m_topic, cutoff = 0.1)  # 0.075

# extract adjacency matrix from topic correlations and build a network via igraph
g_vis <- 
  igraph::simplify(igraph::graph.adjacency(m_corr$poscor, mode = "undirected", weighted = TRUE)) %>% 
  toVisNetworkData()



d_nodes <- 
  g_vis$nodes %>% 
  left_join(d_top_terms %>% filter(top_word) %>% select(-top_word), by = c(id = "topic")) %>% 
  left_join(
    d_gamma_terms %>% filter(top_word) %>% select(topic, gamma),
    by  = c(id = "topic")
  ) %>% 
  mutate(size = 2000 * gamma) %>%
  select(id, label = term, size) %>% 
  mutate(topic = id) %>% 
  left_join(
    d_top_terms %>% 
      group_by(topic) %>% 
      top_n(10, beta) %>% 
      ungroup() %>% 
      mutate(node_id = 1000 * (1:n())) %>% 
      select(topic, term, beta, node_id),
    by = "topic"
  ) %>% 
  mutate(
    id = ifelse(label == term, id, node_id),
    shape = ifelse(label == term, "dot", "square"),
    color = ifelse(label == term, "#3fb1e3", "#888888"),
    # font.color = ifelse(label == term, "#3fb1e3", "#888888"),
    size = 12,
    font.size =12,
    label = term
  )


# add additional properties to the graph edges
d_edges <- 
  g_vis$edges %>% 
  mutate(width = weight * 20) %>% 
  bind_rows(
    d_nodes %>% 
     # filter(id > 100) %>% 
      mutate(weight = 1, width = 1) %>% 
      select(from = topic, to = id, weight, width)
  )




#tf-idf
#---------------------------------------------------
# all_news_sentiment_content_tidy_stop_word %>% 
#   count(rowid,word,sort = TRUE) %>% 
#   bind_tf_idf(word,rowid,n) %>% 
#   arrange(-tf_idf) %>% 
#   group_by(rowid) %>% 
#   top_n(10) %>% 
#   ungroup() %>%
#   filter(rowid <10) %>% 
#   mutate(word = reorder_within(word, tf_idf, rowid)) %>%
#   ggplot(aes(word, tf_idf, fill = rowid)) +
#   geom_col(alpha = 0.8, show.legend = FALSE) +
#   facet_wrap(~ rowid, scales = "free", ncol = 3) +
#   scale_x_reordered() +
#   coord_flip() +
#   theme(strip.text=element_text(size=11)) 
# 
# 
# news_documents <- textProcessor(all_news_sentiment_content$content, metadata = all_news_sentiment_content, lowercase = TRUE,
#               removestopwords = TRUE, removenumbers = TRUE,
#               removepunctuation = TRUE, ucp = FALSE, stem = FALSE,
#               wordLengths = c(3, Inf), sparselevel = 1, language = "dutch",
#               verbose = TRUE, onlycharacter = FALSE, striphtml = FALSE,
#               customstopwords = dutch_stop_words$word, custompunctuation = NULL, v1 = FALSE)
# 
# 
# out <- prepDocuments(news_documents$documents, news_documents$vocab, news_documents$meta,lower.thresh = 0)
# 
# poliblogPrevFit <- stm(out$documents, out$vocab, K=8, prevalence=~positive+negative, 
#                        max.em.its=100, data=out$meta, init.type="Spectral", 
#                        seed=8458159)
# plot(poliblogPrevFit, type="summary", xlim=c(0,10))
# plot(poliblogPrevFit, type="labels", )
# 
# plot(poliblogPrevFit, type="perspectives", topics=c(1,5))
#d_sparse_news <-d_sparse 
#Save words 
#---------------------------------------------------------------------------------------------------
usethis::use_data(all_news, overwrite = TRUE)
usethis::use_data(all_news_sentiment_description, overwrite = TRUE)
usethis::use_data(all_news_sentiment_content, overwrite = TRUE)
usethis::use_data(sentiment_Words_content, overwrite = TRUE)
usethis::use_data(all_news_sentiment_content_tidy_stop_word, overwrite = TRUE)
usethis::use_data(sentiment_Words_description_stop_words, overwrite = TRUE)
usethis::use_data(count_sentiment_Words_description_stop_words, overwrite = TRUE)
usethis::use_data(d_nodes, overwrite = TRUE)
usethis::use_data(d_edges, overwrite = TRUE)
usethis::use_data(d_sparse_news, overwrite = TRUE)
usethis::use_data(d_top_terms, overwrite = TRUE)



