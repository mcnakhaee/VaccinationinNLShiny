## code to prepare `hashtags_social_network` dataset goes here
library(tidyverse)
library(twinetverse)
library(networkD3)
library(visNetwork)
library(echarts4r)

mention_net <- all_tweets %>%
  gt_edges(screen_name, mentions_screen_name) %>%
  gt_nodes() %>%
  # get nodes
  gt_collect() # collect

mention_nodes <- mention_net$nodes   
mention_edges <- mention_net$edges  

mention_nodes <- nodes2sg(mention_nodes)
mention_edges <- edges2sg(mention_edges)


retweet_net <- all_tweets %>%
  gt_edges(screen_name, mentions_screen_name) %>%
  gt_nodes() %>%
  # get nodes
  gt_collect() # collect

retweet_nodes <- retweet_net$nodes   
retweet_edges <- retweet_net$edges  


retweet_nodes <- nodes2sg(retweet_nodes)
retweet_edges <- edges2sg(retweet_edges)
retweet_edges <- retweet_edges %>% 
  filter(size >5)

sigmajs() %>%
  sg_nodes(mention_nodes, id, size) %>%
  sg_edges(mention_edges, id, source, target) %>% 
  sg_layout(layout = igraph::layout_components) %>% 

  sg_settings(
    minNodeSize = 1,
    maxNodeSize = 2.5,
    edgeColor = "default",
    defaultEdgeColor = "#d3d3d3"
  )


retweet_nodes <- retweet_nodes %>% 
  mutate(group = 1,
         symbol = 'circle',
         value = 7)




retweet_edges$source <- match(retweet_edges$source ,retweet_nodes$label) -1
retweet_edges$target <- match(retweet_edges$target,retweet_nodes$label)-1
forceNetwork(Links = retweet_edges, Nodes = retweet_nodes,
             Source = "source", Target = "target",NodeID = 'label',
             Value = "n",Group  ='group',fontSize = 10) 




nodes <- data.frame(
  name = hashtags,
  
  value = 3,
  size = 7,
  grp = 1,
  symbol = 'circle',
  stringsAsFactors = FALSE
)





retweet_edges
e_charts() %>% 
  e_graph()  %>% 
  e_graph_nodes(retweet_nodes, label, value, size,grp) %>% 
  e_graph_edges(retweet_edges, source, target) %>% 
  #e_modularity() %>% 
  e_tooltip()






retweet_edges%>% 
  count(size)


hashtag_cooccurance <- retweet_edges%>% 
  filter(size > 4) %>% 
  rename(Source = source,Target = target ) %>% 
  arrange(desc(Source))%>% 
  as.data.frame()


hashtags_df <- retweet_nodes %>% 
  #select(Source) %>%
  mutate(group = '1') %>% 
  #distinct(Source,.keep_all = TRUE) %>% 
  #arrange(desc(Source)) %>% 
  #tibble::rownames_to_column() %>% 
  as.data.frame() 
  #rename(hashtags = Source)

hashtag_cooccurance$Source <- match(hashtag_cooccurance$Source,hashtags_df$label) -1
hashtag_cooccurance$Target <- match(hashtag_cooccurance$Target,hashtags_df$label)-1
forceNetwork(Links = hashtag_cooccurance, Nodes = hashtags_df,
             Source = "Source", Target = "Target",NodeID = 'label',
             Value = "size",Group  ='group',fontSize = 10) 
  
usethis::use_data(hashtags_social_network, overwrite = TRUE)
