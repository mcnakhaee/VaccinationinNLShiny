#' tweet_hashtag_words UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import shinyWidgets
#' @import visNetwork
mod_tweet_hashtag_words_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(fluidRow(
      column( width = 4,
              
              
              div(
                
                bs4Card(
                  shiny::h3('Filters'),
                  sliderTextInput(
                    inputId =ns("filter_bot_wc"),
                    label = "probability of being  a bot:", 
                    choices = seq(0,1,by=0.01),
                    selected = c(0,1)
                  ),
                  sliderTextInput(
                    inputId =ns("filter_pos_sentiment_wc"),
                    label = "Positivity", 
                    choices = seq(0,10,by=1),
                    selected = c(0,10)
                  ),
                  sliderTextInput(
                    inputId =ns("filter_neg_sentiment_wc"),
                    label = "Negativity", 
                    choices = seq(0,10,by=1),
                    selected = c(0,10)
                  ),
                  sliderTextInput(
                    inputId =ns("filter_fear_sentiment_wc"),
                    label = "Fear", 
                    choices = seq(0,max(bot_filltered_tweets_sentiment$fear)+1,by=1),
                    selected = c(0,max(bot_filltered_tweets_sentiment$fear)+1)
                  ),
                  sliderTextInput(
                    inputId =ns("filter_anger_sentiment_wc"),
                    label = "Anger:", 
                    choices = seq(0,max(bot_filltered_tweets_sentiment$anger)+1,by=1),
                    selected = c(0,max(bot_filltered_tweets_sentiment$anger)+1)
                  ),
                  sliderTextInput(
                    inputId =ns("filter_trust_sentiment_wc"),
                    label = "Trust:", 
                    choices = seq(0,max(bot_filltered_tweets_sentiment$trust)+1,by=1),
                    selected = c(0,max(bot_filltered_tweets_sentiment$trust)+1)
                  ),
                  sliderTextInput(
                    inputId =ns("filter_joy_sentiment_wc"),
                    label = "Joy:", 
                    choices = seq(0,max(bot_filltered_tweets_sentiment$joy)+1,by=1),
                    selected = c(0,max(bot_filltered_tweets_sentiment$joy)+1)
                  ),
                  sliderTextInput(
                    inputId =ns("filter_anticipation_sentiment_wc"),
                    label = "Anticipation:", 
                    choices = seq(0,max(bot_filltered_tweets_sentiment$anticipation)+1,by=1),
                    selected = c(0,max(bot_filltered_tweets_sentiment$anticipation)+1)
                  ),
                  sliderTextInput(
                    inputId =ns("filter_disgust_sentiment_wc"),
                    label = "Disgust:", 
                    choices = seq(0,max(bot_filltered_tweets_sentiment$disgust)+1,by=1),
                    selected = c(0,max(bot_filltered_tweets_sentiment$disgust)+1)
                  ),
                  sliderTextInput(
                    inputId =ns("filter_sadness_sentiment_wc"),
                    label = "Sadness:", 
                    choices = seq(0,max(bot_filltered_tweets_sentiment$sadness)+1,by=1),
                    selected = c(0,max(bot_filltered_tweets_sentiment$sadness)+1)
                  ),
                  inputId = "hashtag_filter",
                  title = "Filters",
                  width = 12,
                  status = "info",
                  closable = FALSE,
                  maximizable = TRUE,
                  collapsible = TRUE
                  
                ), style='width: 300px; height: 1200px' ,class = "span16" 
                
              )
              

              #dateRangeInput()
        
        
      ),
      
      
      column(width = 8,
        
      fluidRow(
        div(
          
          bs4Card(
            echarts4rOutput(ns("hashtags_wordcloud_twitter"),width = "900px", height = "500px"),
            inputId = "hashtag_wc",
            title = "A Wordcloud of Hashtags",
            width = 12,
            status = "info",
            closable = FALSE,
            maximizable = TRUE,
            collapsible = TRUE
            
          ), style='width: 1000px; height: 550px' ,class = "span16" 
          
        ),
        div(
          
          bs4Card(
            visNetworkOutput(ns('hashtag_user_network'),width = "900px" , height = "700px"),
            inputId = "hashtag_user",
            title = "A Network of Hashtags-User",
            width = 12,
            status = "info",
            closable = FALSE,
            maximizable = TRUE,
            collapsible = TRUE
            
          ), style='width: 1000px; height: 700px' ,class = "span16" 
          
        ) ,
        
        div(
          
          bs4Card(
            visNetworkOutput(ns('hashtag_co_mention'),width = "900px",  height = "700px"),
            inputId = "card1",
            title = "A Network of Hashtags Co-occurrence",
            width = 12,
            status = "info",
            closable = FALSE,
            maximizable = TRUE,
            collapsible = TRUE
            
          ), style='width: 1000px; height: 700px' ,class = "span16" 
          
        )
        
        
      ),
      
      fluidRow(
        
        div(
          
          bs4Card(
            pickerInput(
              inputId =ns( "select_topics_tweet"),
              label = "Select a Topic", 
              choices = seq(1,16),
              selected = 1
            ),
            echarts4rOutput(ns('topic_wordcloud_tweet'),width = "900px",  height = "700px"),
            inputId = ns("topic_tweets"),
            title = "Wordcloud of Important Terms Extracted from Topic Modeling",
            width = 12,
            status = "info",
            closable = FALSE,
            maximizable = TRUE,
            collapsible = TRUE
            
          ), style='width: 1000px; height: 700px' ,class = "span16" 
          
        )
        
      )
      
    ),style='width: 1400px; height: 3000px' ,class = "span16" )
    )
  )
}
    
#' tweet_hashtag_words Server Function
#'
#' @noRd 
mod_tweet_hashtag_words_server <- function(input, output, session){
  ns <- session$ns
  twitter_hashtags_cuont_re <- reactive({
    
    bot_tweets_retweets_quotes_sentiment_merged %>% 
      #filter(!is_retweet) %>% 
      #distinct(text,.keep_all = TRUE) %>% 
      filter(!is.na(hashtags)) %>% 
      mutate(hashtags_tmp  = hashtags) %>% 
      separate_rows(hashtags, sep = ' ') %>% 
      filter(prob_bot>=input$filter_bot_wc[1],prob_bot<=input$filter_bot_wc[2]) %>%
      filter(positive>=input$filter_pos_sentiment_wc[1],positive<=input$filter_pos_sentiment_wc[2]) %>%
      filter(negative>=input$filter_neg_sentiment_wc[1],negative<=input$filter_neg_sentiment_wc[2]) %>% 
      filter(joy>=input$filter_joy_sentiment_wc[1],joy<=input$filter_joy_sentiment_wc[2]) %>% 
      filter(fear>=input$filter_fear_sentiment_wc[1],fear<=input$filter_fear_sentiment_wc[2]) %>% 
      filter(anger>=input$filter_anger_sentiment_wc[1],anger<=input$filter_anger_sentiment_wc[2]) %>%
      filter(trust>=input$filter_trust_sentiment_wc[1],trust<=input$filter_trust_sentiment_wc[2]) %>%
      filter(disgust>=input$filter_disgust_sentiment_wc[1],disgust<=input$filter_disgust_sentiment_wc[2]) %>%
      filter(sadness>=input$filter_sadness_sentiment_wc[1],sadness<=input$filter_sadness_sentiment_wc[2]) %>%
      filter(anticipation>=input$filter_anticipation_sentiment_wc[1],anticipation<=input$filter_anticipation_sentiment_wc[2])
      
  })

  
  output$hashtags_wordcloud_twitter <- renderEcharts4r({

      twitter_hashtags_cuont_re() %>%
      count(hashtags,name = 'freq',sort = TRUE) %>% 
        e_color_range(freq,color,c('#05668d','#028090','#00a896','#02c39a','#f0f3bd') ) %>% 
        e_charts() %>% 
        e_cloud(hashtags, freq,color, shape = "circle", sizeRange = c(10, 70)) %>% 
        e_title("Wordcloud of Twitter Hashtags", left = "center") %>% 
      e_tooltip() 
      
      
})
  
  output$hashtag_user_network <- renderVisNetwork({
    hashtags_users_df <- twitter_hashtags_cuont_re() %>%  #bot_filltered_tweets_sentiment %>% 
      group_by(screen_name,hashtags) %>% 
      summarize(
        counts = n(),
        n_followers = mean(followers_count),
        average_likes = mean(favorite_count),
        n_retweets = mean(retweet_count),
        total_likes = max(favourites_count),
        prob_bot = mean(prob_bot),
        positive = mean(positive),
        negative = mean(negative),
        anticipation = mean(anticipation),
        fear = mean(fear),
        anger = mean(anger)
      ) %>%
      ungroup() %>%
      arrange(desc(average_likes)) %>% 
      filter(average_likes>0.5)
    
    
    unique_hashtags <- hashtags_users_df %>%
      count(hashtags,sort = TRUE) %>%
      
      mutate(n = round(n*2) + 1)


    unique_users <- hashtags_users_df %>%
      distinct(screen_name,.keep_all = TRUE) %>%
      arrange(desc(screen_name)) %>%
      mutate(n_followers = (round(log10(n_followers + 1)+1)) ) %>%
      select(screen_name,n_followers) %>%
      arrange((n_followers))
    
    
    
     nodes <- data.frame(
       id = c(unique_users$screen_name,unique_hashtags$hashtags),
       label =c(unique_users$screen_name,unique_hashtags$hashtags),
    
       value = 3,
       size = c( unique_users$n_followers/2, (unique_hashtags$n) *2),
       group = c(   rep('users',length(unique_users$screen_name))  ,  rep('hashtag',length(unique_hashtags$hashtags))),
       shape = c(   rep('circle',length(unique_users$screen_name))  ,  rep('square',length(unique_hashtags$hashtags))),
       color.border = 'ffffff',
       color.highlight.background = "#02c39a", 
       color.highlight.border = "#02c39a",
       shadow = TRUE,
       stringsAsFactors = FALSE )

     
     edges <- data.frame(
       from = hashtags_users_df$screen_name,
       to = hashtags_users_df$hashtags,
       width = hashtags_users_df$counts,
       color = list(color ='#c6dabf',highlight  = '#1a936f'),
       stringsAsFactors = FALSE )
     visNetwork(nodes %>% 
                  distinct(id,.keep_all = TRUE) ,edges) %>% 
       visEdges(arrows = 'to',width = 'width') %>% 
       visGroups(groupname = "users", color = "#247ba0") %>%
       visGroups(groupname = "hashtag", color = "#f25f5c") %>%
       #visOptions(list(enabled =TRUE, degree = 1)) %>% 
       # allow clicking on the nodes, but disble the dropdown menu to select nodes from      visLegend()       %>% 
       
       # connect custom javascript functions to network events 
       visPhysics(
         solver = "forceAtlas2Based",
         timestep = 1,
         minVelocity = 1,
         maxVelocity = 30,
         forceAtlas2Based = list(gravitationalConstant = -800, damping = 1),
         stabilization = list(iterations = 600, updateInterval = 10),
         adaptiveTimestep = TRUE
       ) %>% 
       visIgraphLayout()
     
     # 
     # nodes <- nodes %>% 
     #   mutate(id = row_number()) %>% 
     #   select(id,everything())
     # 
     # edges <- hashtags_users_df %>%
     #   mutate(from = plyr::mapvalues(screen_name, from = nodes$label, to = nodes$id),
     #          to = plyr::mapvalues(hashtags, from =  nodes$label, to = nodes$id)) %>%
     #   select(from, to, everything()) 
     
     
     
  })
  
  
  
  
  output$hashtag_co_mention <- renderVisNetwork({ 

    hashtag_co_mention_edges <- twitter_hashtags_cuont_re() %>% 
      #filter(!is_retweet) %>% 
      #distinct(text,.keep_all = TRUE) %>% 
      
      #filter(!is_retweet) %>% 
      #distinct(text,.keep_all = TRUE) %>% 
      #filter(!is.na(hashtags)) %>% 
      #mutate(hashtags_tmp  = hashtags) %>% 
      #separate_rows(hashtags, sep = ' ') %>% 
      #filter(!is.na(hashtags)) %>% 
      #mutate(hashtags_tmp  = hashtags) %>% 
      #separate_rows(hashtags, sep = ' ') %>% 
      separate_rows(hashtags_tmp, sep = ' ') %>% 
      filter(hashtags!=hashtags_tmp) %>% 
      count(hashtags,hashtags_tmp) %>% 
      rename(from = hashtags, to = hashtags_tmp) %>% 
      filter(n >2)
    
    unique_hashtags <-  hashtag_co_mention_edges  %>% 
      count(from) 
    
    hashtag_co_mention_nodes <- unique_hashtags %>% 
      mutate(id = row_number(),
             label = from,
             color = 'red',
             size = n)
    
    # edges <- hashtag_co_mention_edges %>%
    #   mutate(from = plyr::mapvalues(from, from = hashtag_co_mention_nodes$label, to = hashtag_co_mention_nodes$id),
    #          to = plyr::mapvalues(to, from =  hashtag_co_mention_nodes$label, to = hashtag_co_mention_nodes$id)) %>%
    #   select(from, to, everything()) 
    # 
    
    nodes <- data.frame(
      id = unique_hashtags$from,
      label = unique_hashtags$from,
      value = 3,
      size = (unique_hashtags$n)*2,
      color.highlight.background = "firebrick", 
      color.highlight.border = "firebrick",
      font.color.highlight = "firebrick",
      color.background  = '#02c39a',
      color.border  = '#02c39a',
      
      stringsAsFactors = FALSE )
    
    
    edges <- data.frame(
      from = hashtag_co_mention_edges$from,
      to = hashtag_co_mention_edges$to,
      color = list(color ='#f0f3bd',highlight  = '#ff6b6b'),

     # width = log10(hashtag_co_mention_edges$n),
      stringsAsFactors = FALSE )
    visNetwork(nodes %>% 
                 distinct(id,.keep_all = TRUE) ,edges) %>% 
      visEdges() %>% 
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visIgraphLayout()

    
    
  })
  
  
  
  
  output$topic_wordcloud_tweet <- renderEcharts4r({
    
    d_top_terms_tweet %>% 
      filter(topic == input$select_topics_tweet) %>%
      e_color_range(beta,color ,c('#7400b8','#6930c3','#5e60ce','#5390d9','#4ea8de','#48bfe3','#56cfe1','#64dfdf','#72efdd','#80ffdb') ) %>% 
      e_charts(x = topic) %>% 
      e_title(text = paste("Word frequencies for Topic",input$select_topics_tweet), left = "center") %>%
      e_cloud(term, beta,color,  sizeRange = c(10, 72), width = "90%", height = "70%", rotationRange = c(0, 0)) %>% 
      e_color() %>% 
    e_theme("westeros")
  })
  
  
}
    
## To be copied in the UI
# mod_tweet_hashtag_words_ui("tweet_hashtag_words_ui_1")
    
## To be copied in the server
# callModule(mod_tweet_hashtag_words_server, "tweet_hashtag_words_ui_1")
 
