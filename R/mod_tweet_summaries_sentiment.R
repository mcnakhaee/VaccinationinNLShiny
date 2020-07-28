#' tweet_summaries_sentiment UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import echarts4r
#' @import plotly
mod_tweet_summaries_sentiment_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidRow(
      
      div(bs4TabSetPanel(
        
        id = "twitter_tabcard",
        side = "left",
        bs4TabPanel(
          tabName = "Tweets over Time",
          active = TRUE,
          echarts4rOutput(ns('number_of_tweets_over_time'),width = "1200px", height = "500px")
        ),
        bs4TabPanel(
          tabName = "Sentiment of Tweets",
          active = FALSE,
          
          echarts4rOutput(ns('sentiment_tweet_over_time'),width = "1200px", height = "550px")
        ),
        bs4TabPanel(
          tabName = "Sentiment of Retweets",
          active = FALSE,
          
          echarts4rOutput(ns('sentiment_retweet_over_time'),width = "1200px", height = "550px")
        ),
        bs4TabPanel(
          tabName = "Sentiment of Quotes",
          active = FALSE,
          
          echarts4rOutput(ns('sentiment_qoutes_over_time'),width = "1200px", height = "550px")
        ),
        bs4TabPanel(
          tabName = "Overall Sentiment Trend",
          active = FALSE,
          
          echarts4rOutput(ns('overall_sentiment_over_time'),width = "1200px", height = "550px")
        ),
        
        bs4TabPanel(
          tabName = "Overall Sentiment Scores",
          active = FALSE,
          
          plotlyOutput(ns('sentiment_Scores'))
        ),
        
        bs4TabPanel(
          tabName = "Dataset",
          active = FALSE,
          
          reactableOutput(ns('twitter_table'),width = "1200px", height = "600px")
        )
        
        
        
      ),style='width: 1600px; height: 600px' ,class = "span16")
      
    )
 
  )
}
    
#' tweet_summaries_sentiment Server Function
#'
#' @noRd 
mod_tweet_summaries_sentiment_server <- function(input, output, session){
  ns <- session$ns
 
  source('R/utils_vis_functions.R')
  output$number_of_tweets_over_time <- renderEcharts4r({
    filtered_tweets %>% 
      count(by_day) %>% 
      e_charts(by_day) %>%
      e_line(n,
             smooth = TRUE) %>%
      #e_legend(show = TRUE, right = 0) %>%
      e_tooltip(trigger = "axis", formatter = e_tooltip_pointer_formatter("decimal", 0)) %>%
      e_title(subtext = "Number of Vaccination Related Dutch Tweets  Over Time", left = "center") %>%
      e_x_axis(splitLine = list(show = FALSE)) %>%
      e_y_axis(splitLine = list(show = FALSE), show = FALSE) %>%
      e_theme("essos" )
    
    
    
    
  })
  
  
  output$sentiment_tweet_over_time <- renderEcharts4r({
    
    
    distinct_filtered_tweets_sentiment_text %>%
      group_by(by_day) %>% 
      vis_river_plot("Twitter's Sentiment Regarding Vaccination Over Time")
    
    
  })
  



output$sentiment_retweet_over_time <- renderEcharts4r({
  
  
  retweets_sentiment_small %>%
    group_by(by_day) %>% 
    vis_river_plot("Sentiment of Dutch Retweets Regarding Vaccination Over Time")
  
  
  
})


output$sentiment_qoutes_over_time <- renderEcharts4r({
  quotes_sentiment_small %>%
    group_by(by_day) %>% 
    vis_river_plot("' Sentiment of Dutch Quotes Regarding Vaccination Over Time")
  
  
  
})


output$overall_sentiment_over_time <- renderEcharts4r({
  
  bot_tweets_retweets_quotes_sentiment_merged %>%
    group_by(by_day) %>% 
    vis_river_plot("Overall Sentiment Trend", 'Overall sentiment of tweets, retweets and quotes on Twitter')
  
  
})




output$sentiment_Scores <- renderPlotly({
  g <- distinct_filtered_tweets_sentiment_text_count  %>% 
    ggplot(aes(x = fct_reorder(sentiment,n,.desc = TRUE),y=n ,fill = sentiment)) +
    geom_col() +
    scale_fill_manual(values = c( 'positive' ='#118ab2' ,
                                   'negative'  = '#ef476f',
                                 'anger' ='#d62828',
                                   'fear' = '#f77f00',
                                   'sadness' = '#003049',
                                  'disgust' = '#b5838d' ,
                                  'surprise' = '#ffd166',
                                   'anticipation' = '#028090',
                                  'trust' = '#06d6a0' ,
                                   'joy' = '#BDB2FF'))+
    
    theme_classic() +
    labs(x = '', y = '',title = ' Overall Sentiment Scores of Vaccination Related Tweets ') +
    theme(legend.position = 'none')
  
    ggplotly(g,tooltip = c('n')) %>%
  highlight("plotly_selected")
})




output$twitter_table <- renderReactable({
  reactable(
    distinct_filtered_tweets_sentiment_text %>%
      select(screen_name,text,created_at,retweet_count,favorite_count, positive,negative),
    filterable = TRUE,
    searchable = TRUE,
    sortable = FALSE,
    resizable = TRUE,
    showPageSizeOptions = TRUE,
    onClick = "expand",
    borderless = TRUE,
    highlight = TRUE,
    compact = TRUE,
    
    
  )
})


}
    
## To be copied in the UI
# mod_tweet_summaries_sentiment_ui("tweet_summaries_sentiment_ui_1")
    
## To be copied in the server
# callModule(mod_tweet_summaries_sentiment_server, "tweet_summaries_sentiment_ui_1")
 
