#' reddit_summaries_sentiment UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_reddit_summaries_sentiment_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidRow(
      
      div(bs4TabSetPanel(
        
        id = "tabcard_reddit",
        side = "left",
        bs4TabPanel(
          tabName = "Reddit Comments Over Time",
          active = TRUE,
          
          echarts4rOutput(ns('number_comments_over_time'),width = "1200px", height = "550px")
        ),
        bs4TabPanel(
          tabName = "Sentiment of Comments",
          active = FALSE,
          
          echarts4rOutput(ns('sentiment_comments'),width = "1200px", height = "550px")
        ),
        bs4TabPanel(
          tabName = "Overall Sentiment Scores",
          active = FALSE,
          
          plotlyOutput(ns('reddit_sentiment_Scores'))
        ),
        bs4TabPanel(
          tabName = "Dataset",
          active = FALSE,
          
          reactableOutput(ns('reddit_table'),width = "1200px", height = "500px")
        )
        
      ),style='width: 1600px; height: 600px' ,class = "span16")
      
    )
  )
}
    
#' reddit_summaries_sentiment Server Function
#'
#' @noRd 
mod_reddit_summaries_sentiment_server <- function(input, output, session){
  ns <- session$ns
  
  source('R/utils_vis_functions.R')
  output$number_comments_over_time <- renderEcharts4r({
    reddit_df_sentiment_comment %>% 
      count(comm_date) %>% 
      e_charts(comm_date) %>%
      e_line(n,
             smooth = TRUE) %>%
      #e_legend(show = TRUE, right = 0) %>%
      e_tooltip(trigger = "axis", formatter = e_tooltip_pointer_formatter("decimal", 0)) %>%
      e_title(subtext = "Number of Vaccination Related Dutch Comments on Reddit Over Time", left = "center") %>%
      e_x_axis(splitLine = list(show = FALSE)) %>%
      e_y_axis(splitLine = list(show = FALSE), show = FALSE) %>%
      e_theme("essos" )
    
  })
  
  output$sentiment_comments <- renderEcharts4r({
    reddit_df_sentiment_comment %>% 
      mutate(by_day = comm_date ) %>% 
      group_by(by_day) %>% 
      filter(by_day > '2019-11-01') %>% 
      vis_river_plot("Sentiment of Reddit's Comments Over Time")
    
    
    
  })
  
  
  
  
  output$reddit_sentiment_Scores <- renderPlotly({
    g <- count_reddit_df_sentiment_comment  %>% 
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
      labs(x = '', y = '',title = " Overall Sentiment Scores of Reddit' Vaccination Related Comments ") +
      theme(legend.position = 'none')
    
    ggplotly(g,tooltip = c('n')) %>%
      highlight("plotly_selected")
  })
  
  
  output$reddit_table <- renderReactable({
    reactable(
      reddit_df_sentiment_comment %>%
        select(user,subreddit,comm_date,positive,negative),
      filterable = TRUE,
      searchable = TRUE,
      sortable = TRUE,
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
# mod_reddit_summaries_sentiment_ui("reddit_summaries_sentiment_ui_1")
    
## To be copied in the server
# callModule(mod_reddit_summaries_sentiment_server, "reddit_summaries_sentiment_ui_1")
 
