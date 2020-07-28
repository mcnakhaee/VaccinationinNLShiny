#' news_summaries_sentiment UI Function
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
mod_news_summaries_sentiment_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    
    fluidRow(
      
      div(bs4TabSetPanel(
        id = "tabcard_news",
        side = "left",
        bs4TabPanel(
          tabName = "Articles over Time",
          active = FALSE,
          
          echarts4rOutput(ns('number_news_over_time'),width = "1100px", height = "500px")
        ),
        bs4TabPanel(
          tabName = "Sentiment of Articles' Short Descriptions",
          active = FALSE,
          
          echarts4rOutput(ns('sentiment_description'),width = "1100px", height = "550px")
        ),
        bs4TabPanel(
          tabName = "Sentiment of Articles' Content",
          active = FALSE,
          
          echarts4rOutput(ns('sentiment_content'),width = "1100px", height = "550px")
        ),
        bs4TabPanel(
          tabName = "Overall Sentiment Scores",
          active = FALSE,
          
          plotlyOutput(ns('news_sentiment_Scores'))
        ),
        bs4TabPanel(
          tabName = "Dataset",
          active = FALSE,
          
          reactableOutput(ns('news_table'),width = "1100px", height = "550px")
        )
        
        
        
        
      ),style='width: 1600px; height: 600px' ,class = "span16")
      
    )
    
  )
}
    
#' news_summaries_sentiment Server Function
#'
#' @noRd 
mod_news_summaries_sentiment_server <- function(input, output, session){
  ns <- session$ns
  
  source('R/utils_vis_functions.R')
  output$number_news_over_time <- renderEcharts4r({
    all_news %>% 
      count(by_day) %>% 
      e_charts(by_day) %>%
      e_line(n,
             smooth = TRUE) %>%
      #e_legend(show = TRUE, right = 0) %>%
      e_tooltip(trigger = "axis", formatter = e_tooltip_pointer_formatter("decimal", 0)) %>%
      e_title(subtext = "Number of Vaccination Related News Articles in Dutch Media Over Time", left = "center") %>%
      e_x_axis(splitLine = list(show = FALSE)) %>%
      e_y_axis(splitLine = list(show = FALSE), show = FALSE) %>%
      e_theme("essos" )
    
  })
  
  output$sentiment_description <- renderEcharts4r({
    all_news_sentiment_description %>% 
      group_by(by_day) %>% 
      vis_river_plot("Seniment of Articles' Short Descriptions Over Time")
    
    
    
  })
  

  
  
  output$sentiment_content <- renderEcharts4r({
    all_news_sentiment_content %>% 
      group_by(by_day) %>% 
      vis_river_plot('Seniment of News Articles Over Time')
    
    
    
  })
  
  output$news_table <- renderReactable({
    reactable(
      all_news_sentiment_content %>%
        select(title,url,content,outlet = name,positive,negative),
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
    
  output$news_sentiment_Scores <- renderPlotly({
    g <- count_sentiment_Words_description_stop_words  %>% 
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
      labs(x = '', y = '',title = " Overall Sentiment Scores of Vaccination Related Articles ") +
      theme(legend.position = 'none')
    
    ggplotly(g,tooltip = c('n')) %>%
      highlight("plotly_selected")
  })
  

}
    
## To be copied in the UI
# mod_news_summaries_sentiment_ui("news_summaries_sentiment_ui_1")
    
## To be copied in the server
# callModule(mod_news_summaries_sentiment_server, "news_summaries_sentiment_ui_1")
 
