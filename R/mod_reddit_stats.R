#' reddit_stats UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import plotly
#' @import ggthemes
mod_reddit_stats_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    
      
      
      
      
      
      div(fluidRow(
        column( width = 6,
                
                
                div(
                  
                  bs4Card(
                    plotlyOutput(ns('top_subreddits'),width = "600px", height = "500px"),
                    inputId = "top_subreddits_dutch",
                    title = "Top Subreddits",
                    width = 12,
                    status = "warning",
                    closable = FALSE,
                    maximizable = TRUE,
                    collapsible = TRUE
                    
                  ), style='width: 650px; height: 700px' ,class = "span16" 
                  
                )
                
                
                
                
        ),
        
        
        column(width = 6,
               
               fluidRow(
                 
                 div(
                   
                   bs4Card(
                     plotlyOutput(ns('top_users'),width = "600px", height = "500px"),
                     inputId = "top_users_card",
                     title = "Top Reddit Users",
                     width = 12,
                     status = "warning",
                     closable = FALSE,
                     maximizable = TRUE,
                     collapsible = TRUE
                     
                   ), style='width: 650px; height: 700px' ,class = "span16" 
                   
                 ) ,
                 
                 
                 
                 
               )
               
        )
        #  ,style='width: 1100px; height: 1000px' ,class = "span16" 
      )
      )
      
      
      
      
    
    
  
  )
}
    
#' reddit_stats Server Function
#'
#' @noRd 
mod_reddit_stats_server <- function(input, output, session){
  ns <- session$ns
 
  
  output$top_subreddits <- renderPlotly({
    g <- reddit_df_sentiment_comment  %>% 
      count(subreddit,sort = TRUE) %>% 
      mutate(subreddit = fct_reorder(subreddit,n,.desc = FALSE)) %>% 
      slice(1:27) %>% 
      ggplot(aes(x = subreddit,y=n )) +
      geom_col(fill = '#ff9f1c',alpha = 0.8) +
      coord_flip() +
      labs(X ='', y = '',title = 'Top Subreddits for Vaccine Related Discussions in Dutch') +
      theme_fivethirtyeight() +
      theme(plot.title = element_text(size = 14))
      
    ggplotly(g)
  })
  
  output$top_users <- renderPlotly({
    g <- reddit_df_sentiment_comment  %>% 
      count(user,sort = TRUE) %>% 
      slice(1:15) %>% 
      ggplot(aes(x = fct_reorder(user,n,.desc = FALSE),y=n ,)) +
      geom_col(fill = '#ff9f1c',alpha = 0.8) +
      coord_flip() +
      labs(X ='', y = '',title = 'Top Reddit Users in Vaccine Related Discussions in Dutch') +
      theme_fivethirtyeight() +
      theme(plot.title = element_text(size = 14))
    ggplotly(g)
  })
  
  # reddit_df_sentiment_comment %>% 
  #   mutate(controversiality = as.factor(controversiality)) %>% 
  #   ggplot(aes(x = controversiality,y=negative))+
  #   geom_boxplot()
  
}
    
## To be copied in the UI
# mod_reddit_stats_ui("reddit_stats_ui_1")
    
## To be copied in the server
# callModule(mod_reddit_stats_server, "reddit_stats_ui_1")
 
