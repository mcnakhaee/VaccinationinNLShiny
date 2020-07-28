#' news_stats UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_news_stats_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(fluidRow(
      column( width = 6,
              
              
              div(
                
                bs4Card(
                  plotlyOutput(ns('top_outlets'),width = "550px", height = "650px"),
                  inputId = "top_news_dutch",
                  title = "Top Dutch-Speaking Outlets with Vaccine Related Articles",
                  width = 12,
                  status = "success",
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
                   pickerInput(
                     inputId = ns("select_outlet"),
                     label = "Select an outlet", 
                     choices = all_news_sentiment_description %>% distinct(name) %>% pull(),
                     selected = all_news_sentiment_description %>% distinct(name) %>% pull() %>% pluck(2)
                   ),
                   echarts4rOutput(ns('sentiment_per_outlet'),width = "600px", height = "500px"),
                   inputId = "sentiment_outlet",
                   title = "Overall Sentiment Scores for Different Outlets",
                   width = 12,
                   status = "success",
                   closable = FALSE,
                   maximizable = TRUE,
                   collapsible = TRUE
                   
                 ), style='width: 600px; height: 700px' ,class = "span16" 
                 
               ) ,
               

               
               
             )
             
      )
    #  ,style='width: 1100px; height: 1000px' ,class = "span16" 
      )
    )
  )
}
    
#' news_stats Server Function
#'
#' @noRd 
mod_news_stats_server <- function(input, output, session){
  ns <- session$ns

   
   output$top_outlets <- renderPlotly({
     
     g <- all_news  %>% 
       count(name,sort = TRUE) %>% 
       mutate(name = fct_reorder(name,n,.desc = FALSE)) %>% 
       slice(1:27) %>% 
       ggplot(aes(x = name,y=n )) +
       geom_col(fill = '#00a896',alpha = 0.8) +
       coord_flip() +
       labs(X ='', y = '',title = 'Top Dutch-Speaking Outlets \nwith Vaccine Related Articles') +
       theme_fivethirtyeight() +
       theme(plot.title = element_text(size = 14))
     ggplotly(g)
     
     
   })

 
   output$sentiment_per_outlet <- renderEcharts4r({
     
      all_news_sentiment_description  %>% 
       filter(name == input$select_outlet ) %>% 
       pivot_longer(positive:anger,names_to  = 'sentiment') %>% 
       group_by(sentiment) %>% 
       summarise( mean_sent = mean(value),
                  max_sent = max(value),
                  min_sent = min(value)) %>% 
       ungroup() %>% 
     e_charts(sentiment) %>% 
     e_radar(mean_sent, max = 6, name = "Average Sentiment Value") %>%
     e_radar(max_sent, max = 6, name = "Maximum Sentiment Value") %>%
    e_radar(min_sent, max = 6, name = "Minimum Sentiment Value")   %>% 
       e_tooltip(trigger = "axis") %>%
       e_legend(padding = 30) %>% 
       e_title(text = paste('Overall Sentiment Scores for',input$select_outlet), left = "center") %>%
       #e_x_axis(splitLine = list(show = FALSE)) %>%
       #e_y_axis(splitLine = list(show = FALSE), show = FALSE) %>%
       e_animation(duration = 1000) %>% 
       e_theme("essos" ) 

     
     
   })
 
 
}
    
## To be copied in the UI
# mod_news_stats_ui("news_stats_ui_1")
    
## To be copied in the server
# callModule(mod_news_stats_server, "news_stats_ui_1")
 
