#' news_summaries_value_box UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_news_summaries_value_box_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    div( fluidRow(    
      
      bs4ValueBoxOutput(ns("total_n_articles") ) ,
      bs4ValueBoxOutput(ns("total_n_outlets")     ),
      bs4ValueBoxOutput(ns("total_n_users")) ,
      #bs4ValueBoxOutput(ns("positive_words"))),
    ), style='width: 1200px; height: 150px' ,class = "span16")
  )
  
  

}
    
#' news_summaries_value_box Server Function
#'
#' @noRd 
mod_news_summaries_value_box_server <- function(input, output, session){
  ns <- session$ns
  output$total_n_articles <- renderbs4ValueBox({
    bs4ValueBox(
      value =   all_news %>% 
        dim() %>% 
        pluck(1),
      subtitle = "Articles",
    #  width = 2,
      icon = 'rss',
      status = "success",
    )
  })
  
  output$total_n_outlets <- renderbs4ValueBox({
    bs4ValueBox(
      value = all_news %>%
        count(name) %>% 
        dim() %>% 
        pluck(1),
      subtitle = "Outlets",
     # width = 2,
      icon = 'rss',
      status = "success",
    )
  })
  
  output$total_n_users <- renderbs4ValueBox({
    bs4ValueBox(
      value =   all_news %>%
        count(author) %>% 
        dim() %>% 
        pluck(1),
      subtitle = "Authors",
     # width = 2,
      icon = 'rss',
      status = "success",
    )
  })
  
}
    
## To be copied in the UI
# mod_news_summaries_value_box_ui("news_summaries_value_box_ui_1")
    
## To be copied in the server
# callModule(mod_news_summaries_value_box_server, "news_summaries_value_box_ui_1")
 
