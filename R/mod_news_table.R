#' news_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import reactable
mod_news_table_ui <- function(id){
  ns <- NS(id)
  tagList(

    
  )
}
    
#' news_table Server Function
#'
#' @noRd 
mod_news_table_server <- function(input, output, session){
  ns <- session$ns

    
    

}
    
## To be copied in the UI
# mod_news_table_ui("news_table_ui_1")
    
## To be copied in the server
# callModule(mod_news_table_server, "news_table_ui_1")
 
