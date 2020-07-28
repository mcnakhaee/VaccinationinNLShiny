#' twitter_stats UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_twitter_stats_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' twitter_stats Server Function
#'
#' @noRd 
mod_twitter_stats_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_twitter_stats_ui("twitter_stats_ui_1")
    
## To be copied in the server
# callModule(mod_twitter_stats_server, "twitter_stats_ui_1")
 
