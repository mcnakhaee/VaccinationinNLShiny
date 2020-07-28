#' tweet_summaries_value_box UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tweet_summaries_value_box_ui <- function(id){
  ns <- NS(id)
  tagList(
    

       div( fluidRow(    
          
          bs4ValueBoxOutput(ns("total_n_users") ) ,
          bs4ValueBoxOutput(ns("total_n_tweets")     ),
          bs4ValueBoxOutput(ns("verified")) ,
          #bs4ValueBoxOutput(ns("positive_words"))),
         ), style='width: 1600px; height: 150px' ,class = "span16")
  )
  
  
}
    
#' tweet_summaries_value_box Server Function
#'
#' @noRd 
mod_tweet_summaries_value_box_server <- function(input, output, session){
  ns <- session$ns
 

  output$total_n_tweets <- renderbs4ValueBox({
    bs4ValueBox(
      value = filtered_tweets %>%
        dim() %>% 
        pluck(1),
      subtitle = "Tweets",
      width = 2,
      icon = 'twitter',
      status = "info",
    )
  })
  # output$n_verified <- renderbs4ValueBox({
  #   bs4ValueBox(
  #     value = all_tweets %>% filter(verified) %>% length(),
  #     subtitle = "Verified Tweeters",
  #     width =3,
  #     icon = 'twitter',
  #     status = "info",
  #   )
  # })
  output$total_n_users <- renderbs4ValueBox({
    bs4ValueBox(
      value = filtered_tweets%>%
        count(screen_name) %>% 
        pull(screen_name) %>% 
        length(),
      subtitle = "Tweeters",
      width = 2,
      icon = 'twitter',
      status = "info",
    )
  })
  
  output$verified <- renderbs4ValueBox({
    bs4ValueBox(
        value = filtered_tweets %>% 
          filter(verified) %>% 
          count(screen_name) %>% 
          dim() %>% 
          pluck(1),
      subtitle = "Verified Users",
      width = 2,
      icon = 'twitter',
      status = "info",
    )
  })
  

  
}
    
## To be copied in the UI
# mod_tweet_summaries_value_box_ui("tweet_summaries_value_box_ui_1")
    
## To be copied in the server
# callModule(mod_tweet_summaries_value_box_server, "tweet_summaries_value_box_ui_1")
 
