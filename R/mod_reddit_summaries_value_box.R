#' reddit_summaries_value_box UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_reddit_summaries_value_box_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(fluidRow(    
      
      # column(width = 4,offset = 0,bs4ValueBoxOutput(ns("total_n_comments") )) ,
      # column(width = 4,offset = 0.,bs4ValueBoxOutput(ns("total_n_subreddits")   )  ),
      # column(width = 4,offset = 0,bs4ValueBoxOutput(ns("total_n_users")) ),
      #column( width = 3,bs4ValueBoxOutput(ns("positive_words"))),
      bs4ValueBoxOutput(ns("total_n_comments") ),
      bs4ValueBoxOutput(ns("total_n_subreddits")   ) ,
      bs4ValueBoxOutput(ns("total_n_users")) ,
      
      
      
    ),style='width: 1600px; height: 150px' ,class = "span16")
  )
}
    
#' reddit_summaries_value_box Server Function
#'
#' @noRd 
mod_reddit_summaries_value_box_server <- function(input, output, session){
  ns <- session$ns
  
  
  output$total_n_comments <- renderbs4ValueBox({
    bs4ValueBox(
      value =   reddit_df_sentiment_comment %>% 
        distinct(comment) %>% 
        dim() %>% 
        pluck(1),
      subtitle = "Comments",
      width = 4,
      icon = 'reddit',
      status = "warning",
    )
  })
  
  output$total_n_subreddits <- renderbs4ValueBox({
    bs4ValueBox(
      value = reddit_df_sentiment_comment %>%
        count(subreddit) %>% 
        dim() %>% 
        pluck(1),
      subtitle = "Subreddits",
      width = 4,
      icon = 'reddit',
      status = "warning",
    )
  })
  
  output$total_n_users <- renderbs4ValueBox({
    bs4ValueBox(
      value =   reddit_df_sentiment_comment %>% 
        distinct(user) %>% 
        dim() %>% 
        pluck(1),
      subtitle = "Users",
      width = 4,
      icon = 'reddit',
      status = "warning",
    )
  })
  



}
    
## To be copied in the UI
# mod_reddit_summaries_value_box_ui("reddit_summaries_value_box_ui_1")
    
## To be copied in the server
# callModule(mod_reddit_summaries_value_box_server, "reddit_summaries_value_box_ui_1")
 
