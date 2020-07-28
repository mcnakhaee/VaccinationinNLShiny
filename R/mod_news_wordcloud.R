#' news_wordcloud UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import echarts4r
#' @import stm
mod_news_wordcloud_ui <- function(id){
  ns <- NS(id)
  tagList(
   div( div(fluidRow(
             bs4Card(
               echarts4rOutput(ns("hashtags_wordcloud_news"),width = "900px" , height = "500px"),
               inputId = "news_wc",
               title = "News' Articles Wordcloud ",
               width = 12,
               status = "success",
               closable = FALSE,
               maximizable = TRUE,
               collapsible = TRUE
               
             )      
             
             
      ,style='width: 1100px; height: 500px' ,class = "span16" )
    ),
    div(fluidRow(
      bs4Card(
        pickerInput(
          inputId =ns( "select_topics"),
          label = "Select a Topic", 
          choices = seq(1,8),
          selected = 1
        ),
        echarts4rOutput(ns("topic_wordcloud"),width = "900px" , height = "500px"),
        inputId = ns("news_wc"),
        title = "Wordcloud of Important Terms Extracted from Topic Modeling",
        width = 12,
        status = "success",
        closable = FALSE,
        maximizable = TRUE,
        collapsible = TRUE
        
      )      
      
      
      ,style='width: 1100px; height: 500px' ,class = "span16" )
    )
  ),style='width: 1100px; height: 2500px' ,class = "span16" )
}
    
#' news_wordcloud Server Function
#'
#' @noRd 
mod_news_wordcloud_server <- function(input, output, session){
  ns <- session$ns
 
  
  news_word_cuont_re <- reactive({
    sentiment_Words_description_stop_words %>%
      count(word,name = 'freq',sort = TRUE)
    
    
  })
  
  # twitter_hashtags_cuont_re <- reactive({
  #   
  #   
  # })
  
  output$hashtags_wordcloud_news <- renderEcharts4r({
    news_word_cuont_re()%>% 
      e_color_range(freq,color,c('#05668d','#028090','#00a896','#02c39a','#f0f3bd') ) %>% 
      e_charts() %>% 
      e_cloud( word,freq,color, shape = "circle", sizeRange = c(10, 70)) %>% 
      e_title("Wordcloud of News Words") %>% 
      e_tooltip()
    
  })
  
  
  
  output$topic_wordcloud <- renderEcharts4r({
    
  # 0.075
    
    # extract adjacency matrix from topic correlations and build a network via igraph
    
    d_top_terms %>% 
      filter(topic == input$select_topics) %>%
      e_color_range(beta,color ,c('#7400b8','#6930c3','#5e60ce','#5390d9','#4ea8de','#48bfe3','#56cfe1','#64dfdf','#72efdd','#80ffdb') ) %>% 
      e_charts(x = topic) %>% 
      e_title(text = paste("Word frequencies for Topic",input$select_topics), left = "center") %>%
      e_cloud(term, beta,color,  sizeRange = c(10, 72), width = "90%", height = "70%", rotationRange = c(0, 0)) %>% 
      e_color() %>% 
      e_theme("westeros")
  })
  
}
    
## To be copied in the UI
# mod_news_wordcloud_ui("news_wordcloud_ui_1")
    
## To be copied in the server
# callModule(mod_news_wordcloud_server, "news_wordcloud_ui_1")
 
