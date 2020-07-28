#' news_topicmodeling UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_news_topicmodeling_ui <- function(id){
  ns <- NS(id)
  tagList(
 # fluidRow(
 #   div(
 #     
 #     bs4Card(
 #       visNetworkOutput(ns('topic_news_vis'),width = "1000px" , height = "700px"),
 #       inputId = "hashtag_user",
 #       title = "A Network of Hashtags-User",
 #       width = 12,
 #       status = "success",
 #       closable = FALSE,
 #       maximizable = TRUE,
 #       collapsible = TRUE
 #       
 #     ), style='width: 1200px; height: 700px' ,class = "span16" 
 #     
 #   )
 # )
  )
}
    
#' news_topicmodeling Server Function
#'
#' @noRd 
mod_news_topicmodeling_server <- function(input, output, session){
  ns <- session$ns
  # 
  # output$topic_news_vis <- renderVisNetwork({
  #   
  #   visNetwork(
  #     nodes =  d_nodes %>% distinct(id,.keep_all = TRUE) %>% 
  #       mutate(
  #         color.highlight.background = "firebrick", 
  #         color.highlight.border = "firebrick",
  #         color.border = color,
  #         font.color.highlight = "firebrick"
  #       ) %>%
  #       rename(color.background = color),
  #     edges = d_edges
  #   ) %>%
  #     
  #     visNodes(chosen = list(label = htmlwidgets::JS("function(values, id, selected, hovering){values.color='firebrick'}"))) %>% 
  #     
  #     # use straight edges to improve rendering performance
  #     visEdges(smooth = FALSE, color = list(opacity = 0.5)) %>%
  #     
  #     # configure layouting algorithm
  #     visPhysics(
  #       solver = "forceAtlas2Based",
  #       timestep = 1,
  #       minVelocity = 1,
  #       maxVelocity = 30,
  #       forceAtlas2Based = list(gravitationalConstant = -800, damping = 1),
  #       stabilization = list(iterations = 600, updateInterval = 10),
  #       adaptiveTimestep = TRUE
  #     )
  #   
  # })
  
  
  # output$topic_wordcloud <- renderEcharts4r({
  # 
  # m_topic <- stm(d_sparse, K = 8, verbose = TRUE, max.em.its = 75)
  # 
  # d_beta <- tidy(m_topic)
  # # select only one of the terms that map to the same lemmatised term
  # d_top_terms <- d_beta %>% 
  #   group_by(topic, term) %>% 
  #   summarise(beta = max(beta)) %>% 
  #   
  #   # select the top words by topic
  #   group_by(topic) %>%
  #   top_n(100, beta) %>%
  #   mutate(top_word = beta == max(beta)) %>% 
  #   arrange(topic) %>%
  #   ungroup() %>% 
  #   rename(term = term)
  # 
  # d_gamma <- tidy(m_topic, matrix = "gamma", document_names = rownames(d_sparse))
  # 
  # # compute topic prevalence
  # d_gamma_terms <- 
  #   d_gamma %>%
  #   group_by(topic) %>%
  #   summarise(gamma = mean(gamma)) %>%
  #   arrange(desc(gamma)) %>%
  #   left_join(d_top_terms, by = "topic")
  # 
  # m_corr <- stm::topicCorr(m_topic, cutoff = 0.1)  # 0.075
  # 
  # # extract adjacency matrix from topic correlations and build a network via igraph
  # 
  # d_top_terms %>% 
  #   filter(topic == 1) %>% 
  #   e_charts(x = topic) %>% 
  #   e_title(subtext = "Word frequencies", left = "center") %>% 
  #   e_theme("walden")
  # })
  
}
    
## To be copied in the UI
# mod_news_topicmodeling_ui("news_topicmodeling_ui_1")
    
## To be copied in the server
# callModule(mod_news_topicmodeling_server, "news_topicmodeling_ui_1")

