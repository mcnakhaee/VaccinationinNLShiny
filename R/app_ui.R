#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import flexdashboard
#' @import bs4Dash
#' @noRd
app_ui <- function(request) {
    # Leave this function for adding external resources
    # List the first level UI elements here 
      tmpsidebar <- bs4DashSidebar(
        skin = "light",
        status = "primary",
        title = "Vaccination in Media",
        brandColor = "primary",
        url = "http://mcnakhaee.com",
        src = "https://git.snt.utwente.nl/uploads/-/system/group/avatar/23/utfavicon.png",
        elevation = 3,
        opacity = 0.8,
        width = 20,
        bs4SidebarMenu(
          bs4SidebarHeader("Content"),
          bs4SidebarMenuItem("Summary",
                             tabName = "summary",
                             icon = "book"),
          bs4SidebarMenuItem("In the News",
                             tabName = "media",
                             icon = "feed"),
          bs4SidebarMenuItem("On Twitter",
                             tabName = "twitter",
                             icon = "twitter"),
          bs4SidebarMenuItem("On Reddit",
                             tabName = "reddit",
                             icon = "reddit"),
          bs4SidebarMenuItem("About",
                             tabName = "about",
                             icon = "desktop")

        )
      )
    
      footer <- bs4DashFooter(
        copyrights = a(href = "mcnakhaee.com",
                       target = "_blank", "mcnakhaee.com"),
        right_text = "2020"
      )
      
      
      
      summary_tab<- bs4TabItem(
        
        tabName = "summary",
        
        fluidRow(
          
          bs4Card(
            mod_news_summaries_value_box_ui("news_summaries_value_box_ui_1"),
            mod_news_summaries_sentiment_ui("news_summaries_sentiment_ui_1"),
            inputId = "card_news_summ",
            title = "News",
            width = 12,
            status = "success",
            closable = FALSE,
            maximizable = TRUE,
            collapsible = TRUE
            
          )
          
        ),
        fluidRow(
          bs4Card(
            fluidRow(    mod_tweet_summaries_value_box_ui("tweet_summaries_value_box_ui_1")),
            mod_tweet_summaries_sentiment_ui("tweet_summaries_sentiment_ui_1"),
            inputId = "card_twitter_summ",
            title = "Twitter",
            width = 12,
            status = "info",
            closable = FALSE,
            maximizable = TRUE,
            collapsible = TRUE
            
          )

          
        ),
        fluidRow(
          
          bs4Card(
            fluidRow(mod_reddit_summaries_value_box_ui("reddit_summaries_value_box_ui_1")),
            mod_reddit_summaries_sentiment_ui("reddit_summaries_sentiment_ui_1"),
            inputId = "card1",
            title = "Reddit",
            width = 12,
            status = "warning",
            closable = FALSE,
            maximizable = TRUE,
            collapsible = TRUE
            
          )
          
          

          
        )
        
        
        
      )
      
      twitter_tab <- bs4TabItem(
        tabName = "twitter",
        fluidPage(
          mod_tweet_hashtag_words_ui("tweet_hashtag_words_ui_1"))
   )
      
      reddit_tab <- bs4TabItem(
        tabName = "reddit",
        fluidPage(
          mod_reddit_stats_ui("reddit_stats_ui_1")))  
      media_tab <- bs4TabItem(
        tabName = "media",
        fluidPage(
          mod_news_stats_ui("news_stats_ui_1"),
          mod_news_wordcloud_ui("news_wordcloud_ui_1"),
          #mod_news_topicmodeling_ui("news_topicmodeling_ui_1")
          ))  
      
      body <- bs4DashBody(
    bs4TabItems(
      summary_tab,
      media_tab,
      twitter_tab,
      reddit_tab
    )
      
      )
      
      tagList(
        # Leave this function for adding external resources
        
        golem_add_external_resources(),
        
        bs4DashPage(
          old_school = FALSE,
          sidebar_min = TRUE,
          sidebar_collapsed = FALSE,
          controlbar_collapsed = FALSE,
          controlbar_overlay = FALSE,
          title = "Vaccination in the Netherlands",
          navbar = bs4DashNavbar(),
          sidebar = tmpsidebar,
          #controlbar = right_sidebar,
          footer = footer,
          body = body
        )
        
      )
      
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'vaccinationinNL'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

