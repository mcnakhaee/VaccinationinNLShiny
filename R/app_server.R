#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  callModule(mod_tweet_summaries_value_box_server, "tweet_summaries_value_box_ui_1")
  callModule(mod_tweet_summaries_sentiment_server, "tweet_summaries_sentiment_ui_1")
  callModule(mod_news_summaries_sentiment_server, "news_summaries_sentiment_ui_1")
  callModule(mod_reddit_summaries_sentiment_server, "reddit_summaries_sentiment_ui_1")
  callModule(mod_tweet_hashtag_words_server, "tweet_hashtag_words_ui_1")
  callModule(mod_reddit_summaries_value_box_server, "reddit_summaries_value_box_ui_1")
  callModule(mod_news_summaries_value_box_server, "news_summaries_value_box_ui_1")
  callModule(mod_reddit_stats_server, "reddit_stats_ui_1")
  callModule(mod_news_wordcloud_server, "news_wordcloud_ui_1")
  #callModule(mod_news_topicmodeling_server, "news_topicmodeling_ui_1")
  callModule(mod_news_stats_server, "news_stats_ui_1")
  

}
