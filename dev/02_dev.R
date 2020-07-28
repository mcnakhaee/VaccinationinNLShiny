# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package( "thinkr" )
usethis::use_package( "dplyr" )
usethis::use_package( "ggplot2" )
usethis::use_package( "tidyr" )
usethis::use_package( "lubridate" )
usethis::use_package( "rtweet" )
usethis::use_package( "shinythemes" )
usethis::use_package( "tidytext" )
#usethis::use_package( "ggpage" )
usethis::use_package( "topicmodels" )
#usethis::use_package( "wordcloud2" )
usethis::use_package( "visNetwork" )
usethis::use_package( "stringr" )
usethis::use_package( "stm" )
#usethis::use_package( "rvest" )
usethis::use_package( "trendyy" )
usethis::use_package( "gtrendsR" )
#usethis::use_package( "spacyr" )
usethis::use_package( "wikipediatrend" )
usethis::use_package( "echarts4r" )
usethis::use_package( "WikipediR" )
usethis::use_package( "tidyverse", type = "depends" )
usethis::use_package( "flexdashboard" )
usethis::use_package( "shinyWidgets" )
usethis::use_package( "fresh" )
usethis::use_package( "glue" )
usethis::use_package( "RedditExtractoR" )
usethis::use_package( "tweetbotornot" )
usethis::use_package( "newsanchor" )
usethis::use_package( "bs4Dash" )
#usethis::use_package( "leaflet" )
usethis::use_package( "reactable" )
#usethis::use_package( "leaflet.extras" )
usethis::use_package( "plotly" )
usethis::use_package( "shinydashboard" )
usethis::use_package( "shinycssloaders" )
usethis::use_package( "ggthemes" )
usethis::use_package( "DT" )

usethis::use_package('shinyWidgets')



#usethis::use_package( "thinkr" )
## Add modules ----
## Create a module infrastructure in R/
#golem::add_module( name = "collect_tweets" ) # Name of the module

golem::add_module( name = "about" ) 
golem::add_module( name = "tweet_summaries_value_box" ) 
golem::add_module( name = "tweet_summaries_sentiment" ) 
golem::add_module( name = "news_summaries_value_box" ) 
golem::add_module( name = "news_summaries_sentiment" ) 
golem::add_module( name = "reddit_summaries_value_box" ) 
golem::add_module( name = "reddit_summaries_sentiment" ) 
golem::add_module( name = "tweet_hashtag_words" ) 

golem::add_module( name = "reddit_stats" )
golem::add_module( name = "twitter_stats" ) 
golem::add_module( name = "news_stats" ) 


golem::add_module( name = "reddit_wordcloud" )
golem::add_module( name = "twitter_wordcloud" ) 
golem::add_module( name = "news_wordcloud" ) 


golem::add_module( name = "reddit_table" )
golem::add_module( name = "twitter_table" ) 
golem::add_module( name = "news_table" ) 
golem::add_module( name = "news_topicmodeling" ) 

## Add helper functions ----
## Creates ftc_* and utils_*
#golem::add_fct( "themes" ) 
#golem::add_utils( "load_latest_tweets" )
#golem::add_utils( "load_news" )
#golem::add_utils( "sentiment_analysis" )
#golem::add_utils( "bot_detection" )
#golem::add_utils( "entity_recognition" )
golem::add_utils( "vis_functions" )

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( "collect_tweets", open = FALSE ) 
usethis::use_data_raw( "collect_news", open = FALSE ) 
usethis::use_data_raw( "collect_reddit", open = FALSE ) 
usethis::use_data_raw( "hashtags_social_network", open = FALSE ) 


#usethis::use_data()

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("FarsiTwitter")
devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

