library(echarts4r)
library(tidyverse)
vis_river_plot <- function(data,title,subtitle =''){
  data %>% 
  summarise(positive = sum( (positive)),
            negative = sum( (negative)),
            anger = sum( (anger)),
            fear = sum( (fear)),
            sadness = sum( (sadness)),
            disgust = sum( (disgust)),
            surprise = sum( (surprise)),
            anticipation = sum( (anticipation)),
            trust = sum( (trust)),
            joy = sum( (joy))
  ) %>% 
    e_charts(by_day) %>%
    e_river(positive,
            smooth = TRUE,rm_x = FALSE, rm_y = FALSE) %>%
    e_river(negative,
            smooth = TRUE,rm_x = FALSE, rm_y = FALSE) %>%
    e_river(anger,
            smooth = TRUE,rm_x = FALSE, rm_y = FALSE) %>%
    e_river(fear,
            smooth = TRUE,rm_x = FALSE, rm_y = FALSE) %>%
    e_river(sadness) %>%
    e_river(disgust) %>%
    e_river(surprise) %>%
    e_river(anticipation) %>%
    e_river(trust) %>%
    e_river(joy,show = FALSE) %>%
    #e_datazoom(x_index = c(0,1), type = "slider") %>% 
    e_color(c('#118ab2',
              '#ef476f',
              '#d62828',
              '#f77f00',
              '#003049',
              '#b5838d',
              '#ffd166',
              '#028090',
              '#06d6a0',
              '#BDB2FF',
              '#CAFFBF')) %>% 
    e_tooltip(trigger = "axis") %>%
    e_legend(padding = 50) %>% 
    e_title(text = title, left = "center",subtext = subtitle) %>%
    e_x_axis(splitLine = list(show = FALSE)) %>%
    e_y_axis(splitLine = list(show = FALSE), show = FALSE) %>%
    e_animation(duration = 1000) %>% 
    e_theme("essos" ) 
}


