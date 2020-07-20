# Libraries
library(tidyverse)
library(rvest)

# Custom functions
parse_review <- function(review){
  
  # Review content
  review_id <- review %>% html_attr("id")
  
  review_title <- review %>% 
    html_node(".review-content__title") %>%
    html_text() %>%
    str_squish()
  
  review_text <- review %>% 
    html_node(".review-content__text") %>%
    html_text() %>%
    str_squish()
  
  review_stars <- review %>%
    html_node(".review-content-header") %>%
    html_node("img") %>%
    html_attr("alt")
  
  date_temp <- review %>%
    html_node(".review-content-header") %>%
    html_node("script") %>%
    html_text() %>%
    str_squish() %>%
    jsonlite::fromJSON()
  
  review_timestamp <- date_temp$publishedDate
  
  # The consumer information
  
  consumer_id <- review %>%
    html_node(".consumer-information") %>%
    html_attr("href") %>%
    str_remove("/users/")
  
  consumer_name <- review %>%
    html_node(".consumer-information__name") %>%
    html_text() %>%
    str_squish()
  
  consumer_review_count <- review %>%
    html_node(".consumer-information__review-count") %>%
    html_text() %>%
    str_squish()
  
  tibble(review_id, 
         review_title, 
         review_text,
         review_stars,
         review_timestamp,
         consumer_id,
         consumer_name,
         consumer_review_count)
  
}

# Script
url <- "https://www.trustpilot.com/review/udemy.com"

html_data <- read_html(url)

reviews <- html_data %>% html_nodes(".review")

df <- map_dfr(reviews, parse_review)

# All pages

total_reviews <- html_data %>%
  html_node(".headline__review-count") %>%
  html_text() %>%
  str_squish() %>%
  as.integer()

total_pages <- ceiling(total_reviews / 20)

pb <- progress::progress_bar$new(total = total_pages)

all_reviews <- map_dfr(1:total_pages, function(page){
  
  pb$tick()
  
  url <- glue::glue("https://www.trustpilot.com/review/udemy.com?page={page}")
  
  html_data <- read_html(url)
  
  reviews <- html_data %>% html_nodes(".review")
  
  df <- map_dfr(reviews, parse_review)
  
  df
  
})

all_reviews <- all_reviews %>% distinct()

# Clean df

clean_reviews <- all_reviews %>%
  mutate(
    review_stars = review_stars %>% str_extract_all("\\d") %>% unlist() %>% as.integer(),
    consumer_review_count = consumer_review_count %>% str_extract_all("\\d+") %>% unlist() %>% as.integer(),
    review_timestamp = review_timestamp %>% lubridate::ymd_hms()
    )
  
