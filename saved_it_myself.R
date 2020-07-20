library(tidyverse)
library(rvest)

# Custom functions
parse_product <- function(product){
  
  product_title <- product %>%
    html_node(".co-product__title") %>%
    html_text()
  
  product_volume <- product %>%
    html_node(".co-product__volume") %>%
    html_text()
  
  product_price <- product %>%
    html_node(".co-product__price") %>%
    html_text()
  
  review_count <- product %>%
    html_node(".co-product__review-count") %>%
    html_text()
  
  rating_stars <- product %>%
    html_node(".rating-stars") %>%
    html_attr("aria-label")
  
  tibble(product_title, product_volume, product_price, review_count, rating_stars)
  
}

# Parse html
html_files <- list.files("cookies/", pattern = ".html") %>% str_c("cookies/",.)

all_products <- map_dfr(html_files, function(html_file){
  
  html_data <- read_html(html_file)
  
  products <- html_data %>%
    html_node(".co-product-list") %>%
    html_nodes(".co-item")
  
  df <- map_dfr(products, parse_product)
  
  df
})

### Clean up data ##############################################################

parse_volume <- function(product_volume){
  
  map_dbl(product_volume, function(x){
    
    if(!str_detect(x, "\\d")){
      return(NA_real_)
    }
    
    to_eval <- x %>%
      str_remove("[[:alpha:]]+$") %>%
      str_replace("x", "*")
    
    volume <- eval(parse(text = to_eval))
    
    volume
    
  })
  
}

clean_data <- all_products %>%
  mutate(
    product_price = product_price %>% str_remove("£") %>% as.numeric(),
    review_count = review_count %>% str_remove_all("\\(|\\)|\\+") %>% as.integer(),
    rating_stars = rating_stars %>% str_extract("\\d{1}.{0,1}\\d{0,2}") %>% as.numeric(),
    volume = parse_volume(product_volume),
    volume_unit = product_volume %>% str_extract("[[:alpha:]]+$")
  )


