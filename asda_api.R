library(tidyverse)

url <- "https://groceries.asda.com/api/items/search?requestorigin=gi&storeid=4565&htmlassociationtype=0&listType=12&fromgi=gi&cacheable=true&productperpage=56&pagenum=1&keyword=cookies"

data <- jsonlite::fromJSON(url)

items <- data$items
