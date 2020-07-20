library(tidyverse)
library(rvest)

# Get the data
url <- "https://en.wikipedia.org/wiki/COVID-19_pandemic_by_country_and_territory"
html_data <- read_html(url)

df_raw <- html_data %>%
  html_node("#thetable") %>%
  html_table() %>%
  as_tibble(.name_repair = "unique")

# Clean the data
df_clean <- df_raw %>%
  select(location = 2, cases = 3, deaths = 4, recovered = 5) %>%
  slice(-((n()-1):n())) %>% 
  mutate(location = location %>% str_remove_all("\\[.+")) %>%
  mutate_at(c("cases", "deaths", "recovered"), function(x){
    x %>% str_remove_all(",") %>% as.integer()
  })
