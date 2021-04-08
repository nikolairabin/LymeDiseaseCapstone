# Web scrapping example

library(rvest)
library(tidyr)

df <- read.csv("ws_example.csv")

df$family <- "unknown"

for(i in 1:nrow(df)){
  wiki <- paste0("https://en.wikipedia.org/wiki/", gsub(" ", "_", df$species[i]))
  wiki <- read_html(wiki)
  species_info <- wiki %>% 
    html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
    html_table()
  species_info <- species_info[[1]]
  
  if(length(species_info[grep("Family", species_info[,1]), 2]) <= 0 ){
    species_info <- wiki %>% 
      html_nodes(xpath='//*[@id="mw-content-text"]/div/table[2]') %>%
      html_table()
    species_info <- species_info[[1]]
  } 
  
  df$family[i]<- species_info[grep("Family", species_info[,1]), 2]
}
