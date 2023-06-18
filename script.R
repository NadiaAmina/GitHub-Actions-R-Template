#question 1
source("require_packages.R")
require_packages(c(
  "xml2",
  "httr", "curl"
# "tidyverse", "dplyr", "tidytext", "textdata", "wordcloud", "textshaping"
))
#get feed from bbc 
a <- GET("http://feeds.bbci.co.uk/news/rss.xml?edition=uk")
file <- paste0("file",Sys.Date(), ".xml")
curl_download("http://feeds.bbci.co.uk/news/rss.xml?edition=uk",file)
#save feed data in xml
xml <- readLines(file)
xmlString <- paste0 (xml , collapse = "" )
Ourxml <- read_xml (xmlString)
Ourxml
headlines_nodes <- xml_find_all(Ourxml, ".//item/title")
headlines <- xml_text(headlines_nodes)
headlines
desc_nodes <- xml_find_all(Ourxml, ".//item/description")
desc <- xml_text(desc_nodes)
desc 
data <- paste(headlines,desc)
data
writeLines(data)
body <- data %>% str_remove_all("[:punct:]") %>% str_to_lower()
body
n = 1:length(data)
df <- tibble(body, n)
df
#tokenize data
tokens <- df %>% unnest_tokens(word, body)
tokens
tokens_filtered <-tokens %>%
  anti_join(stop_words, by = "word")
word_count <- tokens_filtered %>% count(word, sort=TRUE)
#top 100 from data frame
top_100 <- top_n ( word_count , 100 , wt = n)
top_100
#plot word cloud





