#question 1
source("require_packages.R")
require_packages(c(
  "xml2",
  "httr", "curl", "tidyverse", "dplyr", "tidytext", "wordcloud"
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
wordcloud( top_100$word , top_100 $n)

nrc_sentiments <- get_sentiments("nrc")
nrc_sentiments
question_nrc_sentiments <- word_count %>% inner_join ( nrc_sentiments )
question_nrc_sentiments
View(question_nrc_sentiments)
#plot nrc graph
nrc_graph <- question_nrc_sentiments %>%
  ggplot ( aes (x = sentiment , y = n , fill = sentiment )) +
  geom_col ( show.legend = FALSE ) +
  labs ( title = " Frequency of NRC sentiments on BBC website ")
nrc_graph
ggsave ( "nrc_ graph.pdf" , plot = nrc_graph , width = 8, height =
           4.5)




