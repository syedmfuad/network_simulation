library(wordcloud)
library(ggwordcloud)

data <- read.csv("funding_new.csv")

#wordcloud(data$Paper, min.freq=1)

#my_string <- gsub('WW', '', my_string)


word <- unique(data$Paper)
word <- gsub("boilers", "boiler", word)
word <- gsub("cycles", "cycle", word)
word <- gsub("liquors", "liquor", word)
word <- gsub("temperatures", "temperature", word)
word <- gsub("processes", "process", word)

docs <- Corpus(VectorSource(word))
inspect(docs)

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("report", "differences", "part", "data", "model", "analysis", "research", "review",
                                    "behavior", "assessment", "different", "new", "studies", "study", "project", "programme", "formed",
                                    "method", "final", "liekki", "liquorliquor", "blackliquor")) 

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 3,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

d_2 <- subset(d, freq>=3)

ggplot(d_2, aes(label = word, size = freq)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 20) +
  theme_minimal()



ggplot(d_2, aes(
    label = word, size = freq,
    color = factor(sample.int(10, nrow(d_2), replace = TRUE)))) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 30) +
  theme_minimal()



