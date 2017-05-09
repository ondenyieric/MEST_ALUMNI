setwd("/home/eric/Desktop/MEST")
library(ggplot2)
library(jsonlite)
library(sqldf)
library(tm)
library(SnowballC)
library(wordcloud)
require(RColorBrewer)
MEST_DATAFRAME<-jsonlite::fromJSON("all_mest.json", simplifyDataFrame = TRUE)


#VALIDATE
count<- sqldf("select count(DISTINCT name) from MEST_DATAFRAME as DISTINCT_PEOPLE")


#CLEAN
CLEAN_MEST_DATAFRAME<- sqldf("select DISTINCT name, img_name , img_url, link, subline, subline_2, subline_3 from MEST_DATAFRAME")

#SEGMENT
CLEAN_MEST_KENYA <- sqldf("select * from CLEAN_MEST_DATAFRAME where subline_2='Kenya' ")
CLEAN_MEST_GHANA <- sqldf("select * from CLEAN_MEST_DATAFRAME where subline_2='Ghana' ")
CLEAN_MEST_NIGERAIA <- sqldf("select * from CLEAN_MEST_DATAFRAME where subline_2='Nigeria' ")

#corpus things
career_corpus <- VCorpus(VectorSource(CLEAN_MEST_DATAFRAME[["subline"]]))

print(career_corpus)

as.character(career_corpus[["13"]])

career_corpus_clean <- tm_map(career_corpus,
content_transformer(tolower))

career_corpus_clean <- tm_map(career_corpus_clean, removeNumbers)

career_corpus_clean <- tm_map(career_corpus_clean,
removeWords, stopwords())


replacePunctuation <- function(x) {
gsub("[[:punct:]]+", " ", x)
}

p <- ggplot(CLEAN_MEST_DATAFRAME,aes(x=subline_2)) +  geom_bar()+scale_color_gradientn(colours = rainbow(5))
p + coord_flip()
ggsave("MESTALUMNI.png")
#career_corpus_clean <- tm_map(career_corpus_clean, stemDocument)

career_corpus_clean <- tm_map(career_corpus_clean, stripWhitespace)

wordcloud(career_corpus_clean, min.freq = 5, random.order = FALSE, ordered.colors=TRUE)

pal2 <- brewer.pal(8,"Dark2")
wordcloud(career_corpus_clean, min.freq = 0, random.order = FALSE, colors=pal2)

png("MEST.png", width=600,height=500)
wordcloud(career_corpus_clean, min.freq = 0, random.order = FALSE, colors=pal2)
dev.off()

career_dtm <- DocumentTermMatrix(career_corpus_clean)
#career_dtm <- DocumentTermMatrix(career_corpus_clean, control = list(
#tolower = TRUE,

#removeNumbers = TRUE,
#stopwords = TRUE,
#removePunctuation = TRUE,
#stemming = FALSE
#))
as.data.frame(findFreqTerms(career_dtm, 5)


dtm <- DocumentTermMatrix(career_corpus_clean, control = list(tokenize = BigramTokenizer, weighting=weightTf))
freqs <- as.data.frame(inspect(dtm))
colSums(freqs)



m <- as.matrix(career_dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

findAssocs(career_dtm, c("eit", "entrepreneur", "cofounder"), c(0.1, 0.1, 0.1))

p <- ggplot(CLEAN_MEST_DATAFRAME,aes(x=subline_2)) + geom_bar() + theme_minimal()
ggsave(p)

local({
## Print result
groups <- rk.list (CLEAN_MEST_DATAFRAME[["subline_2"]])
title <- paste (names (groups), collapse=" by ")
x <- table (interaction (groups))

rk.header ("Pie chart", parameters=list ("Tabulation groups"=paste (names (groups), collapse=" by "), "Tabulation statistic"="Frequency", "Orientation"="Counter clockwise"))

rk.graph.on ()
try ({
	pie(x, clockwise =0, main=title, sub="Frequency")
})
rk.graph.off ()
})