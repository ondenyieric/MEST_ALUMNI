CLEAN_MEST_DATAFRAME_WITH_LOCATION<-read.csv("CLEAN_MEST_DATAFRAME_WITH_LOCATION.csv")


termFreq(CLEAN_MEST_DATAFRAME_WITH_LOCATION[["subline"]])

strsplit_space_tokenizer <- function(x)
    unlist(strsplit(as.character(x), "[[:space:]]+"))
    
    
    
ctrl <- list(tokenize = strsplit_space_tokenizer,
             removePunctuation = list(preserve_intra_word_dashes = TRUE),
             stopwords = c("reuter", "that"),
             stemming = TRUE,
             wordLengths = c(4, Inf))
             
             
             
termFreq(CLEAN_MEST_DATAFRAME_WITH_LOCATION[["subline"]], control = ctrl)