library(tm)
library(RWeka)
library(stringr)

text <- read.csv("data/seinfeld_scripts_enhanced.csv", stringsAsFactors=F)
text$words.clean <- gsub("\\s*\\([^\\)]+\\)", "", text$words)

by.ep <- NULL
for(j in levels(factor(text$num))){
     sub <- text[text$num %in% j, ]
     ep.text <- paste(sub$words.clean, collapse=" ")
     row <- data.frame(episode=sub$episode[1], date=sub$date[1], num=j, key=paste(j, gsub(" ", "-", sub$episode[1]), sep="-"), words=ep.text)
     by.ep <- rbind(by.ep, row)
     
}

myReader <- readTabular(mapping=list(content="words", id="key"))
corpus <- Corpus(DataframeSource(by.ep), readerControl=list(reader=myReader))

# pre-process text
corpus <- tm_map(corpus,content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')), mc.cores=1)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(removePunctuation), mc.cores=1)
corpus <- tm_map(corpus, content_transformer(removeNumbers))
corpus <- tm_map(corpus, content_transformer(stripWhitespace))
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# create term document matrices
options(mc.cores=1)

# ngrams
ngramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 1), paste, collapse = " "), use.names = FALSE)
all.tdm <- TermDocumentMatrix(corpus, control = list(tokenize = ngramTokenizer))
all.tdm.95 <- removeSparseTerms(all.tdm, 0.98) 

# bigrams
bigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
bi.tdm <- TermDocumentMatrix(corpus, control = list(tokenize = bigramTokenizer))
bi.tdm.95 <- removeSparseTerms(bi.tdm, 0.95) 

# trigrams
trigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
tri.tdm <- TermDocumentMatrix(corpus, control = list(tokenize = trigramTokenizer))
tri.tdm.97 <- removeSparseTerms(tri.tdm, 0.98)

# save as a simple data frame
count.n <- data.frame(inspect(all.tdm.95)) 
count.n$gram <- 1
count.b <- data.frame(inspect(bi.tdm.95)) 
count.b$gram <- 2
count.t <- data.frame(inspect(tri.tdm.97))
count.t$gram <- 3

count.all <- rbind(count.n, count.b, count.t)
count.all$sum <- rowSums(count.all[,c(1:172)])
count.all$word <- row.names(count.all)
count.all <- count.all[,c(174, 172, 1:171, 173)]

#write.csv(count.all, "data/seinfeld_tdm_episode.csv", row.names=FALSE)


