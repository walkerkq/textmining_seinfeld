library(tm)
library(RWeka)
library(stringr)

text <- read.csv("seinfeld_scripts.csv", stringsAsFactors=F)
text$words.clean <- gsub("\\s*\\([^\\)]+\\)", "", text$words)

by.speaker <- NULL
speakers <- c("ELAINE", "FRANK", "GEORGE", "JERRY", "KRAMER", "NEWMAN")
for(j in speakers){
     sub <- text[text$speaker %in% j, ]
     speaker.text <- paste(sub$words.clean, collapse=" ")
     row <- data.frame(speaker=j, words=speaker.text)
     by.speaker <- rbind(by.speaker, row)
     
}
all.other <- paste(text$words.clean[!(text$speaker %in% by.speaker$speaker)], collapse=" ")
by.speaker <- rbind(by.speaker, data.frame(speaker="OTHER", words=all.other))

myReader <- readTabular(mapping=list(content="words", id="speaker"))
corpus <- Corpus(DataframeSource(by.speaker), readerControl=list(reader=myReader))

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
all.tdm.8 <- removeSparseTerms(all.tdm, 0.8) # 6520 / 14311

# bigrams
bigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
bi.tdm <- TermDocumentMatrix(corpus, control = list(tokenize = bigramTokenizer))
bi.tdm.75 <- removeSparseTerms(bi.tdm, 0.75) # 5889 / 114562

# trigrams
trigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
tri.tdm <- TermDocumentMatrix(corpus, control = list(tokenize = trigramTokenizer))
tri.tdm.75 <- removeSparseTerms(tri.tdm, 0.75) # 826 / 170940

# save as a simple data frame
count.n <- data.frame(inspect(all.tdm.8)) 
count.n$gram <- 1
count.b <- data.frame(inspect(bi.tdm.75)) 
count.b$gram <- 2
count.t <- data.frame(inspect(tri.tdm.75))
count.t$gram <- 3

count.all <- rbind(count.n, count.b, count.t)
count.all$sum <- rowSums(count.all[,c(1:7)])
count.all$word <- row.names(count.all)
count.all <- count.all[,c(10,1:9)]

#write.csv(count.all, "seinfeld_tdm_speaker.csv", row.names=FALSE)


