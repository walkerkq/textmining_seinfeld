library(tm)
library(RWeka)
library(stringr)

# read in the data set
setwd("/Users/kwalker/git_projects/textmining_seinfeld/data/raw data")
scripts <- read.csv("seinfeld_all_scripts.csv", stringsAsFactors=FALSE)

# condense to get rid of episode
by.speaker <- NULL
for(speaker in unique(scripts$speaker)){
    subset <- scripts[scripts$speaker==speaker, ]
    text <- str_c(subset$words, collapse=" ")
    row <- data.frame(speaker, text)
    by.speaker <- rbind(by.speaker, row)
}

# condense low-volume speakers into one to create a manageable corpus
# this keeps 10
by.speaker$text <- iconv(as.character(by.speaker$text), to="UTF-8-MAC", sub='byte')
by.speaker.big <- by.speaker[nchar(as.character(by.speaker$text)) > 12000, ]

# save the rest of the text into one big speaker "All others"
kept.speakers <- unique(as.character(by.speaker.big$speaker))
other.text <- by.speaker[!(by.speaker$speaker %in% kept.speakers), ]
other.text <- str_c(other.text$text, collapse=" ")
other <- data.frame(speaker="All others", text=other.text)

# add it back in
by.speaker <- rbind(by.speaker.big, other); rm(by.speaker.big)

# create corpus
myReader <- readTabular(mapping=list(content="text", id="speaker"))
corpus <- Corpus(DataframeSource(by.speaker), readerControl=list(reader=myReader))

# pre-process text
corpus <- tm_map(corpus,content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')), mc.cores=1)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(removePunctuation), mc.cores=1)
corpus <- tm_map(corpus, content_transformer(removeNumbers))
corpus <- tm_map(corpus, content_transformer(stripWhitespace))
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# create term document matrix
options(mc.cores=1)
allTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 4))
all.tdm <- TermDocumentMatrix(corpus, control = list(tokenize = allTokenizer))

# remove sparse terms
all.tdm.75 <- removeSparseTerms(all.tdm, 0.75) # 13843 / 633609

# save as a simple data frame
count.all <- data.frame(inspect(all.tdm.75)) 
count.all$word <- row.names(count.all)
#setwd("/Users/kwalker/git_projects/textmining_seinfeld/data/tidy data/")
#write.csv(count.all, "seinfeld_tdm_all.csv", row.names=FALSE)
