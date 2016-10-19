library(XML)
library(RCurl)
#setwd("/Users/kwalker/git_projects/textmining_seinfeld")
setwd("/Users/kaylinwalker/R/textmining_seinfeld")

URL <- "http://www.imsdb.com/TV/Seinfeld.html"
html <- htmlTreeParse(getURL(URL, followlocation=TRUE), useInternal=TRUE)
episodes <- data.frame(link=xpathSApply(html, "//table//tr//td//p//a", xmlGetAttr, "href"),
                       textdata=xpathSApply(html, "//table//tr//td//p", xmlValue))
episodes$title <- sapply(episodes$textdata, function(x) strsplit(as.character(x), " \\(|\\)")[[1]][1])
episodes$date <- sapply(episodes$textdata, function(x) strsplit(as.character(x), " \\(|\\)")[[1]][2])
episodes$writers <- sapply(episodes$textdata, function(x) strsplit(as.character(x), " \\(|\\)")[[1]][3])
episodes$title2 <- gsub(" ", "-", episodes$title)
episodes <- episodes[,c(1,3:6)]

#write.csv(episodes[,2:4], "seinfeld_episodes.csv", row.names=F)

URLstart <- "http://www.imsdb.com/transcripts/Seinfeld-"
all.text <- NULL
for(h in seq_along(episodes$link)) {
     URL <- paste(URLstart, episodes$title2[h], ".html", sep="")
     script <- suppressWarnings(readLines(URL))
     end <- length(script) - 61
     script <- script[233:end]
     script <- gsub("\\<b>|\\</b>|<|\\/", "", script)
     script <- gsub("^\\s+|\\s+$", "", script)
     x <- script
     
     locations <- NULL
     blank.locations <- NULL
     
     for (j in seq_along(x)) {
          location <- j
          if(x[j]=="") blank.locations <- c(blank.locations, location)
          test <- gsub("\\.| ", "", x[j])
          if(grepl("^[[:upper:]]+$", test)) {
               if(nchar(x[j]) < 20) {
                    speaker <- x[j]
                    locations <- rbind(locations, c(speaker, location))
               }
          }
     }
     lines <- NULL
     for (line in seq_along(locations[,2])) {
          speaker <- locations[line,1]
          start <- as.numeric(locations[line,2])
          #find delimiting blank line
          cond <- sapply(blank.locations, function(x) as.numeric(x) > start)
          end <- blank.locations[cond]
          if(length(end)==0) { end1 <- max(length(x)) 
          } else { end <- end[1]; end1 <- end - 1 }
          start1 <- start + 1
          words <- x[start1:end1]
          if(length(words) > 1) words <- str_c(words, collapse=" ")
          row <- data.frame(speaker=speaker, words=words, episode=episodes$title[h], num=h, date=episodes$date[h])
          lines <- rbind(lines, row)
     }
     
     all.text <- rbind(all.text, lines)
}
#write.csv(all.text, "seinfeld_scripts.csv", row.names=F)

######### enhance scripts with other data
### RATINGS
URL <- "http://www.imdb.com/title/tt0098904/epdate"
text <- htmlTreeParse(getURL(URL, followlocation=TRUE), useInternal=TRUE)
table <- xpathSApply(text, "//div[@id='tn15content']//table//tr", xmlValue)
table <- gsub("\\\n|\\\t|Â", "", table)
ratings <- NULL
for(i in 2:length(table)){
     h <- strsplit(table[i], "  ")
     row <- data.frame(num=h[[1]][1], title=h[[1]][2], rating=h[[1]][3], votes=gsub(",","", h[[1]][4]))
     for(k in c(1,3,4)) row[,k] <- as.numeric(as.character(row[,k]))
     ratings <- rbind(ratings, row)
}

#write.csv(ratings, "data/seinfeld_ratings.csv", row.names=F)

all.text <- read.csv("data/raw/seinfeld_scripts.csv", stringsAsFactors=F)

### episodes  
episodes <- read.csv("data/seinfeld_episodes.csv", stringsAsFactors=F)
### GIRLFRIENDS
sig.others <- read.csv("data/seinfeld_dates.csv", stringsAsFactors=F)
### GENDER
gender <- read.csv("data/seinfeld_all_speakers.csv", stringsAsFactors=F)

# fix episode names
all.text$episode <- gsub("Good News, Bad News", "The Seinfeld Chronicles", all.text$episode)
all.text$episode <- gsub("The Bottle Deposit Part 1|The Bottle Deposit Part 2", "The Bottle Deposit", all.text$episode)
all.text$episode <- gsub("The Finale Part 1|The Finale Part 2", "The Finale", all.text$episode)
all.text$episode <- gsub("The Raincoats Part 1|The Raincoats Part 2", "The Raincoats", all.text$episode)
all.text$episode <- gsub("The Cadillac Part 1|The Cadillac Part 2", "The Cadillac", all.text$episode)
all.text$episode <- gsub("The Pilot Part 1|The Pilot Part 2", "The Pilot", all.text$episode)

all.text$words <- gsub("\\s*\\([^\\)]+\\)", "", all.text$words) # remove items in parentheses
all.text$word.count <- sapply(all.text$words, function(x) length(strsplit(x, " ")[[1]])) # word count

all.text$season <- 0
all.text$gender <- NA
for(y in seq_along(all.text$episode)){
    h <- episodes[episodes$title %in% all.text$episode[y], ]
    all.text$season[y] <- h$Season
    all.text$gender[y] <- gender[gender$Name %in% all.text$speaker[y], 3]
    all.text$num[y] <- h$Num
}

all.text <- all.text[ ,c(2,1,8,3,4,7,5,6)]
#write.csv(all.text, "data/seinfeld_scripts_enhanced.csv", row.names=F)




