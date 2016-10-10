library(XML)
library(RCurl)

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
