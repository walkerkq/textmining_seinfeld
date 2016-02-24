library("RCurl")
library("XML")
library("stringr")

# get list of episodes
URL <- "http://www.imsdb.com/TV/Seinfeld.html"
listing <- htmlTreeParse(getURL(URL), useInternal=TRUE)
listing.raw <- xpathApply(listing, "//p", xmlValue)
listing.u <- unlist(listing.raw)
listing.s <- strsplit(listing.u, "\\(")
listing.f <- NULL
for(x in seq_along(listing.s)) listing.f[x] <- listing.s[[x]][1]
listing.f <- gsub("^\\s+|\\s+$", "", listing.f)
listing.f <- tolower(listing.f)
listing.f <- gsub("\\s", "-", listing.f)

# loop through episode pages, scrape lines and store in all.text

URLstart <- "http://www.imsdb.com/transcripts/Seinfeld-"
all.text <- NULL
for(h in 1:length(listing.f)) {
    URL <- paste(URLstart, listing.f[h], ".html", sep="")
    script <- readLines(URL)
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
        row <- data.frame(speaker=speaker, words=words, episode=listing.f[h], num=h)
        lines <- rbind(lines, row)
    }
    
    all.text <- rbind(all.text, lines)
    
}

setwd("/Users/kwalker/git_projects/textmining_seinfeld/data/raw data")
#write.csv(all.text, "seinfeld_all_scripts.csv", row.names=FALSE)
