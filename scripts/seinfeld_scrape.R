library(XML)
library(RCurl)
setwd("/Users/kwalker/git_projects/textmining_seinfeld")

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
### EPISODE DATA
episodes <- read.csv("data/seinfeld_episodes.csv", stringsAsFactors=F)
episodes$num <- 1:length(episodes$date)
episodes$Jerry <- sapply(episodes$writers, function(x) if(grepl("Jerry Seinfeld", x)){ x <- TRUE } else {x <- FALSE })
episodes$Larry <- sapply(episodes$writers, function(x) if(grepl("Larry David", x)){ x <- TRUE } else {x <- FALSE })
for(o in seq_along(episodes$title)){
    if(episodes$num[o] > 0 & episodes$num[o] <= 5) episodes$season[o] <- 1
    if(episodes$num[o] > 5 & episodes$num[o] <= 17) episodes$season[o] <- 2
    if(episodes$num[o] > 17 & episodes$num[o] <= 40) episodes$season[o] <- 3
    if(episodes$num[o] > 40 & episodes$num[o] <= 64) episodes$season[o] <- 4
    if(episodes$num[o] > 64 & episodes$num[o] <= 86) episodes$season[o] <- 5
    if(episodes$num[o] > 86 & episodes$num[o] <= 108) episodes$season[o] <- 6
    if(episodes$num[o] > 108 & episodes$num[o] <= 132) episodes$season[o] <- 7
    if(episodes$num[o] > 132 & episodes$num[o] <= 154) episodes$season[o] <- 8
    if(episodes$num[o] > 155 & episodes$num[o] <= 176) episodes$season[o] <- 9
}
### GIRLFRIENDS
#http://fusion.net/story/155370/heres-what-happened-to-every-single-woman-jerry-dated-on-seinfeld/
girls <- read.csv("data/seinfeld_girlfriends_seinology.csv", stringsAsFactors=F)
girls <- unique(toupper(girls$The.Girl))
girls <- c(girls, "VANESSA", "MARY", "SHERRY", "ISABEL", "GINA", "NINA", "GAIL", "LAURA", "JODI", "PAULA",
           "KAREN", "MARGARET", "KATYA", "LOIS", "CATHY", "SHELLY", "BRIDGETTE", "GENNICE", "SHEILA", 
           "HALLIE", "PAM", "ABBY", "VALERIE", "MELISSA", "GWEN", "KERI", "SOPHIE", "LISI", "CINDY" )
girls <- girls[girls != "ELAINE"]

georges_girls <- c('MARLENE', 'PATRICE', 'MONICA', 'AUDREY', 'EVIE', 'NOELLE', 'CYNTHIA', 'CARRIE', 'ROBIN',
                   'SUSAN', 'CHERYL', 'ALISON', 'BETSY', 'KAREN', 'GWEN', 'SASHA', 'DIANE', 'DAPHNE', 'ROBBIN',
                   'JANE', 'VICTORIA', 'JULIE', 'LINDSEY', 'NINA', 'BONNIE', 'DENISE', 'PAULA', 'NANCY', 'SIENNA',
                   'MARISA', 'ANNA', 'SHEILA', 'HEATHER', 'LOUISE', 'CELIA', 'ALLISON', 'DANIELLE', 'MARCY',
                   'MARYANN', 'TARA', 'VIVIAN', 'RHISA', 'MIRANDA', 'JANET', 'MAURA')
elaines_bfs <- c("ROBERT", 'OWEN', 'DICK', 'KEITH', 'EDUARDO', 'DR. RESTON', 'DAVOLA', 'DOCTOR',
                 'JOHN F. KENNEDY JR.', 'FRED', 'ROY', 'CARL', 'RUSSELL', 'LLOYD', 'JOEL', 'RICKY', 
                 'TONY', 'PHIL', 'AARON', 'JAKE', 'SIMON', 'NED', 'WHATLEY', 'ROBERT', 'JIMMY', 'PUDDY', 
                 'BOB', 'JAMES', 'FRED', 'BILLY', 'JOHN', 'BOB', 'CRAIG', 'TODD', 'DAVID', 'KEVIN',
                 'BRETT', 'BEN', 'ALAN', 'KURT', 'VINCENT', 'BLAINE', 'HAL', 'MR. LIPPMAN', 'JACK',
                 'PETER', 'GLENN', 'DARRYL', 'ZACH')
kramers_girls <- c("TINA", 'MARION', 'CHELSEA', 'GAIL', 'LOLA', 'LESLIE', 'CHERYL', 'ERICA', 'OLIVE', 'TOBY',
                   'NOREEN', 'WENDY', 'SALLY', 'CONNIE', 'PAM', 'EMILY')
### GENDER
gender <- read.csv("data/all.speakers.csv", stringsAsFactors=F)
all.text <- read.csv("data/seinfeld_scripts.csv", stringsAsFactors=F)
all.text$words <- gsub("\\s*\\([^\\)]+\\)", "", all.text$words) # remove items in parentheses
all.text$word.count <- sapply(all.text$words, function(x) length(strsplit(x, " ")[[1]])) # word count
all.text$jerry_gf <- FALSE
all.text$george_gf <- FALSE
all.text$elaine_bf <- FALSE
all.text$kramer_gf <- FALSE
for(y in seq_along(all.text$episode)){
    h <- episodes[episodes$title %in% all.text$episode[y], ]
    all.text$season[y] <- h$season
    all.text$writers[y] <- h$writers
    all.text$Jerry[y] <- h$Jerry
    all.text$Larry[y] <- h$Larry
    if(all.text$speaker[y] %in% girls) all.text$jerry_gf[y] <- TRUE
    if(all.text$speaker[y] %in% georges_girls) all.text$george_gf[y] <- TRUE
    if(all.text$speaker[y] %in% elaines_bfs) all.text$elaine_bf[y] <- TRUE
    if(all.text$speaker[y] %in% kramers_girls) all.text$kramer_gf[y] <- TRUE
    all.text$gender[y] <- gender[gender$Name %in% all.text$speaker[y], 3]
}
all.text <- all.text[,c(2,1,15,4,6,11,3,5,12,7:10,13,14)]
#write.csv(all.text, "seinfeld_scripts_enhanced.csv", row.names=F)




