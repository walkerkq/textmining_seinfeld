library(ggplot2)
library(reshape2)
setwd("/Users/kaylinwalker/R/textmining_seinfeld")
#setwd("/Users/kwalker/git_projects/textmining_seinfeld")

all.text <- read.csv("data/seinfeld_scripts_enhanced.csv", stringsAsFactors=F)
all.text$date <- as.Date(all.text$date)

############################################################
########## share of words overall
shareOverall <- aggregate(word.count ~ speaker, all.text, sum)
shareOverall$Overall.Share <- sapply(shareOverall$word.count, function(x) x/sum(shareOverall$word.count))
other <- data.frame(speaker="OTHER", 
                    word.count=sum(shareOverall$word.count[!(shareOverall$speaker %in% c("JERRY", "ELAINE", "GEORGE", "KRAMER"))]),
                    Overall.Share=sum(shareOverall$Overall.Share[!(shareOverall$speaker %in% c("JERRY", "ELAINE", "GEORGE", "KRAMER"))]))
shareOverall <- rbind(shareOverall[shareOverall$speaker %in% c("JERRY", "ELAINE", "GEORGE", "KRAMER"),], other)
#write.csv(shareOverall, "seinfeld_plot_shareoveral.csv", row.names=F)

############################################################
##########  share of words spoken by season
shareBySeason <- aggregate(word.count ~ speaker + season, all.text, sum)
for(j in seq_along(shareBySeason[,1])){
    season.total <- sum(shareBySeason$word.count[shareBySeason$season %in% shareBySeason$season[j]])
    shareBySeason$Season.Share[j] <- shareBySeason$word.count[j]/season.total
}
other <- aggregate(Season.Share ~ season, shareBySeason[!(shareBySeason$speaker %in% c("JERRY", "ELAINE", "GEORGE", "KRAMER")),  ], sum)
other$speaker <- "OTHER"
other$word.count <- aggregate(word.count ~ season, shareBySeason[!(shareBySeason$speaker %in% c("JERRY", "ELAINE", "GEORGE", "KRAMER")),  ], sum)[,2]
shareBySeason <- rbind(shareBySeason[shareBySeason$speaker %in% c("JERRY", "ELAINE", "GEORGE", "KRAMER"),], other[,c(3,1,4,2)])
#write.csv(shareBySeason, "seinfeld_plot_sharebyseason.csv", row.names=F)

############################################################
##########  gender disparity
genderByEp <- aggregate(word.count ~ gender + episode + num, all.text[all.text$gender %in% c("F", "M"), ], sum)
genderByEp$Share <- 0
for(j in seq_along(genderByEp$gender)){
     genderByEp$Share[j] <- genderByEp$word.count[j] / sum(genderByEp$word.count[genderByEp$episode %in% genderByEp$episode[j]])
}
genderExp <- dcast(genderByEp, num ~ gender)
# write.csv(genderExp, "seinfeld_plot_sharebyep.csv", row.names=F)


############################################################
############################################################
#### Now onto word content instead of count
LL <- read.csv("data/seinfeld_LL_episodes.csv", stringsAsFactors=F)
LL$name2 <- gsub("\\.", " ", LL$name)
LL$num <- sapply(LL$name2, function(x) as.numeric(gsub("X", "", strsplit(x, " ")[[1]][1])))

for(j in seq_along(LL$o1)) { 
     LL$match[j] <- grepl(LL$word[j], tolower(LL$name2[j])) 
}
# best word per episode
perEp <- NULL
for(j in unique(LL$num)){
     sub <- LL[LL$num %in% j, ]
     sub <- sub[order(-sub$chisq), ]
     row <- sub[1,]
     perEp <- rbind(perEp, row)
}
#write.csv(perEp, "seinfeld_top_perEp.csv", row.names=F)

