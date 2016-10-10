library(ggplot2)
#setwd("/Users/kaylinwalker/R/textmining_seinfeld")
setwd("/Users/kwalker/git_projects/textmining_seinfeld")

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
#all.text$group <- "-"
#ggplot(all.text, aes(group, word.count)) + geom_bar(stat="identity", aes(fill=Larry)) + 
#    coord_flip() + facet_grid(season ~ .) + scale_fill_brewer(palette="Paired")

# does Jerry talk more in episodes he wrote? NO
j <- aggregate(word.count ~ num + speaker, all.text[all.text$speaker %in% "JERRY", ], sum)
all <- aggregate(word.count ~ num, all.text, sum)    
j <- merge(j, all, by="num", all.x=TRUE)
j$Share <- j$word.count.x/j$word.count.y
larry <- unique(all.text[ , c(4,6,7,8,9,14,15)])
j <- merge(j, larry[,c(1,6)], by="num", all.x=TRUE)
t.test(Share ~ Jerry, j) # not really significant
ggplot(j, aes(Jerry, Share*100, group=Jerry)) + geom_boxplot() + labs(title="Does Jerry Talk More in Episodes He Wrote?") +
    xlab("Episode Written by Jerry Seinfeld") + ylab("Share of Words Spoken (%)")

# does George talk more in episodes Larry David wrote? YES
g <- aggregate(word.count ~ num + speaker, all.text[all.text$speaker %in% "GEORGE", ], sum)
g <- merge(g, all, by="num", all.x=TRUE)
g$Share <- g$word.count.x/g$word.count.y
g <- merge(g, larry[,c(1,7)], by="num", all.x=TRUE)
t.test(Share ~ Larry, g) # significant
ggplot(g, aes(Larry, Share*100, group=Larry)) + geom_boxplot() + labs(title="Does George Talk More in Episodes Larry David Wrote?") +
    xlab("Episode Written by Larry David") + ylab("Share of Words Spoken (%)")



speaker <- read.csv("data/seinfeld_tdm_speaker.csv", stringsAsFactors=F)
ep <- read.csv("data/seinfeld_tdm_episode.csv", stringsAsFactors=F)

