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


# do females speak more in episodes written by women?
female_writers <- c("Jennifer Crittenden", "Carol Leifer", "Elaine Pope", "Marjorie Gross")
for(u in seq_along(all.text$words)){
     if(grepl(paste(paste("Written by", female_writers), collapse="|"), all.text$writers[u])) {
          all.text$Writer.Female[u] <- "Female"
     } else if(grepl(paste(female_writers, collapse="|"), all.text$writers[u])) { 
          all.text$Writer.Female[u] <- "Mixed"
     } else { all.text$Writer.Female[u] <- "Male" }
}
f_ep <- merge(genderByEp, unique(all.text[,c(4,16)]), by="num", all.x=TRUE)
ggplot(f_ep[f_ep$gender %in% "F", ], aes(Writer.Female, Share, group=Writer.Female)) + geom_boxplot()
t.test(Share ~ Writer.Female, f_ep[f_ep$gender %in% "F",]) # slightly 0.07


############################################################
############################################################
#### Now onto word content instead of count
source("scripts/LL_function.R")
speaker <- read.csv("data/seinfeld_tdm_speaker.csv", stringsAsFactors=F)
LL.df <- NULL
for(u in 2:8){
     LL.temp <- LL(speaker[,1:8], column.number=u, threshold=6.6)
     LL.df <- rbind(LL.df, LL.temp)
}
#write.csv(LL.df, "seinfeld_LL_speakers.csv", row.names=F)

ep <- read.csv("data/seinfeld_tdm_episode.csv", stringsAsFactors=F)
LL.df.ep <- NULL
for(u in 151:176){
     LL.temp <- LL(ep[,c(1,3:178)], column.number=u, threshold=6.6)
     LL.df.ep <- rbind(LL.df.ep, LL.temp)
}
#write.csv(LL.df.ep, "seinfeld_LL_episodes.csv", row.names=F)

