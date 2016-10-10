library(ggplot2)
setwd("/Users/kaylinwalker/R/textmining_seinfeld")

episodes <- read.csv("data/seinfeld_episodes.csv", stringsAsFactors=F)
episodes$num <- 1:length(episodes$date)
episodes$Jerry <- sapply(episodes$writers, function(x) if(grepl("Jerry Seinfeld", x)){ x <- 1} else {x <- 0})
episodes$Larry <- sapply(episodes$writers, function(x) if(grepl("Larry David", x)){ x <- 1} else {x <- 0})
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

all.text <- read.csv("data/seinfeld_scripts.csv", stringsAsFactors=F)
all.text$word.count <- sapply(all.text$words, function(x) length(strsplit(x, " ")[[1]]))
for(y in seq_along(all.text$episode)){
     h <- episodes[episodes$title %in% all.text$episode[y], ]
     all.text$season[y] <- h$season
     #all.text$Jerry[y] <- h$Jerry
     #all.text$Larry[y] <- h$Larry
}
all.text$date <- as.Date(all.text$date)
all.text$Overall.Share <- 0
all.text$Episode.Share <- 0
all.text$Season.Share <- 0
for(j in seq_along(all.text$num)){
     all.text$Overall.Share[j] <- all.text$word.count[j]/sum(all.text$word.count)
     all.text$Episode.Share[j] <- round(all.text$word.count[j]/sum(all.text$word.count[all.text$num %in% all.text$num[j]]), 4)
     all.text$Season.Share[j] <- round(all.text$word.count[j]/sum(all.text$word.count[all.text$season %in% all.text$season[j]]), 4)
}

# share of words overall
shareOverall <- aggregate(Overall.Share ~ speaker, all.text, sum)
other <- data.frame(speaker="OTHER", Overall.Share=sum(shareOverall$Overall.Share[!(shareOverall$speaker %in% c("JERRY", "ELAINE", "GEORGE", "KRAMER"))]))
shareOverall <- rbind(shareOverall[shareOverall$speaker %in% c("JERRY", "ELAINE", "GEORGE", "KRAMER"),], other)
shareOverall$group <- ""

shareOverall$speaker <- factor(as.character(shareOverall$speaker), levels=c("JERRY", "OTHER", "GEORGE", "KRAMER", "ELAINE"))
shareOverall <- shareOverall[order(shareOverall$speaker), ]
ggplot(shareOverall, aes(group, Overall.Share*100)) + geom_bar(stat="identity", aes(fill=speaker)) + labs(title="Share of Words Spoken, Overall") +
     xlab("") + ylab("Share of Words Spoken (%)") + theme_classic() + scale_fill_brewer(palette="Paired") + 
     geom_text(aes(x=group, y=cumsum(Overall.Share*100)-Overall.Share*50, label=paste(speaker,": ",round(Overall.Share*100,1),"%", sep="")), 
               color="white", fontface="bold", size=4) +
     theme(legend.position=1, axis.title.y=element_text(margin=margin(0,10,0,0)))
# write.csv(shareOverall, "seinfeld_plot_shareoveral.csv", row.names=F)

# share of words spoken by season
shareBySeason <- aggregate(Season.Share ~ speaker + season, all.text, sum)
other <- aggregate(Season.Share ~ season, all.text[!(all.text$speaker %in% c("JERRY", "ELAINE", "GEORGE", "KRAMER")),], sum)
other$speaker <- "OTHER"
other <- other[,c(3,1,2)]
seasonPlot <- rbind(other, shareBySeason[shareBySeason$speaker %in% c("JERRY", "ELAINE", "GEORGE", "KRAMER"), ])
ggplot(seasonPlot, aes(as.character(season), Season.Share*100)) + 
     geom_bar(stat="identity",  aes(fill=speaker)) + labs(title="Share of Words Spoken by Character by Season") +
     scale_fill_brewer(palette="Paired") +
     xlab("Season") + ylab("Share of Words (%)") + facet_grid(.~speaker) + theme_bw()
# write.csv(seasonPlot, "seinfeld_plot_sharebyseason.csv", row.names=F)

# gender disparity
gender <- read.csv("data/all.speakers.csv", stringsAsFactors=F)
for(i in seq_along(all.text$num)){
     all.text$gender[i] <- gender[gender$Name %in% all.text$speaker[i], 3]
}
genderByEp <- aggregate(word.count ~ gender + episode + num, all.text[all.text$gender %in% c("F", "M"), ], sum)
genderByEp$Share <- 0
for(j in seq_along(genderByEp$gender)){
     genderByEp$Share[j] <- genderByEp$word.count[j] / sum(genderByEp$word.count[genderByEp$episode %in% genderByEp$episode[j]])
}
ggplot(genderByEp, aes(num, Share*100)) + geom_bar(stat="identity", aes(fill=gender)) +
     xlab("Episode #") + ylab("") + labs(title="Share of Words Spoken by Female Characters") +
     theme_bw() + scale_fill_brewer(palette="Paired") + theme(legend.position=1) + coord_flip()
# write.csv(genderByEp, "seinfeld_plot_sharebyep.csv", row.names=F)

all.text$group <- "-"
ggplot(all.text[all.text$gender %in% c("F", "M"), ], aes(group, word.count)) + 
     geom_bar(stat="identity", aes(fill=gender)) + coord_flip() + facet_grid(season ~ .)



speaker <- read.csv("seinfeld_tdm_speaker.csv", stringsAsFactors=F)
ep <- read.csv("seinfeld_tdm_episode.csv", stringsAsFactors=F)

