### LL df generation
setwd("/Users/kaylinwalker/R/textmining_seinfeld")
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
for(u in 151:174){
     LL.temp <- LL(ep[,c(1,3:173)], column.number=u, threshold=6.6)
     LL.df.ep <- rbind(LL.df.ep, LL.temp)
}
#write.csv(LL.df.ep, "data/seinfeld_LL_episodes.csv", row.names=F)