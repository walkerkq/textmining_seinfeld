library(ggplot2)
library(RColorBrewer)

setwd("/Users/kwalker/git_projects/textmining_seinfeld/data/tidy data")
count <- read.csv("seinfeld_tdm_ALL.csv", stringsAsFactors=FALSE)
count$ngramsize <- 0
for(u in seq_along(count[,1])){
    word <- count$word[u]
    words <- strsplit(word, " ")
    count$ngramsize[u] <- length(words[[1]])
}

singles <- count[count$ngramsize==1,]
speakertotals <- data.frame(colSums(singles[,1:11]))
speakertotals$speaker <- row.names(speakertotals)
colnames(speakertotals) <- c("total", "speaker")
speakertotals <- speakertotals[order(-speakertotals$total), ]
speakertotals$share <- round(speakertotals$total/sum(speakertotals$total),2)
speakertotals$speaker <- factor(speakertotals$speaker, levels=speakertotals$speaker)

# make the plot
ggplot(speakertotals, aes(speaker,share)) + 
    geom_bar(stat="identity", aes(fill=speaker)) +
    theme_classic() + labs(title="1 in 4 words come from Jerry")  +
    ylab("Share of Total Words Spoken (%)") + 
    scale_fill_brewer(palette="BrBG") + xlab("") +
    theme(legend.position=1,plot.title = element_text(size=18), 
          axis.title.y=element_text(margin=margin(0,10,0,0)),
          axis.title=element_text(size=16)) +
    geom_text(aes(x=speaker, y=share+.01, 
                  label=paste(share,"%", sep="")), 
              color="black", fontface="bold", size=4)
