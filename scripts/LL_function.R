
# requires column of interest in tdm df where column 1 is the words and there is so sum or gram column
LL <- function(df, column.number, threshold){
     returned <- NULL
     df <- df[df[,column.number] > 0, ]
     for(w in seq_along(df[,column.number])){
        word <- df[w,1]
        a <- df[w, column.number] # word, corpus one
        b <- sum(df[w, -c(1, column.number)]) # word, corpus two
        c <- sum(df[-w, column.number]) # not word, corpus one
        d <- sum(df[-w, -c(1, column.number)]) # not word, corpus two
        N <- a + b + c +d
        e1 <- (a + c) * ((a + b)/N)
        e2 <- (b + d) * ((a + b)/N)
        chisq <- 2 * ((a * log(a / e1)) + (b * log(b / e2)))
        if(!is.nan(chisq) & chisq > threshold){
             if(a < e1) chisq <- chisq * - 1
             row <- data.frame(o1=a, o2=b, e1, e2, chisq, word=df[w,1], name=colnames(df)[column.number])
             returned <- rbind(returned, row)
        }
     }
     return(returned)
}

