library(doBy)
library(dplyr)
library(lawstat)


snsd <- read.csv("D:/수업 17-2/실험계획법/덤플/SNSD.csv")
sper <- snsd[,c(13:21)]

#
percentage <- c()
song <- c()
member <- c()
for(i in 1:nrow(sper)){
  percentage <- c(percentage, sper[i,])
  song <- c(song, rep(i,ncol(sper)))
  member <- c(member, colnames(sper))
}

#합쳐
data <- as.data.frame(cbind(percentage, song, member))
data$percentage <- as.numeric(as.character(data$percentage))

data$song <- as.character(data$song)
data$member <- unlist(data$member)

#행이름
row.names(data) <- 1:nrow(data)

#제시카 빠지기 전/후
ss.without.jessica.idx <- c((which(is.na(data))[1]-8):nrow(data))

ss.without.jessica <- data[ss.without.jessica.idx,]
ss.without.jessica <- na.omit(ss.without.jessica)
ss.with.jessica <- data[-ss.without.jessica.idx,]

#paired-t
memberr <- unique(ss.with.jessica$member)
ss.with.jessica$song <- as.factor(ss.with.jessica$song)
ss.with.jessica$member <- as.factor(ss.with.jessica$member)

for(i in 1:8){
  for(j in (i+1):9){
    
    a <- ss.with.jessica[which(ss.with.jessica$member == memberr[i]),1]
    b <- ss.with.jessica[which(ss.with.jessica$member == memberr[j]),1]
    
    c <- t.test(a, b, paired=TRUE,  alternative="two.sided", conf.level=0.90)
    cat(memberr[i], "  VS  ", memberr[j],"\n\n")
    print(c)
    
  }
}
