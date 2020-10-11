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

lm.out = lm(percentage~song+member+song:member, data = ss.with.jessica)
anova(lm.out)


##anova
ssmodel <- aov(percentage ~ member, data = ss.with.jessica)
summary(ssmodel)
TukeyHSD(ssmodel)
v1 <- TukeyHSD(ssmodel)$member
write.csv(v1, "D:/수업 17-2/실험계획법/덤플/제시카있을때사후분석.csv")


ssmodel <- aov(percentage ~ member, data = ss.without.jessica)
summary(ssmodel)
TukeyHSD(ssmodel)
v2 <- TukeyHSD(ssmodel)$member
write.csv(v2, "D:/수업 17-2/실험계획법/덤플/제시카없을때사후분석.csv")

#정규성 검정
memberr <- unique(data$member)

#제시카 있을때 
for(i in memberr){
  name <- i
  a <- ss.with.jessica %>% filter(member == i)
  print(name)
  print(shapiro.test(a$percentage))
}
#효연만 정규성 안따름

#제시카 없을때
for(i in memberr[-length(memberr)]){
  name <- i
  a <- ss.without.jessica %>% filter(member == i)
  print(name)
  print(shapiro.test(a$percentage))
}

#등분산성 검정 with mean
levene.test(ss.with.jessica$percentage, ss.with.jessica$member, location = "mean")
levene.test(ss.without.jessica$percentage, ss.without.jessica$member, location = "mean")

#등분산성 검정 with median
levene.test(ss.with.jessica$percentage, ss.with.jessica$member, location = "median")
levene.test(ss.without.jessica$percentage, ss.without.jessica$member, location = "median")

#비모수검정
kruskal.test(percentage~member,as.matrix.data.frame(ss.with.jessica))
kruskal.test(percentage~member,as.matrix.data.frame(ss.without.jessica))