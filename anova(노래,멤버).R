snsd <- read.csv("D:/수업 17-2/실험계획법/덤플/SNSD.csv")
sper <- snsd[,c(13:21)]

#factor1:노래, factor2: 멤버별 퍼센티지
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
data$song <- as.numeric(as.character(data$song))
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


#two-way anova
library(doBy)

aov_model <- aov(percentage ~ song + member + song:member, data = ss.without.jessica)
aov_model <- aov(percentage ~ song + member + song:member, data = ss.with.jessica)
summary(aov_model)

TukeyHSD(aov_model)
TukeyHSD(lm)





#밑에는 무시 

#정규성 검정
# shapiro.test(ss.with.jessica$percentage)

#등분산성
# levene.test(ss.with.jessica$percentage)

#kruskal
# kruskal.test(percentage ~ song, data = ss.with.jessica)
# 
# #friedman
# friedman.test(sper, groups = )
# 
# #friedman
# friedman.test(ss.without.jessica$percentage, ss.without.jessica$album, ss.without.jessica$member)
# friedman.test(percentage,album,member,percentage~member,data=ss.with.jessica)
# friedman.test(ss.without.jessica, y= percentage, groups = album, blocks = member)
# friedman.test(ss.with.jessica)
# 
# # box plot and interaction plot
# par(mfrow = c(9, 66))
# plot(percentage ~ song, main="box plot by song", data = data)
# plot(percentage ~ member, main="box plot by member", data = data)
# interaction.plot(song, member, percentage, bty='l', main="interaction effect plot", data = data)
# interaction.plot(member, song, percentage, bty='l', main="interaction effect plot", data = data)
# 
