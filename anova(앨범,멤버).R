snsd <- read.csv("D:/?ˆ˜?—… 17-2/?‹¤?—˜ê³„íšë²?/?¤?”Œ/SNSD.csv")
sper <- snsd[,c(13:21)]


#factor1:?•¨ë²?, factor2: ë©¤ë²„ë³? ?¼?„¼?‹°ì§€
albumss <- snsd[,1]
percentage <- c()
album <- c()
member <- c()
for(i in 1:nrow(sper)){
  percentage <- c(percentage, sper[i,])
  album <- c(album, rep(as.character(albumss[i]),ncol(sper)))
  member <- c(member, colnames(sper))
}

#?•©ì³?
data <- as.data.frame(cbind(percentage, album, member))
data$percentage <- as.numeric(as.character(data$percentage))
data$album <- as.character(data$album)
data$member <- unlist(data$member)


#?–‰?´ë¦?
row.names(data) <- 1:nrow(data)

#? œ?‹œì¹? ë¹ ì?€ê¸? ? „/?›„
ss.without.jessica.idx <- c((which(is.na(data))[1]-8):nrow(data))

ss.without.jessica <- data[ss.without.jessica.idx,]
ss.without.jessica <- na.omit(ss.without.jessica)
ss.with.jessica <- data[-ss.without.jessica.idx,]

#two-way anova
library(doBy)

aov_model <- aov(percentage ~ album + member + album:member, data = ss.without.jessica)
aov_model <- aov(percentage ~ album + member + album:member, data = ss.with.jessica)
summary(aov_model)

TukeyHSD(aov_model)

#nonparametric

friedman.test(as.matrix(ss.with.jessica))
# 
# kk <- friedman.test(percentage ~ album + member, data = ss.without.jessica)
# 
# friedman.test(ss.without.jessica$percentage, ss.without.jessica$album, ss.without.jessica$member)
# friedman.test(percentage,album,member,percentage~member,data=ss.with.jessica)
# friedman.test(ss.without.jessica, groups = album, blocks = member)
