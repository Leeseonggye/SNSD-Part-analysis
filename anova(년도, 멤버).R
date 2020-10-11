snsd <- read.csv("D:/수업 17-2/실험계획법/덤플/SNSD.csv")

datess <- as.character(snsd$date)
datesss <- substr(datess,1,4)


#factor1:년ㄷ, factor2: 멤버별 퍼센티지
percentage <- c()
year <- c()
member <- c()
for(i in 1:nrow(sper)){
  percentage <- c(percentage, sper[i,])
  year <- c(year, rep(as.character(datesss[i]),ncol(sper)))
  member <- c(member, colnames(sper))
}

#합쳐
data <- as.data.frame(cbind(percentage, year, member))
data$percentage <- as.numeric(as.character(data$percentage))
data$year <- as.character(data$year)
data$member <- unlist(data$member)


#행이름
row.names(data) <- 1:nrow(data)

#제시카 빠지기 전/후
ss.without.jessica.idx <- c((which(is.na(data))[1]-8):nrow(data))

ss.without.jessica <- data[ss.without.jessica.idx,]
ss.without.jessica <- na.omit(ss.without.jessica)
ss.with.jessica <- data[-ss.without.jessica.idx,]

#two-way anova
library(doBy)

aov_model <- aov(percentage ~ year + member + year:member, data = ss.without.jessica)
aov_model <- aov(percentage ~ year + member + year:member, data = ss.with.jessica)
summary(aov_model)

TukeyHSD(aov_model)

#nonparametric

friedman.test(as.matrix(ss.with.jessica))


#밑에는 무시 
# kk <- friedman.test(percentage ~ year + member, data = ss.without.jessica)
# 
# friedman.test(ss.without.jessica$percentage, ss.without.jessica$year, ss.without.jessica$member)
# friedman.test(percentage,year,member,percentage~member,data=ss.with.jessica)
# friedman.test(ss.without.jessica, groups = year, blocks = member)
