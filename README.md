# snsd_anova
snsd_analyze_with_anova
snsd <- read.csv("C:/Users/sunggye/Desktop/수업/제시카있을때사후분석.csv")

datess <- as.character(snsd$date)
datesss <- substr(datess,1,4)


#factor1
percentage <- c()
year <- c()
member <- c()
for(i in 1:nrow(sper)){
  percentage <- c(percentage, sper[i,])
  year <- c(year, rep(as.character(datesss[i]),ncol(sper)))
  member <- c(member, colnames(sper))
}

#Data_prepare
data <- as.data.frame(cbind(percentage, year, member))
data$percentage <- as.numeric(as.character(data$percentage))
data$year <- as.character(data$year)
data$member <- unlist(data$member)



row.names(data) <- 1:nrow(data)

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
