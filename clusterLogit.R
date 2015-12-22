require(stargazer)

d <- as.data.frame(read.csv("../DataFiles/clustersWithInactive.csv", stringsAsFactors=F))
d$isCentral <- apply(d, 1, function(x) sum(x[5:76]=='1',na.rm=TRUE)>=2)
dSmall <- d[,c(2,3,4,77)]
xtabs(~isCentral+T1, data=dSmall)
xtabs(~isCentral+T2, data=dSmall)
xtabs(~isCentral+T3, data=dSmall)
dSmall[dSmall == '0'] <- 'LowActivity'
#dSmall[is.na(dSmall)] <- 'LowActivity'
dSmall[dSmall == '1'] <- 'CentralMember'
dSmall[dSmall == '2'] <- 'PeripheralExpert'
dSmall[dSmall == '3'] <- 'Newbie'
dSmall[dSmall == 'IA'] <- 'Inactive'
dSmall[dSmall == ''] <- 'Inactive'

cols <- c('T1','T2','T3')
dSmall[,cols] <- apply(dSmall[,cols], 2, function(x) factor(x, ordered=FALSE, levels=c("LowActivity","CentralMember","Newbie","PeripheralExpert", "Inactive")))
mylogit <- glm(isCentral ~ T1 + T2 + T3, data=dSmall, family="binomial")
print(summary(mylogit))
stargazer(mylogit, out="../Results/clusterIALogit.tex")
