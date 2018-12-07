##########Clear workspace##########
#rm(list=ls(all=TRUE))#

##########Set working directory##########
setwd("C:\\Users\\Celin\\Documents\\MSc Psychology (Lund)\\PSYP13\\SubCourse1\\Home Assignment")

##########Require packages##########
require(psych)
require(lsr)
require(MASS)
require(vegan)
require(data.table)
require(ggplot2)

##########First look at the data##########
who(TRUE)
summary(nations)
describe(nations)
plot(nations)

##########Create copy of data#########
nations1=nations

##########Convert to dissimilarities##########
nations1 <- sim2diss(nations, method = 9)
nations1 <- as.dist(as.matrix(nations1))


##########NMDS##########
nations.mds <- isoMDS(nations1)

##########2D##########
x <- nations.mds$points[,1]
y <- nations.mds$points[,2]

##########Plot NMDS##########
plot(x,y, xlab="Coordinate 1", ylab="Coordinate 2", 
     xlim=range(nations.mds$points[,1])*1.2, type="n") 
text(x, y, labels=colnames(nations), cex=0.6)
nations_sh <- Shepard(nations[lower.tri(nations)], nations.mds$points)

##########Shepherd diagram#########
nations.sh=Shepard(nations1, nations.mds$points)
windows()
plot(nations.sh,pch=20,xlab="Dissimilarities",ylab="Distance",
xlim=range(nations.sh$x),
ylim=range(nations.sh$x),
main="Shepard Diagram")
lines(nations.sh$x, nations.sh$yf, type = "S")

##########Format data##########
mds.values <- nations.mds$points
mds.data2 <- data.frame(Sample=rownames(nations.mds), X=nations.mds$points[,1], Y=nations.mds$points[,2])


##########Variation##########
mds.var.per <- round(nations.mds$eig/sum(nations.mds$eig)*100, 1)

##########Graph########## 
ggplot(data=mds.values, aes(x=X, y=Y, label=Sample))
  geom_text()
  theme_bw()
  xlab(paste("MDS1 - ", nations1[1], "%", sep=""))
  ylab (paste("MDS2 - ", nations1[2], "%", sep=""))
  ggtitle("MDS plot") 