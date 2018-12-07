##########Clear workspace##########
#rm(list=ls(all=TRUE))#

##########Set working directory##########
setwd("C:\\Users\\Celin\\Documents\\MSc Psychology (Lund)\\PSYP13\\SubCourse1\\Home Assignment")

##########Require packages##########
require(psych)
require(lsr)
require(factoextra)
require(graphics)

##########First look at data##########
who(TRUE)
summary(PAQ_Aabol)
describe(PAQ_Aabol)
plot(PAQ_Aabol)

##########Create copy##########
PAQ_Aabol1=PAQ_Aabol 

##########Remove columns##########
PAQ_Aabol1=PAQ_Aabol1[4:12]
plot(PAQ_Aabol1)

##########First look at new data#########
summary(PAQ_Aabol1)
describe(PAQ_Aabol1)
plot(PAQ_Aabol1)

##########Calculate cor and cov##########
cor(PAQ_Aabol1)
cov(PAQ_Aabol1)

##########Principal Component Analysis##########
PAQ_Aabol1=princomp(PAQ_Aabol1,cor=TRUE)
summary(PAQ_Aabol1, loadings=TRUE)

##########Loadings of principal components##########
loadings(PAQ_Aabol1)

#########Scores of the components#########
PAQ_Aabol1$scores[1:10,]

##########Scree plot#########
screeplot(PAQ_Aabol1, type = "line", main = "Scree plot")

#########Bi plot of score values##########
biplot(PAQ_Aabol1)

##########log(eigenvalue diagram)##########
plot(log(PAQ_Aabol1$sdev^2), xlab = "Component number", ylab = "log(Component variance)", type="l", main = "Log(eigenvalue) diagram")

##########Plot individuals########
fviz_pca_ind(PAQ_Aabol1,
             col.ind = "cos2",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)