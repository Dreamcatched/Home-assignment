#####Getting started#####
rm(list=ls(all=TRUE))
graphics.off()

#####Loading packages#####
require(lsr)
require(car)
require(gsheet)
require(ggplot2)
require(psych)
require(dplyr)
require(lm.beta)

#####Backup#####
data.backup = home_sample_1

#####View data#####
View(data.backup)

#####Check data#####
describe(data.backup$pain)
hist(data.backup$pain, breaks = 20)

describe(data.backup$age)
hist(data.backup$age, breaks = 20)

describe(data.backup$STAI_trait)
hist(data.backup$STAI_trait, breaks = 20)

describe(data.backup$pain_cat)
hist(data.backup$pain_cat, breaks = 20)

describe(data.backup$cortisol_serum)
hist(data.backup$cortisol_serum, breaks = 20)

describe(data.backup$cortisol_saliva)
hist(data.backup$cortisol_saliva, breaks = 20)

describe(data.backup$mindfulness)
hist(data.backup$mindfulness, breaks = 20)

#####Checking for outliers#####
my_reg= lm (ID ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness,data=data.backup)

lev<-hat(model.matrix(my_reg))
plot(lev)
data.backup[lev>.15,]

N<-nrow(data.backup)
mahad<-(N-1)*(lev-1/N)
qchisq(.999,df=6
tail(sort(x=mahad),n=5)
order(mahad,decreasing = T)[c(5,4,3,2,1)]

#####Remove outliers#####
data.backup = data.backup[-which(data.backup[, "ID"] == "ID_90"), ]
data.backup = data.backup[-which(data.backup[, "ID"] == "ID_28"), ]
data.backup = data.backup[-which(data.backup[, "ID"] == "ID_146"), ]
data.backup = data.backup[-which(data.backup[, "ID"] == "ID_112"), ]

#####Descriptive statistics#####
summary(data.backup$sex)
describe(data.backup)

#####Prediction####
plot(pain ~ sex, data = data.backup)
abline(lm(pain ~ sex, data = data.backup))
plot(pain ~ age, data = data.backup)
abline(lm(pain ~ age, data = data.backup))

#####Regression#####
mod1 <- lm(pain ~ age + sex, data = data.backup)
mod1
summary(mod1)

ggplot(data.backup, aes(x = age, y = pain)) + geom_point() + 
geom_smooth(method = "lm", formula = y ~ x)

#####Assumptions#####
hist(residuals(mod1), breaks = 20) #Histogram
plot(mod1, which = 2) #QQPlot
describe(residuals(mod1)) #Kurtosis and skew
shapiro.test(residuals(mod1)) #Shapiro

#####Assumption of linearity#####
fitted<- fitted.values(object = mod1)
plot(x=fitted, y=data.backup$sex, 
     xlab="Fitted Values", ylab = "Observed Values")
fitted<- fitted.values(object = mod1)
plot(x=fitted, y=data.backup$age, 
     xlab="Fitted Values", ylab = "Observed Values")
plot(mod1, which = 1)
residualPlots(mod1) #Residuals and Tukey

#####Excess multicollinearity#####
vif(mod1)

#####Homoscedasticity#####
plot(mod1, which = 3)
ncvTest(mod1)

######Outliers#####
plot(pain ~ age, data = data.backup)
abline(mod1)
plot(pain ~ sex, data = data.backup)
abline(mod1)
plot(mod1, which = 4)

#####Assessment#####
AIC(mod1)
confint(mod1)
sm1 = summary(mod1) 
sm1
lm.beta(mod1)

#####Build a second model#####
plot(pain ~ STAI_trait, data = data.backup) 
abline(lm(pain ~ STAI_trait, data = data.backup))
plot(pain ~ pain_cat, data = data.backup) 
abline(lm(pain ~ pain_cat, data = data.backup))
plot(pain ~ mindfulness, data = data.backup) 
abline(lm(pain ~ mindfulness, data = data.backup))
plot(pain ~ cortisol_serum, data = data.backup)
abline(lm(pain ~ cortisol_serum, data = data.backup))
plot(pain ~ cortisol_saliva, data = data.backup)
abline(lm(pain ~ cortisol_saliva, data = data.backup))

mod2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data.backup)
mod2
summary(mod2)

#####Checking assumptions#####
hist(residuals(mod2), breaks = 20) #Historgram
plot(mod2, which = 2) #QQPlot
describe(residuals(mod2)) #Skew and Kurtosis
shapiro.test(residuals(mod2)) #Shapiro

#####Assumption of linearity#####
fitted2<- fitted.values(object = mod2)
plot(x=fitted2, y=data.backup$STAI_trait, 
     xlab="Fitted Values", ylab = "Observed Values")
plot(x=fitted2, y=data.backup$pain_cat, 
     xlab="Fitted Values", ylab = "Observed Values")
plot(x=fitted2, y=data.backup$cortisol_serum, 
     xlab="Fitted Values", ylab = "Observed Values")
plot(x=fitted2, y=data.backup$mindfulness, 
     xlab="Fitted Values", ylab = "Observed Values")
plot(mod2, which = 1)

residualPlots(mod2) #Residuals and Tukey
vif(mod2)

#####Homoscedasticity#####
plot(mod2, which = 3)
ncvTest(mod2)

#####Check for outliers#####
plot(pain ~ STAI_trait, data = data.backup)
abline(mod2)
plot(pain ~ pain_cat, data = data.backup)
abline(mod2)
plot(pain ~ cortisol_serum, data = data.backup)
abline(mod2)
plot(pain ~ mindfulness, data = data.backup)
abline(mod2)
plot(mod2, which = 4)

#####Assessment#####
AIC(mod2)
confint(mod2)
lm.beta(mod2)
sm2 = summary(mod2) 
sm2

#####Compare the two models#####
sm1
sm2
AIC(mod1)
AIC(mod2)
confint(mod1) 
lm.beta(mod1)
confint(mod2) 
lm.beta(mod2)
anova(mod1,mod2)