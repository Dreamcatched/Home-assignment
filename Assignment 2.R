#####Load packages#####
library(psych) 
library(dplyr) 
library(gsheet) 
library(ggplot2) 
library(car)
library(lm.beta)

#####Create backup#####
data1=home_sample_1

#####First look at data#####
View(data1)

describe(data1$mindfulness)
hist(data1$mindfulness, breaks = 20)

describe(data1$weight)
hist(data1$weight, breaks = 20)

describe(data1$age)
hist(data1$age, breaks = 20)

describe(data1$pain)
hist(data1$pain, breaks = 20)

describe(data1$STAI_trait)
hist(data1$STAI_trait, breaks = 20)

describe(data1$pain_cat)
hist(data1$pain_cat, breaks = 20)

describe(data1$cortisol_serum)
hist(data1$cortisol_serum, breaks = 20)

describe(data1$cortisol_saliva)
hist(data1$cortisol_saliva, breaks = 20)

#####Identifying and removing multivariate outliers#####
my_reg= lm (ID ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness,data=data1)

lev<-hat(model.matrix(my_reg))
plot(lev)
data1[lev>.15,]

N<-nrow(data1)
mahad<-(N-1)*(lev-1/N)
qchisq(.999,df=6)
tail(sort(x=mahad),n=5)
order(mahad,decreasing = T)[c(5,4,3,2,1)]

data1 = data1[-which(data1[, "ID"] == "ID_112"), ]
data1 = data1[-which(data1[, "ID"] == "ID_146"), ]
data1 = data1[-which(data1[, "ID"] == "ID_28"), ]
data1 = data1[-which(data1[, "ID"] == "ID_90"), ]

describe(data1)
summary(data1)

#####Backwards regression#####
plot(pain ~ weight, data = data1) 
abline(lm(pain ~ weight, data = data1)) #Other plots shown in assignment 1.

#####Create new model#####
mod2 <- lm(pain ~ age + sex + weight + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data1)
mod2
summary(mod2)

#####Check assumptions#####

#Assumptions of normality
describe(residuals(mod2)) 
plot(mod2, which = 2)
shapiro.test(residuals(mod2))
hist(residuals(mod2), breaks = 20)

#Assumptions of linearity
fitted2<- fitted.values(object = mod2)
plot(x=fitted2, y=data1$weight, xlab="Fitted Values", ylab = "Observed Values")
plot(mod2, which = 1)
residualPlots(mod2) 

#Assumption of multicollinearity
vif(mod2)

#Assumption of homoscedasticity
plot(mod2, which = 3)
ncvTest(mod2)

#####Cooks distance#####
plot(pain ~ weight, data = data1)
abline(mod2)
plot(mod2, which = 4)

#####Running the regression####
mod_back_2 = step(mod2, direction = "backward")

#####Create new model#####
mod3bw <- lm(pain ~ age + sex + pain_cat + mindfulness + cortisol_serum, data = data1)
mod3bw

#####Compare models#####
mod3tb <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data1)
mod3tb
anova(mod2,mod3bw)
AIC(mod2)
AIC(mod3bw)
summary(mod3bw)
anova(mod3bw,mod3tb)
summary(mod3bw)$adj.r.squared
summary(mod3tb)$adj.r.squared
AIC(mod3bw)
AIC(mod3tb)
confint(mod3bw) 
lm.beta(mod3bw)

#####Try on other data#####
data2 = home_sample_2
describe(data2)
pred_test <- predict(mod3tb, data2) 
pred_test_back <- predict(mod3bw, data2)
RSS_test = sum((data2[, "pain"] - pred_test)^2) 
RSS_test_back = sum((data2[, "pain"] - pred_test_back)^2)
RSS_test
RSS_test_back