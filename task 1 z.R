##########Clear workspace##########
#rm(list=ls(all=TRUE))#

##########Set working directory##########
setwd("C:\\Users\\Celin\\Documents\\MSc Psychology (Lund)\\PSYP13\\SubCourse1\\Home Assignment")

##########Require packages##########
require(psych)
require(dplyr)
require(gsheet)
require(ggplot2)

##########Check dataset##########
View(home_sample_1)
describe(home_sample_1)
plot(age~pain, data = home_sample_1)
complete.cases(home_sample_1)

##########Clean up data##########
(home_cleaned <- home_sample_1[c(-28,-86, -44),])
plot(pain ~ age + sex, data = home_cleaned)

##########Regression analysis##########
mod1 = lm(pain~age+sex, data = home_cleaned)
plot(pain~age+sex, data = home_cleaned)
abline(mod1)
mod1

##########Predict##########
fitted(mod1)
predict(mod1)

##########Plot predicted values##########
age = c(20, 40, 60, 80, 100)
sex = c(female, male)
newdata_to_predict <- as.data.frame(cbind(age, sex))
predicted_pain <- predict(mod1, newdata = newdata_to_predict)
cbind(newdata_to_predict, predicted_pain)

plot(age~pain, data = home_cleaned)
abline(mod1)
points(age_df_with_predicted, col = "red", pch = 19, cex = 2)

##########RSS##########
RSS = sum((home_cleaned$pain - predict(mod1))^2)
RSS

##########Residual error#########
plot(mod_mean, col = "red", x_var = "height")

##########R2##########
R2 = 1-(RSS/TSS)
R2
plot(home_cleaned$age, home_cleaned$pain)
plot(sample(1:nrow(home_cleaned)), home_cleaned$age)

##########ANOVA##########
anova(mod_mean, mod1)

##########Model information##########
summary(mod1)
confint(mod1)
AIC(mod1)

##########Plot confidence interval##########
ggplot(home_cleaned, aes(pain,age,sex))
  geom_point()
  geom_smooth(method='lm',formula=y~x)
  
  
################################################################################
  
##########Create another model##########
mod2 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_saliva + cortisol_serum, data = home_cleaned)
mod2

##########Summary##########
summary(mod2)
AIC(mod2)
confint(mod2)
lm.beta(mod2)

##########Compare adj.rsquared#########
summary(mod1)$adj.r.squared
summary(mod2)$adj.r.squared

#########Compare residual error########
anova(mod1, mod2)
AIC(mod1)
AIC(mod2)

##########Beta values########
lm.beta(mod2)