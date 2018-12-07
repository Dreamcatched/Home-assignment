##########Set working directory##########
setwd("C:\\Users\\Celin\\Documents\\MSc Psychology (Lund)\\PSYP13\\SubCourse1\\Home Assignment")

##########Require packages##########
require(lm.beta)
require(car)
require(ggplot2)
require(rgl)
require(psych)
require(MASS)

##########Check dataset##########
View(home_sample_1)
describe(home_sample_1)
plot(age~pain, data = home_sample_1)
plot(pain~weight, data = home_sample_1)
complete.cases(home_sample_1)

##########Clean up data##########
(home_cleaned <- home_sample_1[c(-28,-86, -44),])
plot(age~pain,data = home_cleaned)
plot(sex~pain,data=home_cleaned)

##########Create model##########
mod3 <- lm(pain~age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight, data = home_cleaned)
summary(mod3)
print(mod3)

##########Backwards regression##########
step <- stepAIC(mod3, direction="backward")
step$anova

##########New model##########
backward_model <- lm(pain~age + sex + pain_cat + mindfulness + cortisol_serum, data =home_cleaned)
summary(backward_model)

##########Compare backward and theory-based model##########
AIC(mod2)
anova(mod2)
AIC(backward_model)
anova(backward_model)
anova(mod2)

##########Apply models to new data##########
plot(home_sample_2)

plot(predict(mod2))
plot(predict(backward_model))