# load libraries

library(DAAG)

data(allbacks)

# fit first medel

m1<-lm(weight~volume+cover, data=allbacks)

summary(m1)

# load more data

states<-read.csv("http://bit.ly/dasi_states")

# fit model

m2<-lm(poverty~female_house, data=states)

summary(m2)

anova(m2)

m3<-lm(poverty~female_house+white, data=states)

summary(m3)

anova(m3)

sst<-sum(anova(m3)[,2])

sst


# Inference for MLR

# load data

d1<-read.csv("http://bit.ly/dasi_cognitive")

# full model

m4<-lm(kid_score~., data=d1)

summary(m4)

m5<-lm(kid_score~mom_iq+mom_hs+mom_work, data=d1)

summary(m5)

plot(m5)

plot(m5$residuals~d1$mom_iq)

mean(m5$residuals)

hist(m5$residuals)

qqnorm(m5$residuals)

qqline(m5$residuals)

plot(m5$residuals~m5$fitted)

plot(abs(m5$residuals)~m5$fitted.values)

plot(m5$residuals)
