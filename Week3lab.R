# Week 3 lab

# load packages

library(statsr)
library(dplyr)
library(ggplot2)
library(GGally)

# load data

data(evals)

# Q1
# Observational study

# Q2
# Revise to ask if there is an association

# Q3

summary(evals$score)
hist(evals$score)
below3<-filter(evals, score<3)
nrow(below3)

# Exercise stuff

ggplot(data = evals, aes(x = bty_avg, y = score)) +
  geom_point()

ggplot(data = evals, aes(x = bty_avg, y = score)) +
  geom_jitter()

ggplot(data = evals, aes(x = bty_avg, y = score)) +
  geom_jitter()+geom_smooth(method = "lm")

# Q4

m1<-lm(score~bty_avg, data=evals)

summary(m1)


# Exercise stuff

ggplot(data = evals, aes(x = bty_f1lower, y = bty_avg)) +
  geom_jitter()
evals %>% 
  summarise(cor(bty_avg, bty_f1lower))

ggpairs(evals, columns = 13:19)

# Q6 & 7

m2<-lm(score~bty_avg+gender, data=evals)
summary(m2)
plot(m2)

# Q8

m3<-lm(score~bty_avg+rank, data=evals)
summary(m3)
plot(m3)


# Q9

m4<-lm(score ~ rank + ethnicity + gender + language + age + cls_perc_eval 
       + cls_students + cls_level + cls_profs + cls_credits + bty_avg 
       + pic_outfit + pic_color, data = evals)

summary(m4)
plot(m4)

m5<-lm(score~ethnicity + gender + language + age + cls_perc_eval 
       + cls_students + cls_level + cls_profs + cls_credits + bty_avg 
       + pic_outfit + pic_color, data = evals)

m6<-lm(score~rank + gender + language + age + cls_perc_eval 
       + cls_students + cls_level + cls_profs + cls_credits + bty_avg 
       + pic_outfit + pic_color, data = evals)

m7<-lm(score~rank + gender + age + cls_perc_eval 
       + cls_students + cls_level + cls_profs + cls_credits + bty_avg 
       + pic_outfit + pic_color, data = evals)

m8<-lm(score~rank + gender + language + cls_perc_eval 
       + cls_students + cls_level + cls_profs + cls_credits + bty_avg 
       + pic_outfit + pic_color, data = evals)

m9<-lm(score~rank + gender + language + age + cls_students + cls_level + cls_profs + cls_credits + bty_avg 
       + pic_outfit + pic_color, data = evals)

m10<-lm(score~rank + gender + language + age + cls_perc_eval 
        + cls_level + cls_profs + cls_credits + bty_avg 
        + pic_outfit + pic_color, data = evals)

m11<-lm(score~rank + gender + language + age + cls_perc_eval 
        + cls_students + cls_profs + cls_credits + bty_avg 
        + pic_outfit + pic_color, data = evals)

m12<-lm(score~rank + gender + language + age + cls_perc_eval 
        + cls_students + cls_level +  cls_credits + bty_avg 
        + pic_outfit + pic_color, data = evals)

m13<-lm(score~rank + gender + language + age + cls_perc_eval 
        + cls_students + cls_level + cls_profs +  bty_avg 
        + pic_outfit + pic_color, data = evals)

m14<-lm(score~rank + gender + language + age + cls_perc_eval 
        + cls_students + cls_level + cls_profs + cls_credits + pic_outfit + pic_color, data = evals)

m15<-lm(score~rank + gender + language + age + cls_perc_eval 
        + cls_students + cls_level + cls_profs + cls_credits + bty_avg 
        + pic_color, data = evals)

m16<-lm(score~rank + gender + language + age + cls_perc_eval 
        + cls_students + cls_level + cls_profs + cls_credits + bty_avg 
        + pic_outfit, data = evals)
