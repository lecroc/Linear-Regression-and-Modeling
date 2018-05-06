# Load Packages

library(dplyr)
library(ggplot2)
library(statsr)

# load data

data("mlb11")

# Question 2 plot runs v at_bats

plot(runs~at_bats, data=mlb11)

# relationship looks linear

# correlation coefficient

cor(mlb11$runs, mlb11$at_bats)

mlb11 %>%
  summarise(cor(runs, at_bats))

# Question 4

m1<-lm(runs~homeruns, data=mlb11)

summary(m1)

# Question 5

mlb11 %>%
  filter(at_bats == 5579) %>%
  select(runs)

m2<-lm(runs~at_bats, data=mlb11)

summary(m2)

.6305*5579-2789.2429


# Question 6

ggplot(data = m2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Residuals")


# Question 7

ggplot(data = m1, aes(x = .resid)) +
  geom_histogram(binwidth = 25) +
  xlab("Residuals")


# Question 9

m3<-lm(runs~hits, data=mlb11)

m4<-lm(runs~bat_avg, data=mlb11)

m5<-lm(runs~strikeouts, data=mlb11)

m6<-lm(runs~stolen_bases, data=mlb11)

m7<-lm(runs~wins, data=mlb11)


summary(m1)

summary(m2)

summary(m3)

summary(m4)

summary(m5)

summary(m6)

summary(m7)

# batting avg best predictor - highest R2

# Question 10

m8<-lm(runs~new_onbase, data=mlb11)

m9<-lm(runs~new_slug, data=mlb11)

m10<-lm(runs~new_obs, data=mlb11)

summary(m8)

summary(m9)

summary(m10)
