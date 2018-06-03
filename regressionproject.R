# Regression project

# libraries

library(ggplot2)
library(dplyr)
library(GGally)

# data
load("eaca_movies.Rdata")

d1<-as.data.frame(movies)

# variables to keep 

keep<-c("title_type", "genre","runtime","mpaa_rating", "thtr_rel_year",
        "thtr_rel_month", "thtr_rel_day", "dvd_rel_year", "dvd_rel_month",
        "dvd_rel_day", "imdb_rating", "imdb_num_votes", "audience_score",
        "critics_score", "best_pic_nom",
        "best_pic_win", "best_actor_win", "best_actress_win", "best_dir_win",
        "top200_box")

kept<-d1 %>%
  select(keep)

dim(kept)

# remove records with nas

sum(is.na(kept))
cc<-complete.cases(kept)
kept<-kept[cc,]
sum(is.na(kept))
dim(kept)

# summary statistics

keptnum<-select_if(kept, is.numeric)

s1<-summary(keptnum$runtime)
s2<-summary(keptnum$thtr_rel_year)
s3<-summary(keptnum$thtr_rel_month)
s4<-summary(keptnum$thtr_rel_day)
s5<-summary(keptnum$dvd_rel_year)
s6<-summary(keptnum$dvd_rel_month)
s7<-summary(keptnum$dvd_rel_day)
s8<-summary(keptnum$imdb_rating)
s9<-summary(keptnum$imdb_num_votes)
s10<-summary(keptnum$audience_score)
s11<-summary(keptnum$critics_score)

Stats<-names(keptnum)

sumtable<-as.data.frame(rbind(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11))

sumtable<-as.data.frame(cbind(names, sumtable))

sumtable<-sumtable %>%
  mutate(Skew=ifelse(Mean>Median, "Right", "Left"))

sumtable

# check for colinearity

p1<-ggpairs(keptnum)+
  theme(legend.position = "none", axis.text.x = element_text(angle = 60,hjust = 1))

p1




# Factor plots

summary(kept$title_type)

summary(kept$genre)

summary(kept$mpaa_rating)

summary(kept$best_pic_nom)

summary(kept$best_actor_win)

summary(kept$best_actress_win)

summary(kept$best_dir_win)

summary(kept$best_pic_win)

summary(kept$top200_box)

p2<-ggplot(kept, aes(x=factor(genre), y=critics_score, fill=factor(genre)))+
  geom_boxplot()+
  theme(legend.position = "none", axis.text.x = element_text(angle = 60,hjust = 1))+
  ggtitle("Critic's Score by Genre")

p2

p3<-ggplot(kept, aes(x=factor(mpaa_rating), y=critics_score, fill=factor(mpaa_rating)))+
  geom_boxplot()+
  theme(legend.position = "none", axis.text.x = element_text(angle = 60,hjust = 1))+
  ggtitle("Critic's Score by Rating")

p3

p4<-ggplot(kept, aes(x=factor(title_type), y=critics_score, fill=factor(title_type)))+
  geom_boxplot()+theme(legend.position = "none", axis.text.x = element_text(angle = 60,hjust = 1))+
  ggtitle("Critic's Score by Title Type")

p4

p6<-ggplot(kept, aes(x=factor(best_actor_win), y=critics_score, fill=factor(best_actor_win)))+
  geom_boxplot()+
  theme(legend.position = "none", axis.text.x = element_text(angle = 60,hjust = 1))+
  ggtitle("Critic's Score by Best Actor Win")

p6

p7<-ggplot(kept, aes(x=factor(best_actress_win), y=critics_score, fill=factor(best_actress_win)))+
  geom_boxplot()+
  theme(legend.position = "none", axis.text.x = element_text(angle = 60,hjust = 1))+
  ggtitle("Critic's Score by Best Actress Win")

p7

p8<-ggplot(kept, aes(x=factor(best_pic_nom), y=critics_score, fill=factor(best_pic_nom)))+
  geom_boxplot()+
  theme(legend.position = "none", axis.text.x = element_text(angle = 60,hjust = 1))+
  ggtitle("Critic's Score by Best Picture Nomination")

p8

p9<-ggplot(kept, aes(x=factor(best_pic_win), y=critics_score, fill=factor(best_pic_win)))+
  geom_boxplot()+
  theme(legend.position = "none", axis.text.x = element_text(angle = 60,hjust = 1))+
  ggtitle("Critic's Score by Best Picture Win")

p9

p10<-ggplot(kept, aes(x=factor(best_dir_win), y=critics_score, fill=factor(best_dir_win)))+
  geom_boxplot()+
  theme(legend.position = "none", axis.text.x = element_text(angle = 60,hjust = 1))+
  ggtitle("Critic's Score by Best Director Win")

p10

# pick between IMDb rating and audience rating

tm1<-lm(critics_score~imdb_rating, data=kept)

summary(tm1)

tm2<-lm(critics_score~audience_score, data = kept)

summary(tm2)

# IMDb stronger predictor, drop audience score

kept$audience_score<-NULL

# Number of votes no linear relationship with critics_score so drop

qqplot(keptnum$imdb_num_votes, keptnum$critics_score)

kept$imdb_num_votes<-NULL

# create df of independent variables

indvar<-kept[,-12]

# store variable names in vector

varlist <- names(indvar)

# function to run lms

models <- lapply(varlist, function(x) {
  lm(substitute(critics_score ~ i, list(i = as.name(x))), data = kept)
})

# grab adjusted r squared and output ranked in descending order

adjr2<-NULL
options(scipen=999)

for (i in 1:length(models)){
  adjr2[[i]]<-summary(models[[i]])$adj.r.squared
}

results1<-as.data.frame(cbind(varlist, adjr2))

results1<-results1 %>%
  arrange(desc(adjr2))
  
  results1

# restart process round 2

# store variable names in vector

varlist2 <- varlist[-11]

# function to run lms

models2 <- lapply(varlist2, function(x) {
  lm(substitute(critics_score ~ imdb_rating+i, list(i = as.name(x))), data = kept)
})

# grab adjusted r squared and output ranked in descending order

adjr2<-NULL
options(scipen=999)

for (i in 1:length(models2)){
  adjr2[[i]]<-summary(models2[[i]])$adj.r.squared
}

results2<-as.data.frame(cbind(varlist2, adjr2))

results2 %>%
  arrange(desc(adjr2))

# restart process round 3

# store variable names in vector

varlist3 <- varlist2[-4]

# function to run lms

models3 <- lapply(varlist3, function(x) {
  lm(substitute(critics_score ~ imdb_rating+mpaa_rating+i, list(i = as.name(x))), data = kept)
})

# grab adjusted r squared and output ranked in descending order

adjr2<-NULL
options(scipen=999)

for (i in 1:length(models3)){
  adjr2[[i]]<-summary(models3[[i]])$adj.r.squared
}

results3<-as.data.frame(cbind(varlist3, adjr2))

results3 %>%
  arrange(desc(adjr2))

# restart process round 4

# store variable names in vector

varlist4 <- varlist3[-1]

# function to run lms

models4 <- lapply(varlist4, function(x) {
  lm(substitute(critics_score ~ imdb_rating+mpaa_rating+title_type+i, list(i = as.name(x))), data = kept)
})

# grab adjusted r squared and output ranked in descending order

adjr2<-NULL
options(scipen=999)

for (i in 1:length(models4)){
  adjr2[[i]]<-summary(models4[[i]])$adj.r.squared
}

results4<-as.data.frame(cbind(varlist4, adjr2))

results4 %>%
  arrange(desc(adjr2))

# restart process round 5

# store variable names in vector

varlist5 <- varlist4[-1]

# function to run lms

models5 <- lapply(varlist5, function(x) {
  lm(substitute(critics_score ~ imdb_rating+mpaa_rating+title_type+genre+
                  i, list(i = as.name(x))), data = kept)
})

# grab adjusted r squared and output ranked in descending order

adjr2<-NULL
options(scipen=999)

for (i in 1:length(models5)){
  adjr2[[i]]<-summary(models5[[i]])$adj.r.squared
}

results5<-as.data.frame(cbind(varlist5, adjr2))

results5 %>%
  arrange(desc(adjr2))

# restart process round 6

# store variable names in vector

varlist6 <- varlist5[-2]

# function to run lms

models6 <- lapply(varlist6, function(x) {
  lm(substitute(critics_score ~ imdb_rating+mpaa_rating+title_type+
                  genre+thtr_rel_year+i, list(i = as.name(x))), data = kept)
})

# grab adjusted r squared and output ranked in descending order

adjr2<-NULL
options(scipen=999)

for (i in 1:length(models6)){
  adjr2[[i]]<-summary(models6[[i]])$adj.r.squared
}

results6<-as.data.frame(cbind(varlist6, adjr2))

results6 %>%
  arrange(desc(adjr2))

# restart process round 7

# store variable names in vector

varlist7 <- varlist6[-4]

# function to run lms

models7 <- lapply(varlist7, function(x) {
  lm(substitute(critics_score ~ imdb_rating+mpaa_rating+title_type+
                  genre+thtr_rel_year+dvd_rel_year+
                  i, list(i = as.name(x))), data = kept)
})

# grab adjusted r squared and output ranked in descending order

adjr2<-NULL
options(scipen=999)

for (i in 1:length(models7)){
  adjr2[[i]]<-summary(models7[[i]])$adj.r.squared
}

results7<-as.data.frame(cbind(varlist7, adjr2))

results7 %>%
  arrange(desc(adjr2))

# restart process round 8

# store variable names in vector

varlist8 <- varlist7[-5]

# function to run lms

models8 <- lapply(varlist8, function(x) {
  lm(substitute(critics_score ~ imdb_rating+mpaa_rating+title_type+
                  genre+thtr_rel_year+dvd_rel_year+dvd_rel_day+
                  i, list(i = as.name(x))), data = kept)
})

# grab adjusted r squared and output ranked in descending order

adjr2<-NULL
options(scipen=999)

for (i in 1:length(models8)){
  adjr2[[i]]<-summary(models8[[i]])$adj.r.squared
}

results8<-as.data.frame(cbind(varlist8, adjr2))

results8 %>%
  arrange(desc(adjr2))

# restart process round 8

# store variable names in vector

varlist8 <- varlist7[-5]

# function to run lms

models8 <- lapply(varlist8, function(x) {
  lm(substitute(critics_score ~ imdb_rating+mpaa_rating+title_type+
                  genre+thtr_rel_year+dvd_rel_year+dvd_rel_day+
                  i, list(i = as.name(x))), data = kept)
})

# grab adjusted r squared and output ranked in descending order

adjr2<-NULL
options(scipen=999)

for (i in 1:length(models8)){
  adjr2[[i]]<-summary(models8[[i]])$adj.r.squared
}

results8<-as.data.frame(cbind(varlist8, adjr2))

results8 %>%
  arrange(desc(adjr2))


# restart process round 9

# store variable names in vector

varlist9 <- varlist8[-9]

# function to run lms

models9 <- lapply(varlist9, function(x) {
  lm(substitute(critics_score ~ imdb_rating+mpaa_rating+title_type+
                  genre+thtr_rel_year+dvd_rel_year+dvd_rel_day+
                  best_dir_win+i, list(i = as.name(x))), data = kept)
})

# grab adjusted r squared and output ranked in descending order

adjr2<-NULL
options(scipen=999)

for (i in 1:length(models9)){
  adjr2[[i]]<-summary(models9[[i]])$adj.r.squared
}

results9<-as.data.frame(cbind(varlist9, adjr2))

results9 %>%
  arrange(desc(adjr2))

# restart process round 10

# store variable names in vector

varlist10 <- varlist9[-5]

# function to run lms

models10 <- lapply(varlist10, function(x) {
  lm(substitute(critics_score ~ imdb_rating+mpaa_rating+title_type+
                  genre+thtr_rel_year+dvd_rel_year+dvd_rel_day+
                  best_dir_win+best_pic_nom+
                  i, list(i = as.name(x))), data = kept)
})

# grab adjusted r squared and output ranked in descending order

adjr2<-NULL
options(scipen=999)

for (i in 1:length(models10)){
  adjr2[[i]]<-summary(models10[[i]])$adj.r.squared
}

results10<-as.data.frame(cbind(varlist10, adjr2))

results10 %>%
  arrange(desc(adjr2))

# restart process round 11

# store variable names in vector

varlist11 <- varlist10[-1]

# function to run lms

models11 <- lapply(varlist11, function(x) {
  lm(substitute(critics_score ~ imdb_rating+mpaa_rating+title_type+
                  genre+thtr_rel_year+dvd_rel_year+dvd_rel_day+
                  best_dir_win+best_pic_nom+runtime+
                  i, list(i = as.name(x))), data = kept)
})

# grab adjusted r squared and output ranked in descending order

adjr2<-NULL
options(scipen=999)

for (i in 1:length(models11)){
  adjr2[[i]]<-summary(models11[[i]])$adj.r.squared
}

results11<-as.data.frame(cbind(varlist11, adjr2))

results11 %>%
  arrange(desc(adjr2))

# restart process round 12

# store variable names in vector

varlist12 <- varlist11[-7]

# function to run lms

models12 <- lapply(varlist12, function(x) {
  lm(substitute(critics_score ~ imdb_rating+mpaa_rating+title_type+
                  genre+thtr_rel_year+dvd_rel_year+dvd_rel_day+
                  best_dir_win+best_pic_nom+runtime+top200_box+
                  i, list(i = as.name(x))), data = kept)
})

# grab adjusted r squared and output ranked in descending order

adjr2<-NULL
options(scipen=999)

for (i in 1:length(models12)){
  adjr2[[i]]<-summary(models12[[i]])$adj.r.squared
}

results12<-as.data.frame(cbind(varlist12, adjr2))

results12 %>%
  arrange(desc(adjr2))

# adj r2 went down on round 12 - go with model11

final<-lm(critics_score ~ imdb_rating+mpaa_rating+title_type+
                genre+thtr_rel_year+dvd_rel_year+dvd_rel_day+
                best_dir_win+best_pic_nom+runtime+top200_box, data = kept)

summary(final)

# Diagnostics

mean(final$residuals)

par(mfrow=c(1,3))

qqnorm(final$residuals)
hist(final$residuals)
plot(final$residuals~final$fitted.values)

# scatter plots of numeric x

par(mfrow=c(2,3))

plot(kept$critics_score~kept$runtime)
abline(lm(kept$critics_score~kept$runtime), col="red", lwd=3)

plot(kept$critics_score~kept$thtr_rel_year)
abline(lm(kept$critics_score~kept$thtr_rel_year), col="red", lwd=3)

plot(kept$critics_score~kept$dvd_rel_year)
abline(lm(kept$critics_score~kept$dvd_rel_year), col="red", lwd=3)

plot(kept$critics_score~kept$dvd_rel_day)
abline(lm(kept$critics_score~kept$dvd_rel_day), col="red", lwd=3)

plot(kept$critics_score~kept$imdb_rating)
abline(lm(kept$critics_score~kept$imdb_rating), col="red", lwd=3)





plot(final$residuals~kept$imdb_rating)
plot(final$residuals~kept$thtr_rel_year)
plot(final$residuals~kept$dvd_rel_year)
plot(final$residuals~kept$dvd_rel_day)
plot(final$residuals~kept$runtime)


plot(final$residuals~kept$title_type)
plot(final$residuals~kept$genre)
plot(final$residuals~kept$best_dir_win)
plot(final$residuals~kept$best_pic_nom)
plot(final$residuals~kept$top200_box)
plot(final$residuals~kept$top200_box)

plot(final$residuals)

acf(final$residuals, main="")

plot(kept$critics_score~final$residuals)


# Predict

# The Revenant
# La La Land
# Get Out
# The Space Between Us

newdata<-read.csv("newdata.csv")

preds<-as.data.frame(predict(final, newdata=newdata, interval="confidence", level=.95))

Title<-c("The Revenant", "La La Land", "Get Out", "The Space Between Us")

preds<-as.data.frame(cbind(Title, preds))

names(preds)<-c("Title", "Predicted Score", "Lower 95%", "Upper 95%")

preds$ActualScore<-newdata$critics_score

preds



