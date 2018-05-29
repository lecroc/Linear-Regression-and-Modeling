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

# check correlarions for colinearity

keptnum<-select_if(kept, is.numeric)

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

p4<-ggplot(kept, aes(x=factor(genre), y=critics_score, fill=factor(genre)))+
  geom_boxplot()+facet_grid(title_type~genre)+
  theme(legend.position = "none", axis.text.x = element_text(angle = 60,hjust = 1))+
  ggtitle("Critic's Score by Genre and Title Type")

p4

p5<-ggplot(kept, aes(x=factor(genre), y=critics_score, fill=factor(genre)))+
  geom_boxplot()+facet_grid(best_actor_win~genre)+
  theme(legend.position = "none", axis.text.x = element_text(angle = 60,hjust = 1))+
  ggtitle("Critic's Score by Genre and Best Actor Win")

p5

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

str(kept)

# kitchen sink model

m1<-lm(critics_score~., data=kept)

summary(m1)








