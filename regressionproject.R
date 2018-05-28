# Regression project

# libraries

library(ggplot2)
library(dplyr)
library(GGally)

# data
load("eaca_movies.Rdata")

d1<-as.data.frame(movies)

dim(d1)

# columns to keep critics

keep1<-c("title", "title_type", "genre","runtime","mpaa_rating", "thtr_rel_year",
        "thtr_rel_month", "thtr_rel_day", "dvd_rel_year", "dvd_rel_year",
        "dvd_rel_day", "imdb_rating", "imdb_num_votes",
        "critics_score", "best_pic_nom",
        "best_pic_win", "best_actor_win", "best_actress_win", "best_dir_win",
        "top200_box")

# columns to keep audience

keep2<-c("title", "title_type", "genre","runtime","mpaa_rating", "thtr_rel_year",
         "thtr_rel_month", "thtr_rel_day", "dvd_rel_year", "dvd_rel_year",
         "dvd_rel_day", "imdb_rating", "imdb_num_votes",
         "audience_score", "best_pic_nom",
         "best_pic_win", "best_actor_win", "best_actress_win", "best_dir_win",
         "top200_box")

# model dataset for critics

kept1<-d1 %>%
  select(keep1)

# remove records with nas

cc1<-complete.cases(kept1)
kept1<-kept1[cc1,]
sum(is.na(kept1))

# model dataset for audience

kept2<-d1 %>%
  select(keep2)

# remove records with nas

cc2<-complete.cases(kept2)
kept2<-kept2[cc2,]
sum(is.na(kept2))

# check for colinearity for critics numeric columns

keptnum1<-select_if(kept1, is.numeric)

p1<-ggpairs(keptnum1)

p1

# check for colinearity for audience columns

keptnum2<-select_if(kept2, is.numeric)

p2<-ggpairs(keptnum2)

p2


# kitchen sink model

# drop title
kept1<-kept1[,2:19]

m1<-lm(critics_score~., data=kept1)

summary(m1)








