rm(list=ls())

data <- read.csv("Spotify_Youtube.csv", header=TRUE)
View(data)
#remove columns 1-5, 7 in Spotify section
#remove 19-21, 25 in YouTube section
#convert column 6 and 26-27 to dummy variables (binary)

#stepwise regression to select variables, or CV (k-fold)
#Lasso for prediction
#do Spotify and YouTube separately, use comments instead of views in YouTube

#####Cleaning the data
#Spotify data
#Here is a (not effiecient) way I split the data (COLIN)
#datafix1 <- data.frame(as.matrix(data[,-1:-5]))
#datafix2 <- data.frame(as.matrix(datafix1[,-2]))
#spotify_data_worse <- data.frame(as.matrix(datafix2[,-13:-21]))
#here is a better way of doing what I did shoutout to Bavita
spotremcol <- c(1,2,3,4,5,7,19,20,21,22,23,24,25,26,27)
spotify_data <- data.frame(as.matrix(data[-spotremcol]))
#change album/single to 1 for album 0 for single
for(i in 1:20718){
       if(spotify_data$Album_type[i] == "album"){
                  spotify_data$Album_type[i] = 1
       }
       if(spotify_data$Album_type[i] == "compilation"){
                  spotify_data$Album_type[i] = 1
       }
       if(spotify_data$Album_type[i] == "single"){
                  spotify_data$Album_type[i] = 0
       }
}
spotify_data$Album_type <- as.numeric(spotify_data$Album_type)
head(spotify_data$Album_type)
#now making youtube dataset
#datafix3 <- data.frame(as.matrix((data[,-1:-7])))
#datafix4 <- data.frame(as.matrix(datafix3[-12:-14]))
#datafix5 <- data.frame(as.matrix(datafix4[-15]))
#youtube_data_worse <- data.frame(as.matrix(datafix5[-17]))


#checking for correlation:
#pairs(spotify_data)
#cor(spotify_data, y=Null)

#cannot do pairs or correlation cause there's still non-numeric argument, so going to convert each variable to numeric (Bavita)
spotify_data$Album_type <- c(single=0,compilation=1,album=1)[spotify_data$Album_type]
summary(spotify_data)
spotify_data$Album_type <- as.numeric(spotify_data$Album_type)
spotify_data$Danceability <- as.numeric(spotify_data$Danceability)
spotify_data$Energy <- as.numeric(spotify_data$Energy)
spotify_data$Key <- as.numeric(spotify_data$Key)
spotify_data$Loudness <- as.numeric(spotify_data$Loudness)
spotify_data$Speechiness <- as.numeric(spotify_data$Speechiness)
spotify_data$Acousticness <- as.numeric(spotify_data$Acousticness)
spotify_data$Instrumentalness <- as.numeric(spotify_data$Instrumentalness)
spotify_data$Liveness <- as.numeric(spotify_data$Liveness)
spotify_data$Valence <- as.numeric(spotify_data$Valence)
spotify_data$Tempo <- as.numeric(spotify_data$Tempo)
spotify_data$Duration_ms <- as.numeric(spotify_data$Duration_ms)
spotify_data$Stream <- as.numeric(spotify_data$Stream)

#pairs(spotify_data)
#cor(spotify_data)

#we also have NA values in stream variable, so correlation function does not work. SO, omit the observations that include NAs (Bavita)
is.na(spotify_data)
as.data.frame(na.omit(spotify_data))
cor(na.omit(spotify_data))
pairs(na.omit(spotify_data))


#Youtube data
#removing all the columns at once
remcol <- c(1,2,3,4,5,7,19,20,21,25,28)
youtube_data <- data.frame(as.matrix(data[-remcol]))

#converting 'Licensed' and 'official_video' to 0 and 1
youtube_data$Licensed <- as.logical(youtube_data$Licensed)
youtube_data$Licensed <- as.integer(youtube_data$Licensed)

youtube_data$official_video <- as.logical(youtube_data$official_video)
youtube_data$official_video <- as.integer(youtube_data$official_video)

youtube_data$Album_type <- c(single=0,compilation=1,album=1)[youtube_data$Album_type]
youtube_data$Album_type <- as.numeric(youtube_data$Album_type)

summary(youtube_data)
#changing all the variables to numeric (Bavita)
youtube_data$Danceability <- as.numeric(youtube_data$Danceability)
youtube_data$Energy <- as.numeric(youtube_data$Energy)
youtube_data$Key <- as.numeric(youtube_data$Key)
youtube_data$Loudness <- as.numeric(youtube_data$Loudness)
youtube_data$Speechiness <- as.numeric(youtube_data$Speechiness)
youtube_data$Acousticness <- as.numeric(youtube_data$Acousticness)
youtube_data$Instrumentalness <- as.numeric(youtube_data$Instrumentalness)
youtube_data$Liveness <- as.numeric(youtube_data$Liveness)
youtube_data$Valence <- as.numeric(youtube_data$Valence)
youtube_data$Tempo <- as.numeric(youtube_data$Tempo)
youtube_data$Duration_ms <- as.numeric(youtube_data$Duration_ms)
youtube_data$Views <- as.numeric(youtube_data$Views)
youtube_data$Likes <- as.numeric(youtube_data$Likes)
youtube_data$Comments <- as.numeric(youtube_data$Comments)
youtube_data$Licensed <- as.numeric(youtube_data$Licensed)
youtube_data$official_video <- as.numeric(youtube_data$official_video)

summary(youtube_data)

#this data also has NA value so going to omit the observations that include NA values (Bavita)
youtube_data <- na.omit(youtube_data)
pairs(youtube_data)
cor(youtube_data)



####Our goal: To find the best model to predict the number of streams/views of a given song on Spotify and/or YouTube(Bavita)
##mulitple regression on youtube data
#The order of variables depend on the correlation coefficient for views
youtube_lm <- lm(Views~Likes+Comments+Licensed+official_video+Loudness+Danceability+Acousticness+Instrumentalness+Energy+Valence+Album_type+Duration_ms+Liveness+Speechiness+Key+Tempo, data=youtube_data)
summary(youtube_lm)

#taking out official video, loudness, acousticness, instrumentalness, liveness and key  one by one cause they are insignificant variables
youtube_lm1 <- lm(Views~Likes+Comments+Licensed+Loudness+Danceability+Acousticness+Instrumentalness+Energy+Valence+Album_type+Duration_ms+Liveness+Speechiness+Key+Tempo, data=youtube_data)
summary(youtube_lm1)

youtube_lm2 <- lm(Views~Likes+Comments+Licensed+Danceability+Acousticness+Instrumentalness+Energy+Valence+Album_type+Duration_ms+Liveness+Speechiness+Key+Tempo, data=youtube_data)
summary(youtube_lm2)

youtube_lm3 <- lm(Views~Likes+Comments+Licensed+Danceability+Instrumentalness+Energy+Valence+Album_type+Duration_ms+Liveness+Speechiness+Key+Tempo, data=youtube_data)
summary(youtube_lm3)

youtube_lm4 <- lm(Views~Likes+Comments+Licensed+Danceability+Energy+Valence+Album_type+Duration_ms+Liveness+Speechiness+Key+Tempo, data=youtube_data)
summary(youtube_lm4)

youtube_lm5 <- lm(Views~Likes+Comments+Licensed+Danceability+Energy+Valence+Album_type+Duration_ms+Speechiness+Key+Tempo, data=youtube_data)
summary(youtube_lm5)

youtube_lm6 <- lm(Views~Likes+Comments+Licensed+Danceability+Energy+Valence+Album_type+Duration_ms+Speechiness+Tempo, data=youtube_data)
summary(youtube_lm6)
#now all the variables are significant (Bavita)

anova(youtube_lm, youtube_lm1, youtube_lm2, youtube_lm3, youtube_lm4, youtube_lm5, youtube_lm6)
#all the models are equal to each other cause the p value is really high but model youtube_lm3 has the smallest p value among these models
#interaction terms in youtube_lm3:
youtube_lm_int <- lm(Views~Likes*Comments+Licensed+Danceability+Instrumentalness+Energy+Valence+Album_type+Duration_ms+Liveness+Speechiness+Key+Tempo, data=youtube_data)
summary(youtube_lm_int)

anova(youtube_lm, youtube_lm1, youtube_lm2, youtube_lm3, youtube_lm4, youtube_lm5, youtube_lm6, youtube_lm_int)

youtube_lm_int2 <- lm(Views~Likes*Comments*Licensed+Danceability+Instrumentalness+Energy+Valence+Album_type+Duration_ms+Liveness+Speechiness+Key+Tempo, data=youtube_data)
summary(youtube_lm_int2)

youtube_lm_int3 <- lm(Views~Likes*Comments*Licensed*Danceability+Instrumentalness+Energy+Valence+Album_type+Duration_ms+Liveness+Speechiness+Key+Tempo, data=youtube_data)
summary(youtube_lm_int3)

anova(youtube_lm, youtube_lm1, youtube_lm2, youtube_lm3, youtube_lm4, youtube_lm5, youtube_lm6, youtube_lm_int, youtube_lm_int2, youtube_lm_int3)


#Log transforming views for youtube data
youtube_data_ln <- youtube_data
youtube_data_ln$Views <- log(youtube_data_ln$Views)
#Using youtube_lm6's model with transformed response:

youtube_lm_ln <- lm(Views~Likes+Comments+Licensed+Danceability+Energy+Valence+Album_type+Duration_ms+Speechiness+Tempo, data=youtube_data_ln)
summary(youtube_lm_ln)

#Log transforming spotify data
spotify_data_ln <- spotify_data
spotify_data_ln$Stream <- log(spotify_data$Stream)

#Lasso implementation
library(glmnet)
set.seed(5) #to get the same results

train <- sample(1:dim(youtube_data.lasso)[1], dim(youtube_data.lasso)[1]/2)
test <- -train

youtube_data.train <- youtube_data[train,]
youtube_data.test <- youtube_data[test,]

train.mat <- model.matrix(Views ~ ., data= youtube_data.train)[,-1]
test.mat <- model.matrix(Views ~., data = youtube_data.test)[,-1]

fit.lasso <- glmnet(train.mat, youtube_data.train$Views, alpha=1)
cv.lasso <- cv.glmnet(train.mat, youtube_data.train$Views, alpha=1)

bestlam.lasso <- cv.lasso$lambda.min #obtaining optimal lambda using cross validation
bestlam.lasso

pred.lasso <- predict(fit.lasso, s = bestlam.lasso, newx = test.mat)
mean((pred.lasso - youtube_data.test$Views)^2)

