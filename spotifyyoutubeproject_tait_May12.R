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
pairs(spotify_data)
cor(spotify_data, y=Null)

#cannot do pairs or correlation cause there's still non-numeric argument, so going to convert each variable to numeric (Bavita)

spotify_data$Album_type <- c(single=0,compilation=1,album=1)[spotify_data$Album_type] # this line is instead of the for loop above, as they do the same thing and cannot be run together
summary(spotify_data)
'''
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
'''

# Convert all columns to numeric
spotify_data <- as.data.frame(lapply(spotify_data, as.numeric))

pairs(spotify_data)
cor(spotify_data)

#we also have NA values in stream variable, so correlation function does not work. SO, omit the observations that include NAs (Bavita)
is.na(spotify_data)
as.data.frame(na.omit(spotify_data))
cor(na.omit(spotify_data))
pairs(na.omit(spotify_data))


#Youtube data
#removing all the columns that are not important for this analysis at once
remcol <- c(1,2,3,4,5,7,19,20,21,25,28)
youtube_data <- data.frame(as.matrix(data[-remcol]))

#creating dummy variables, ex: converting 'Licensed' and 'official_video' to 0 and 1
youtube_data$Licensed <- as.logical(youtube_data$Licensed)
youtube_data$Licensed <- as.integer(youtube_data$Licensed)

youtube_data$official_video <- as.logical(youtube_data$official_video)
youtube_data$official_video <- as.integer(youtube_data$official_video)

youtube_data$Album_type <- c(single=0,compilation=1,album=1)[youtube_data$Album_type]
youtube_data$Album_type <- as.numeric(youtube_data$Album_type)

summary(youtube_data)
#changing all the variables to numeric (Bavita)
'''
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
'''

# Convert all columns to numeric
youtube_data <- as.data.frame(lapply(youtube_data, as.numeric))

summary(youtube_data)

#this data also has NA value so going to omit the observations that include NA values (Bavita)
youtube_data <- na.omit(youtube_data)

#histograms that show log transformation makes the response closer to normal
hist(youtube_data$Views)
hist(log(youtube_data$Views))
hist(log10(youtube_data$Views))

#log transformation of views
youtube_data <- youtube_data[youtube_data$Views > 0, ] #drop rows that are <= 0 before log transform
youtube_data$Views = log10(youtube_data$Views)

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

youtube_lm_int4 <- lm(Views~Likes+Comments+Licensed+official_video+Loudness*Energy+Danceability*Valence+Album_type+Duration_ms+Liveness+Speechiness+Key+Tempo, data=youtube_data)
summary(youtube_lm_int4)

anova(youtube_lm, youtube_lm1, youtube_lm2, youtube_lm3, youtube_lm4, youtube_lm5, youtube_lm6, youtube_lm_int, youtube_lm_int2, youtube_lm_int3,youtube_lm_int4)


#Yuliya
#Log transforming views for youtube data
youtube_data_ln <- youtube_data
youtube_data_ln <- youtube_data_ln[youtube_data_ln$Views > 0, ] #drop rows that are <= 0 before log transform
youtube_data_ln <- na.omit(youtube_data_ln)
youtube_data_ln$Views <- log(youtube_data_ln$Views) 

#Using youtube_lm6's model with transformed response:

youtube_lm_ln <- lm(Views~Likes+Comments+Licensed+Danceability+Energy+Valence+Album_type+Duration_ms+Speechiness+Tempo, data=youtube_data_ln)
summary(youtube_lm_ln)

library(DAAG)
vif(youtube_lm_ln)
'''
According to variance inflation factor, all variables have values below 5,
which implies relatively low multicollinearity.
'''

residuals <- residuals(youtube_lm_ln)
predicted <- predict(youtube_lm_ln)
# Scatter plot of residuals
plot(predicted, residuals, xlab = "Predicted Values", ylab = "Residuals", main = "Scatter Plot of Residuals")
abline(h = 0, col = "red", lwd = 2)  # Add a horizontal line at y = 0

#residuals look strange, since there should not be values higher than 10
max(predicted) #20.45
max(youtube_data_ln$Views) #9.907
hist(residuals)
#The residuals are approximately normally distributed around the mean and median, which is a good sign.
#Some of the results were seriously overestimated, but very few.


#Split the data on train and test sets for proper accuracy evaluation, then run the model.

set.seed(42)
train_index <- sample(1:nrow(youtube_data_ln), nrow(youtube_data_ln) * 0.7)  # 70% for training
train_data <- youtube_data_ln[train_index, ]
test_data <- youtube_data_ln[-train_index, ]

youtube_ln_train <- lm(Views~Likes+Comments+Licensed+Danceability+Energy+Valence+Album_type+Duration_ms+Speechiness+Tempo, data=train_data)
summary(youtube_ln_train)
#R-squared improved by a 0.02

#Obtain predicted values using test data to compare to actual values in test data
predicted <- predict(youtube_ln_train, newdata = test_data)

#Mean Squared Error
mse <- mean((test_data$Views - predicted)^2) # 1.029 - no idea how to interpret that
mse

#Adjusting the predictors: picking the ones with correlation above 0.15, removing the rest
youtube_ln_train1 <- lm(Views~Likes+Comments+Licensed+official_video+Danceability+Energy+Loudness+Acousticness+Instrumentalness, data=train_data)
summary(youtube_ln_train1)
#R-squared improved!
predicted1 <- predict(youtube_ln_train1, newdata = test_data)
mse1 <- mean((test_data$Views - predicted1)^2) # 0.996 - a bit better
mse1

residuals1 <- test_data$Views - predicted1
plot(predicted1, residuals1, xlab = "Predicted Values", ylab = "Residuals", main = "Scatter Plot of Residuals")
abline(h = 0, col = "red", lwd = 2)  # Add a horizontal line at y = 0
hist(residuals1)

#Same model to fit all data.
predicted5 <- predict(youtube_ln_train1, newdata = youtube_data)
mse5 <- mean((youtube_data$Views - predicted5)^2)
mse5
residuals5 <- youtube_data$Views - predicted5
plot(predicted5, residuals5, xlab = "Predicted Values", ylab = "Residuals", main = "Scatter Plot of Residuals")
abline(h = 0, col = "red", lwd = 2)  # Add a horizontal line at y = 0
hist(residuals5)
ggplot(youtube_data, aes(x = predicted5, y = youtube_data$Views)) + geom_point(colour = "660066") + geom_abline(intercept=0, slope=1) +
  labs(x='Predicted Values', y='Actual Values', title='Predicted vs. Actual Values')


library(ggplot2) 

ggplot(test_data, aes(x = predicted1, y = test_data$Views)) + geom_point() + geom_abline(intercept=0, slope=1) +
        labs(x='Predicted Values', y='Actual Values', title='Predicted vs. Actual Values')

#Trying the earlier models that had decent ANOVA results.
youtube_lm_int2 <- lm(Views~Likes*Comments*Licensed+Danceability+Instrumentalness+Energy+Valence+Album_type+Duration_ms+Liveness+Speechiness+Key+Tempo, data=youtube_data)
residuals2 <- residuals(youtube_lm_int2)
predicted2 <- predict(youtube_lm_int2)
mse2 <- mean((youtube_data$Views - predicted2)^2)
mse2
# Scatter plot of residuals
plot(predicted2, residuals2, xlab = "Predicted Values", ylab = "Residuals", main = "Scatter Plot of Residuals")
abline(h = 0, col = "red", lwd = 2)  # Add a horizontal line at y = 0
ggplot(youtube_data, aes(x = predicted2, y = youtube_data$Views)) + geom_point() + geom_abline(intercept=0, slope=1) +
  labs(x='Predicted Values', y='Actual Values', title='Predicted vs. Actual Values')

youtube_lm_int3 <- lm(Views~Likes*Comments*Licensed*Danceability+Instrumentalness+Energy+Valence+Album_type+Duration_ms+Liveness+Speechiness+Key+Tempo, data=youtube_data)
residuals3 <- residuals(youtube_lm_int3)
predicted3 <- predict(youtube_lm_int3)
mse3 <- mean((youtube_data$Views - predicted3)^2)
mse3
# Scatter plot of residuals
plot(predicted3, residuals4, xlab = "Predicted Values", ylab = "Residuals", main = "Scatter Plot of Residuals")
abline(h = 0, col = "red", lwd = 2)  # Add a horizontal line at y = 0
ggplot(youtube_data, aes(x = predicted3, y = youtube_data$Views)) + geom_point() + geom_abline(intercept=0, slope=1) +
  labs(x='Predicted Values', y='Actual Values', title='Predicted vs. Actual Values')
hist(residuals3)

#This is the same model as youtube_lm_int3, but split on training and test sets to see its' real accuracy.
youtube_lm_int6 <- lm(Views~Likes*Comments*Licensed*Danceability+Instrumentalness+Energy+Valence+Album_type+Duration_ms+Liveness+Speechiness+Key+Tempo, data=train_data)
predicted6 <- predict(youtube_lm_int6, newdata = test_data)
residuals6 <- test_data$Views - predicted6
mse6 <- mean((test_data$Views - predicted6)^2)
mse6
# Scatter plot of residuals
plot(predicted6, residuals6, xlab = "Predicted Values", ylab = "Residuals", main = "Scatter Plot of Residuals")
abline(h = 0, col = "red", lwd = 2)  # Add a horizontal line at y = 0
ggplot(test_data, aes(x = predicted6, y = test_data$Views)) + geom_point() + geom_abline(intercept=0, slope=1) +
  labs(x='Predicted Values', y='Actual Values', title='Predicted vs. Actual Values')
hist(residuals6)

#Before, youtube_lm_int3 seemed one of the best MLR models we have got. It had highest R-squared,
# lowest residual standard error, and slightly more fit on predicted vs actual values plot.
#But after splitting data on train and test, and rerunning the model only on train set,
#then predicting test set it has not seen yet, the MSE worsened a lot (1.433424)!
#youtube_ln_train1 is now the best model we have got. It had MSE of the test data 0.9957658,
#and after using this trained model to predict the full data, we got MSE = 0.9323132.

install.packages("relaimpo")
#Plot relative importance of the variables in the model youtube_ln_train1
relative_importance <- calc.relimp(youtube_ln_train1, type="lmg")$lmg
df = data.frame(x1=names(relative_importance), y1=round(c(relative_importance) * 100,2))
ggplot(df, aes(x = reorder(x1, -y1), y = y1)) + geom_col(fill = "336666") + geom_text(aes(label=y1), vjust=.3, hjust=1.4, size=3, color="black")+
  coord_flip() + labs(title = "Relative importance of variables", y = "Importance level", x = "") + theme_classic(base_size = 15)

