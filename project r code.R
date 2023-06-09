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
youtube_data_ln <- youtube_data
youtube_data_ln <- youtube_data_ln[youtube_data_ln$Views > 0, ] #drop rows that are <= 0 before log transform
youtube_data_ln <- na.omit(youtube_data_ln)
youtube_data_ln$Views <- log(youtube_data_ln$Views) 
summary(youtube_data_ln)
pairs(youtube_data_ln)
cor(youtube_data_ln)

#relationship between views and other variables
plot(youtube_data_ln$Danceability,youtube_data_ln$Views)
plot(youtube_data_ln$Key,youtube_data_ln$Views)
plot(youtube_data_ln$Loudness,youtube_data_ln$Views)

plot(youtube_data_ln$Speechiness,youtube_data_ln$Views)
plot(log(youtube_data_ln$Speechiness),youtube_data_ln$Views)

plot(youtube_data_ln$Acousticness,youtube_data_ln$Views)
plot(youtube_data_ln$Instrumentalness,youtube_data_ln$Views)
plot(log(youtube_data_ln$Instrumentalness),youtube_data_ln$Views)

plot(youtube_data_ln$Liveness,youtube_data_ln$Views)
plot(youtube_data_ln$Valence,youtube_data_ln$Views)
plot(youtube_data_ln$Tempo,youtube_data_ln$Views)

plot(youtube_data_ln$Duration_ms,youtube_data_ln$Views)
plot(log10(youtube_data_ln$Duration_ms),youtube_data_ln$Views)

plot(youtube_data_ln$Likes,youtube_data_ln$Views)
plot(log(youtube_data_ln$Likes+1),log(youtube_data_ln$Views))

plot(youtube_data_ln$Comments,youtube_data_ln$Views)
plot(log10(youtube_data_ln$Comments),youtube_data_ln$Views)

#transform other variables
youtube_data_ln$Speechiness <- log(youtube_data_ln$Speechiness+1) 
youtube_data_ln$Instrumentalness <- log(youtube_data_ln$Instrumentalness+1) 
youtube_data_ln$Duration_ms <- log10(youtube_data_ln$Duration_ms) 
youtube_data_ln$Likes <- log(youtube_data_ln$Likes+1) 
youtube_data_ln$Comments <- log10(youtube_data_ln$Comments+1) 

cor(youtube_data_ln$views)
####Our goal: To find the best model to predict the number of streams/views of a given song on Spotify and/or YouTube(Bavita)
##mulitple regression on youtube data
#The order of variables depend on the correlation coefficient for views
youtube_lm <- lm(Views~Likes+Comments+Licensed+official_video+Loudness+Danceability+Acousticness+Instrumentalness+Energy+Valence+Album_type+Duration_ms+Liveness+Speechiness+Key+Tempo, data=youtube_data_ln)
summary(youtube_lm)
plot(youtube_lm)

#measuring the multicollinearity in full model, (Licensed, official_video, loudness, and energy have vif value around 3 and every other variables has value around 1. Also, looking at corrleation matrix, licensed is correlated with offcial video and loudness is correlated with energy so drop these variables)
library(DAAG)
vif(youtube_lm)


#drop out key and tempo
youtube_lm_1 <- lm(Views~Likes+Comments+Licensed+official_video+Loudness+Danceability+Acousticness+Instrumentalness+Energy+Valence+Album_type+Duration_ms+Liveness+Speechiness+Key, data=youtube_data_ln)
summary(youtube_lm_1)

youtube_lm_2 <- lm(Views~Likes+Comments+Licensed+official_video+Loudness+Danceability+Acousticness+Instrumentalness+Energy+Valence+Album_type+Duration_ms+Liveness+Speechiness, data=youtube_data_ln)
summary(youtube_lm_2)
vif(youtube_lm_2)

#more backward selection
youtube_lm_y <- lm(Views~Likes+Comments+Licensed+official_video+Loudness+Danceability+Acousticness+Instrumentalness+Energy+Album_type+Duration_ms+Liveness+Speechiness, data=youtube_data_ln)
summary(youtube_lm_y)#without valence
vif(youtube_lm_y)

youtube_lm_z <- lm(Views~Likes+Comments+Licensed+official_video+Loudness+Danceability+Instrumentalness+Energy+Valence+Album_type+Duration_ms+Liveness+Speechiness, data=youtube_data_ln)
summary(youtube_lm_z)#without acousticness
vif(youtube_lm_z)

#models with interactions
youtube_lm_x <- lm(Views~Likes*Comments+Licensed+official_video+Loudness+Danceability+Acousticness+Instrumentalness+Energy+Valence+Album_type+Duration_ms+Liveness+Speechiness, data=youtube_data_ln)
summary(youtube_lm_x)
vif(youtube_lm_x)

youtube_lm_x1 <- lm(Views~Licensed+official_video+Loudness+Danceability+Acousticness+Instrumentalness+Energy+Valence+Album_type+Duration_ms+Liveness+Speechiness, data=youtube_data_ln)
summary(youtube_lm_x1)


anova(youtube_lm,youtube_lm_1,youtube_lm_2,youtube_lm_y,youtube_lm_z)#smallestF value for youtube_lm_y

#final model should be 
youtube_lm_y <- lm(Views~Likes+Comments+Licensed+official_video+Loudness+Danceability+Acousticness+Instrumentalness+Energy+Album_type+Duration_ms+Liveness+Speechiness, data=youtube_data_ln)
summary(youtube_lm_y)
vif(youtube_lm_y)
plot(youtube_lm_y)

residuals <- residuals(youtube_lm_y)
predicted <- predict(youtube_lm_y)
# Scatter plot of residuals
plot(predicted, residuals, xlab = "Predicted Values", ylab = "Residuals", main = "Scatter Plot of Residuals")
abline(h = 0, col = "red", lwd = 2)  # Add a horizontal line at y = 0

#residuals look strange, since there should not be values higher than 10
max(predicted) #22.82
max(youtube_data_ln$Views) #22.81
hist(residuals)
#The residuals are approxiamtely normally distributed around the mean and median, which is a good sign.
#Some of the results were seriously overesimated, but very few.


#Split the data on train and test sets for proper accuracy evaluation, then run the model.

set.seed(42)
train_index <- sample(1:nrow(youtube_data_ln), nrow(youtube_data_ln) * 0.7)  # 70% for training
train_data <- youtube_data_ln[train_index, ]
test_data <- youtube_data_ln[-train_index, ]

youtube_ln_train <- lm(Views~Likes+Comments+Licensed+official_video+Loudness+Danceability+Acousticness+Instrumentalness+Energy+Album_type+Duration_ms+Liveness+Speechiness, data=train_data)
summary(youtube_ln_train)
#R-squared stays the same

#Obtain predicted values using test data to compare to actual values in test data
predicted <- predict(youtube_ln_train, newdata = test_data)

#Mean Squared Error
mse <- mean((test_data$Views - predicted)^2) # 0.39205
mse

#Adjusting the predictors: picking the ones with correlation above 0.15, removing the rest
youtube_ln_train1 <- lm(Views~Likes+Comments+official_video+Licensed+Danceability+Loudness+Acousticness+Instrumentalness+Energy+Album_type+Duration_ms, data=train_data)
summary(youtube_ln_train1)
#R-squared decreased!
predicted1 <- predict(youtube_ln_train1, newdata = test_data)
mse1 <- mean((test_data$Views - predicted1)^2) #0.398
mse1


#with the full model
youtube_ln_train2 <- lm(Views~Likes*Comments+Licensed+official_video+Loudness+Danceability+Acousticness+Instrumentalness+Energy+Valence+Album_type+Duration_ms+Liveness+Speechiness, data=train_data)
summary(youtube_ln_train2)
#R-squared decreased!
predicted2 <- predict(youtube_ln_train2, newdata = test_data)
mse2 <- mean((test_data$Views - predicted2)^2) #0.3799
mse2








residuals1 <- test_data$Views - predicted1
plot(predicted1, residuals1, xlab = "Predicted Values", ylab = "Residuals", main = "Scatter Plot of Residuals")
abline(h = 0, col = "red", lwd = 2)  # Add a horizontal line at y = 0
hist(residuals1)


install.packages("relaimpo")
library(relaimpo)
#Plot relative importance of the variables in the model youtube_ln_train1
relative_importance <- calc.relimp(youtube_ln_train, type="lmg")$lmg
df = data.frame(x1=names(relative_importance), y1=round(c(relative_importance) * 100,2))
library(ggplot2) 
ggplot(df, aes(x = reorder(x1, -y1), y = y1)) + geom_col(fill = "336666") + geom_text(aes(label=y1), vjust=.3, hjust=1.4, size=3, color="black")+
  coord_flip() + labs(title = "Relative importance of variables", y = "Importance level", x = "") + theme_classic(base_size = 15)



#Lasso implementation
library(glmnet)
set.seed(5) #to get the same results

train <- sample(1:dim(youtube_data_ln)[1], dim(youtube_data_ln)[1]/2)
test <- -train

youtube_data.train <- youtube_data_ln[train,]
youtube_data.test <- youtube_data_ln[test,]

train.mat <- model.matrix(Views ~ ., data= youtube_data.train)[,-1]
test.mat <- model.matrix(Views ~., data = youtube_data.test)[,-1]

fit.lasso <- glmnet(train.mat, youtube_data.train$Views, alpha=1)
plot(fit.lasso)
cv.lasso <- cv.glmnet(train.mat, youtube_data.train$Views, alpha=1)
plot(cv.lasso)
bestlam.lasso <- cv.lasso$lambda.min #obtaining optimal lambda using cross validation
bestlam.lasso

pred.lasso <- predict(fit.lasso, s = bestlam.lasso, newx = test.mat)
mean((pred.lasso - youtube_data.test$Views)^2)





