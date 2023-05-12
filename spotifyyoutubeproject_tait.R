rm(list=ls())

data <- read.csv("Spotify_Youtube.csv", header=TRUE)
View(data)
#remove columns 1-5, 7 in Spotify section
#remove 19-21, 25 in YouTube section
#convert column 6 and 26-27 to dummy variables (binary)

#stepwise regression to select variables, or CV (k-fold)
#Lasso for prediction
#do Spotify and YouTube separately, use comments instead of views in YouTube

#Here is a (not effiecient) way I split the data (COLIN)
datafix1 <- data.frame(as.matrix(data[,-1:-5]))
datafix2 <- data.frame(as.matrix(datafix1[,-2]))
spotify_data <- data.frame(as.matrix(datafix2[,-13:-21]))
#change album/single to 1 for album 0 for single
