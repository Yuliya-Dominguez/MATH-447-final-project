rm(list=ls())

data <- read.csv("Spotify_Youtube.csv", header=TRUE)
View(data)
#remove columns 1-5, 7 in Spotify section
#remove 19-21, 25 in YouTube section
#convert column 6 and 26-27 to dummy variables (binary)

#stepwise regression to select variables, or CV
#Lasso for prediction
#do Spotify and YouTube separately, use comments instead of views in YouTube