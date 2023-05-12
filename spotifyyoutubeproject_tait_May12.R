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
#removing all the columns at once
remcol <- c(1,2,3,4,5,6,7,19,20,21,25,28)
youtube_data <- data.frame(as.matrix(data[-remcol]))

#converting 'Licensed' and 'official_video' to 0 and 1
youtube_data$Licensed <- as.logical(youtube_data$Licensed)
youtube_data$Licensed <- as.integer(youtube_data$Licensed)

youtube_data$official_video <- as.logical(youtube_data$official_video)
youtube_data$official_video <- as.integer(youtube_data$official_video)

