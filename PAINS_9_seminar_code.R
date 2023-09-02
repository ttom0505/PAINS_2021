# Load DAta
setwd("C:/Users/user/Desktop/Korea University/2021 전역 후/PAINS/")
player <- read.csv("Golden 97.csv", header=T)
player<-data.frame(player)
colnames(player)[4:11]<- c("Appear1718", "Appear1819", "Appear1920", "Appear2021", "Time1718", "Time1819", "Time1920", "Time2021")
summary(player)

# Player Appearance in K League 2021
Appear2021K <- ifelse(player$Last.Season.Europe == "No", (player$Appear2021)*2, player$Appear2021)
player$Appear2021K<- Appear2021K

# Player Appearance Time in K League 2021
Time2021K <- ifelse(player$Last.Season.Europe == "No", (player$Time2021)*2, player$Appear2021)
player$Time2021K<- Time2021K
player<-player[,c(1:7,13,8:11,14,12)]

# Remove Goalkeeper Data
player_no.gk<-player[-c(1,12,21,35),]

# Load ggplot Library
library(ggplot2)

# Factorize to 1,0 by in Tokyo Olympic or Not
player$Tokyo<- factor(player$Tokyo)
player_no.gk$Tokyo <- factor(player_no.gk$Tokyo)

# Logistic Regression
firstappear<-glm(Tokyo~Appear1718, data=player_no.gk, family=binomial)
lastappear<-glm(Tokyo~Appear2021K, data=player_no.gk, family=binomial)
firstgame <- glm(Tokyo~Time1718, data=player_no.gk, family=binomial)
lastgame <- glm(Tokyo~Time2021K, data=player_no.gk, family=binomial)

summary(firstappear)
summary(lastappear)
summary(firstgame)
summary(lastgame)

# cdplot Graphs
cdplot(Tokyo~Appear1718, data=player_no.gk)
cdplot(Tokyo~Appear2021K, data=player_no.gk)
cdplot(Tokyo~Time1718, data=player_no.gk)
cdplot(Tokyo~Time2021K, data=player_no.gk)

# Counter examples for overworked players
dev.off()
par(new=TRUE)
plot(c(2018:2021), player[31,c(9,10,11,13)], type='o', lty=1, xlab="Season", ylab="Time", col="blue", ylim=c(1,3500), pch=15)
plot(c(2018:2021), player[36,c(9,10,11,13)], type='o', lty=1, xlab="Season", ylab="Time", col="orange", ylim=c(1,3500), pch=15)
abline(h=1710, col="green")
abline(h=3060, col="red")
legend(x=2020.3, y=500, c("Kim, Daewon", "Won, Dujae"), col=c("orange", "blue"), pch=c(15,15))
