# Load Libraries
library(tidyverse)
library(ggplot2)
library(readxl)

# Load Data
setwd("C:/Users/user/Desktop/Korea University/2021 전역 후/PAINS/")
EPL <- read.csv("EPL.csv",header=T)
colnames(EPL)[c(2,7,8)]<-c("Market_Value", "GCperG", "Clean_Seats")

# Linear Regression with x = Goal
attach(EPL)
goal1 <- lm(Market_Value ~ Goal, EPL)
summary(goal1)
plot(Goal, Market_Value, main="Market Value by Goals (by 1M EUR)")
abline(summary(goal1), col="blue")
cor(Market_Value[1:55], Goal[1:55])


windows(width=8, height=6)
ggplot(EPL, aes(x=Goal, y=Market_Value))+geom_point(size=3) +geom_smooth(method='lm', se=F) +ggtitle("Market Value by Goals")+ theme(plot.title=element_text(hjust=0.5, color="red")) +ylab("Market Value(1M EUR)")
savePlot("Market Value by Goals.png", type="png")

# Specify by Positions
goal1.f<-lm(Market_Value~Goal,subset(EPL, Position=="FW"))
summary(goal1.f)
goal1.m<-lm(Market_Value~Goal,subset(EPL, Position=="MF"))
summary(goal1.m)
goal1.d<-lm(Market_Value~Goal,subset(EPL, Position=="DF"))
summary(goal1.d)
goal1.g<-lm(Market_Value~Goal,subset(EPL, Position=="GK"))
summary(goal1.g)


windows(width=8, height=6)
ggplot(EPL, aes(x=Goal, y=Market_Value))+geom_point(aes(size=3, col=Position)) +geom_smooth(method='lm', se=F) +ggtitle("Market Value by Goals & Position")+ theme(plot.title=element_text(hjust=0.5, color="red")) +ylab("Market Value(1M EUR)") +scale_color_manual(values=c("DF"="#0033ff","FW"="#ff5500","GK"="#ffb500","MF"="#00a000"))
savePlot("Market Value by Goals & Position.png", type="png")



# Linear Regression by Goal + Assist
windows(width=8, height=6)
ggplot(EPL, aes(x=Goal, y=Assist)) +geom_point(size=3)
savePlot("Cor of Goal and Assist.png", type="png")
cor(Goal[1:55],Assist[1:55])

goal2 <- lm(Market_Value ~ Score, EPL)
summary(goal2)

windows(width=8, height=6)
ggplot(EPL, aes(x=Score, y=Market_Value))+geom_point(size=3) +geom_smooth(method='lm', se=F) +ggtitle("Market Value by Goal+Assist")+ theme(plot.title=element_text(hjust=0.5, color="red")) +ylab("Market Value(1M EUR)")
savePlot("Market Value by Score.png", type="png")
cor(Market_Value[1:55],Score[1:55])

# Specify by positions
goal2.f<-lm(Market_Value~Score,subset(EPL, Position=="FW"))
summary(goal2.f)
goal2.m<-lm(Market_Value~Score,subset(EPL, Position=="MF"))
summary(goal2.m)
goal2.d<-lm(Market_Value~Score,subset(EPL, Position=="DF"))
summary(goal2.d)
goal2.g<-lm(Market_Value~Score,subset(EPL, Position=="GK"))
summary(goal2.g)

windows(width=8, height=6)
ggplot(EPL, aes(x=Score, y=Market_Value))+geom_point(aes(size=3, col=Position)) +geom_smooth(method='lm', se=F) +ggtitle("Market Value by Goal+Assist & Position")+ theme(plot.title=element_text(hjust=0.5, color="red")) +xlab("Goal+Assist")+ylab("Market Value(1M EUR)") +scale_color_manual(values=c("DF"="#0033ff","FW"="#ff5500","GK"="#ffb500","MF"="#00a000"))
savePlot("Market Value by Score&Position.png", type="png")


# Linear Regression by GK Stats
Goal3.1<-lm(Market_Value~GCperG, subset(EPL, Position=="GK"))
summary(Goal3.1)
Goal3.2<-lm(Market_Value~Clean_Seats, subset(EPL, Position=="GK"))
summary(Goal3.2)
Goal3.3<-lm(Market_Value~(GCperG+Clean_Seats), subset(EPL, Position=="GK"))
summary(Goal3.3)

# Linear Regression by Powerranking
EPL.P<-na.omit(EPL)
EPL.P<-EPL[-c(4,6,19,29,56),]
EPL.P
goal4<-lm(Market_Value~Power.Ranking, EPL.P)
summary(goal4)

windows(width=8, height=6)
ggplot(EPL.P, aes(x=Power.Ranking, y=Market_Value))+geom_point(size=3) +geom_smooth(method='lm', se=F) +ggtitle("Market Value by Power Ranking")+ theme(plot.title=element_text(hjust=0.5, color="red")) +ylab("Market Value(1M EUR)")
savePlot("Market Value by PR.png", type="png")
cor(EPL.P$Market_Value,EPL.P$Power.Ranking)

# Specific Positions
goal4.f<-lm(Market_Value~Power.Ranking,subset(EPL.P, Position=="FW"))
summary(goal4.f)
goal4.m<-lm(Market_Value~Power.Ranking,subset(EPL.P, Position=="MF"))
summary(goal4.m)
goal4.d<-lm(Market_Value~Power.Ranking,subset(EPL.P, Position=="DF"))
summary(goal4.d)
goal4.g<-lm(Market_Value~Power.Ranking,subset(EPL.P, Position=="GK"))
summary(goal4.g)

windows(width=8, height=6)
ggplot(EPL.P, aes(x=Power.Ranking, y=Market_Value))+geom_point(aes(size=3, col=Position)) +geom_smooth(method='lm', se=F) +ggtitle("Market Value by Power Ranking & Position")+ theme(plot.title=element_text(hjust=0.5, color="red")) +xlab("Power Ranking")+ylab("Market Value(1M EUR)") +scale_color_manual(values=c("DF"="#0033ff","FW"="#ff5500","GK"="#ffb500","MF"="#00a000"))
savePlot("Market Value by PR&Position.png", type="png")

# Compare others with 'Thomas Muller'
EPL.M<-EPL
EPL.M[56,c(1,3,10)]<-c("Thomas Muller", "Muller", "NA")
EPL.M[56,c(2,4,5,6,7,8,9)]<-c(30, 13, 19, 32, 0, 0, 7.51)
EPL.M


windows(width=8, height=6)
ggplot(EPL.M, aes(x=Goal, y=Market_Value))+geom_point(aes(size=3, col=Position)) +geom_smooth(method='lm', se=F) +ggtitle("Market Value by Goals & Position")+ theme(plot.title=element_text(hjust=0.5, color="red")) +ylab("Market Value(1M EUR)") +scale_color_manual(values=c("DF"="#0033ff","FW"="#ff5500","GK"="#ffb500","MF"="#00a000", "Muller"="#AA0000"))
savePlot("Market Value of Muller, Goals & Position.png", type="png")

windows(width=8, height=6)
ggplot(EPL.M, aes(x=Score, y=Market_Value))+geom_point(aes(size=3, col=Position)) +geom_smooth(method='lm', se=F) +ggtitle("Market Value by Score & Position")+ theme(plot.title=element_text(hjust=0.5, color="red")) +ylab("Market Value(1M EUR)") +scale_color_manual(values=c("DF"="#0033ff","FW"="#ff5500","GK"="#ffb500","MF"="#00a000", "Muller"="#AA0000"))
savePlot("Market Value of Muller, Score & Position.png", type="png")

dev.off()
