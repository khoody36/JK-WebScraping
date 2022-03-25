library(rvest)
library(xml2)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(robotstxt)
library(tidyr)
library(caTools)

#check permissions
paths_allowed("https://www.nfl.com/players/tom-brady/stats/career")

#create link for Tom Brady career stats
link <- "https://www.nfl.com/players/tom-brady/stats/career"

#Pull stats 
TB <- read_html("https://www.nfl.com/players/tom-brady/stats/career") %>% 
  html_nodes(".nfl-o-roster") %>% 
  html_table()

str(TB[1])

#Superbowl Outcome
Superbowl <- data.frame(SB_APPER = c("no","yes","no","yes","yes","no","no","yes","no","no","no","yes","no","no","yes","no","yes","yes","yes","no","yes","no",""),
                        SB_WIN = c("no","yes","no","yes","yes","no","no","no","no","no","no","no","no","no","yes","no","yes","no","yes","no","yes","no",""))

#Add Superbowl data to TBcareer
TBcareer <- cbind(TB[1],Superbowl)
TBcareer$SB_APPER <- ifelse(TBcareer$SB_APPER=="yes",1,0)
TBcareer$SB_WIN <- ifelse(TBcareer$SB_WIN=="yes",1,0)

#TBcareer structure
str(TBcareer)


TBcareer1 <- cbind(TBcareer$YEAR,TBcareer$TEAM,G,ATT,COMP,PCT,YDS,AVG,LNG,TD,INT,FIRST,FIRSTPCT,PLUS20,SCK,SCKY,RATE,SB_APPER)
colnames(TBcareer1) <- c("YEAR","TEAM","G","ATT","COMP","PCT","YDS","AVG","LNG","TD","INT","FIRST","FIRSTPCT","PLUS20","SCK","SCKY","RATE","SB_APPER")
str(TBcareer1)

#Regression
TBcareer2 <- TBcareer1[!(TBcareer1$YEAR %in% c(2000,2008)), ]

split <- sample.split(TBcareer2, SplitRatio = 0.5)
training <- subset(TBcareer2, split=="TRUE")
testing <- subset(TBcareer2, split=="FALSE")

model1 <- glm(SB_APPER ~ RATE + TD + PCT - INT, training, family = "binomial")
summary(model1)

data <- data.frame(RATE=92.6, TD=28, PCT=60.76, INT=14)
data2 <- data.frame(RATE=102.1,TD=43, PCT=67.45, INT=12)

answer <- predict.glm(model1,data,type = "response")>.5
answer2 <- predict.glm(model1,data2,type = "response")>.5

print(c(answer,answer2))