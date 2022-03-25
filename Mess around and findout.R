
library(pacman)
p_load(rvest,xml2,dplyr,stringr,ggplot2,plotly,robotstxt,tidyr,caTools)

#check permissions
paths_allowed("https://www.nfl.com/players/tom-brady/stats/career")

#create link for Tom Brady career stats
link <- "https://www.nfl.com/players/tom-brady/stats/career"

#Pull stats 
TB <- read_html(link) %>% 
  html_nodes(".nfl-o-roster") %>% 
  html_table()

#Superbowl Outcome
Superbowl <- data.frame(SB_APPER = c("no","yes","no","yes","yes","no","no","yes","no","no","no","yes","no","no","yes","no","yes","yes","yes","no","yes","no",""),
                        SB_WIN = c("no","yes","no","yes","yes","no","no","no","no","no","no","no","no","no","yes","no","yes","no","yes","no","yes","no",""))

#Add Superbowl data to TBcareer
TBcareer <- cbind(TB[1],Superbowl)
TBcareer$SB_APPER <- ifelse(TBcareer$SB_APPER=="yes",1,0)
TBcareer$SB_WIN <- ifelse(TBcareer$SB_WIN=="yes",1,0)

#Regression
TBcareer1 <- TBcareer[!(TBcareer$YEAR %in% c(2000,2008)), ]

split <- sample.split(TBcareer1, SplitRatio = 0.5)
training <- subset(TBcareer1, split=="TRUE")
testing <- subset(TBcareer1, split=="FALSE")

model1 <- glm(SB_APPER ~ RATE + TD + PCT - INT, training, family = "binomial")
summary(model1)

data2004 <- data.frame(RATE=92.6, TD=28, PCT=60.76, INT=14)
data2021 <- data.frame(RATE=102.1,TD=43, PCT=67.45, INT=12)

answer <- predict.glm(model1,data2004,type = "response")>.5
answer2 <- predict.glm(model1,data2021,type = "response")>.5

print(c(answer,answer2))