library(rvest)
library(xml2)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(robotstxt)
library(tidyr)

#check permissions
paths_allowed("https://www.nfl.com/players/tom-brady/stats/career")

#create link for Tom Brady career stats
link <- "https://www.nfl.com/players/tom-brady/stats/career"

#Pull stats 
TBstats <- read_html(link) %>% html_nodes(".nfl-t-stats__col-16") %>% html_text2()

#Make stats by year
StatsCat <- as.data.frame(TBstats[c(1:17)])
TBstats2000 <- as.data.frame(t(TBstats[c(18:34)]))
TBstats2001 <- as.data.frame(t(TBstats[c(35:51)]))
TBstats2002 <- as.data.frame(t(TBstats[c(52:68)]))
TBstats2003 <- as.data.frame(t(TBstats[c(69:85)]))
TBstats2004 <- as.data.frame(t(TBstats[c(86:102)]))
TBstats2005 <- as.data.frame(t(TBstats[c(103:119)]))
TBstats2006 <- as.data.frame(t(TBstats[c(120:136)]))
TBstats2007 <- as.data.frame(t(TBstats[c(137:153)]))
TBstats2008 <- as.data.frame(t(TBstats[c(154:170)]))
TBstats2009 <- as.data.frame(t(TBstats[c(171:187)]))
TBstats2010 <- as.data.frame(t(TBstats[c(188:204)]))
TBstats2011 <- as.data.frame(t(TBstats[c(205:221)]))
TBstats2012 <- as.data.frame(t(TBstats[c(222:238)]))
TBstats2013 <- as.data.frame(t(TBstats[c(239:255)]))
TBstats2014 <- as.data.frame(t(TBstats[c(256:272)]))
TBstats2015 <- as.data.frame(t(TBstats[c(273:289)]))
TBstats2016 <- as.data.frame(t(TBstats[c(290:306)]))
TBstats2017 <- as.data.frame(t(TBstats[c(307:323)]))
TBstats2018 <- as.data.frame(t(TBstats[c(324:340)]))
TBstats2019 <- as.data.frame(t(TBstats[c(341:357)]))
TBstats2020 <- as.data.frame(t(TBstats[c(358:374)]))
TBstats2021 <- as.data.frame(t(TBstats[c(375:391)]))

#Categories
categories <- c('YEAR','Team','G','ATT','COMP','PCT',
                 'YDS','AVG','LNG','TD','INT','1st','1st%',
                 '20+','SCK','SCKY','RATE')

#List of data frames
ListTBdf <- list(TBstats2000, TBstats2001, TBstats2002, TBstats2003,
               TBstats2004, TBstats2005, TBstats2006, TBstats2007,
               TBstats2008, TBstats2009, TBstats2010, TBstats2011,
               TBstats2012, TBstats2013, TBstats2014, TBstats2015,
               TBstats2016, TBstats2017, TBstats2018, TBstats2019,
               TBstats2020, TBstats2021)

#Rename columns
ListTBdf1 <- lapply(ListTBdf, function(x){names(x) <- categories; return(x)})

#Combine data frames
TBcareer <- rbind(TBstats2000, TBstats2001, TBstats2002, TBstats2003,
                  TBstats2004, TBstats2005, TBstats2006, TBstats2007,
                  TBstats2008, TBstats2009, TBstats2010, TBstats2011,
                  TBstats2012, TBstats2013, TBstats2014, TBstats2015,
                  TBstats2016, TBstats2017, TBstats2018, TBstats2019,
                  TBstats2020, TBstats2021)

#Rename columns TBcareer
colnames(TBcareer) <- categories


#Superbowl Outcome
Superbowl <- data.frame(SB_APPER = c("no","yes","no","yes","yes","no","no","yes","no","no","no","yes","no","no","yes","no","yes","yes","yes","no","yes","no"),
                        SB_WIN = c("no","yes","no","yes","yes","no","no","no","no","no","no","no","no","no","yes","no","yes","no","yes","no","yes","no"))

#Add Superbowl data to TBcareer
TBcareer <- cbind(TBcareer,Superbowl)
TBcareer$SB_APPER <- ifelse(TBcareer$SB_APPER=="yes",TRUE,FALSE)
TBcareer$SB_WIN <- ifelse(TBcareer$SB_WIN=="yes",TRUE,FALSE)

#Clean-up white space
TBcareer <- TBcareer %>% mutate_if(is.character, str_trim)

#Structure Data
unlist(TBcareer)
G <- as.numeric(TBcareer$G) %>% as.data.frame()
ATT <- as.numeric(TBcareer$ATT) %>% as.data.frame()
COMP <- as.numeric(TBcareer$COMP) %>% as.data.frame()
PCT <- as.numeric(TBcareer$PCT) %>% as.data.frame()
YDS <- as.numeric(TBcareer$YDS) %>% as.data.frame()
AVG <- as.numeric(TBcareer$AVG) %>% as.data.frame()
LNG <- as.numeric(TBcareer$LNG) %>% as.data.frame()
TD <- as.numeric(TBcareer$TD) %>% as.data.frame()
INT <- as.numeric(TBcareer$INT) %>% as.data.frame()
FIRST <- as.numeric(TBcareer$`1st`) %>% as.data.frame()
FIRSTPCT <- as.numeric(TBcareer$`1st%`) %>% as.data.frame()
PLUS20 <- as.numeric(TBcareer$`20+`) %>% as.data.frame()
SCK <- as.numeric(TBcareer$SCK) %>% as.data.frame()
SCKY <- as.numeric(TBcareer$SCKY)%>% as.data.frame()
RATE <- as.numeric(TBcareer$RATE) %>% as.data.frame()

TBcareer1 <- cbind(G,ATT,COMP,PCT,YDS,AVG,LNG,TD,INT,FIRST,FIRSTPCT,PLUS20,SCK,SCKY,RATE)
colnames(TBcareer1) <- c("G","ATT","COMP","PCT","YDS","AVG","LNG","TD","INT","FIRST","FIRSTPCT","PLUS20","SCK","SCKY","RATE")
str(TBcareer1)


<<<<<<< HEAD
#TDLoop
=======

#Regression
model1 <- lm(TBcareer1$RATE ~ TBcareer1$G )

summary(model1)

>>>>>>> 6f7635de625094b7f07ca49fbcab05a724efdc7e

i = as.integer(length(TBcareer$TD))
bestTD = as.integer(0)
for(x in 1:i){
  if(TBcareer$TD[x] > bestTD) {
    bestTD = TBcareer$TD[x]
  }
}
print (bestTD)

<<<<<<< HEAD

TBcareer$SCK
#copy as numeric dataframe
len = as.integer(nrow(TBcareer))

bradyYear <- 1:len
bradyG <- 1:len
bradyATT <- 1:len
bradyCOMP <- 1:len
bradyPCT <- 1:len
bradyYDS <- 1:len
bradyAVG <- 1:len
bradyLNG <- 1:len
bradyTD <- 1:len
bradyINT <- 1:len
brady1st <- 1:len
brady1stPct <- 1:len
brady20 <- 1:len
bradySCK <- 1:len
bradySCKY <- 1:len
bradyRATE <- 1:len

=======
max(TBcareer$TD)
>>>>>>> 6f7635de625094b7f07ca49fbcab05a724efdc7e



for(x in 1:len){
  bradyYear[x] <- as.numeric(TBcareer$YEAR[x])
  bradyG[x] <- as.numeric(TBcareer$G[x])
  bradyATT[x] <- as.numeric(TBcareer$ATT[x])
  bradyCOMP[x] <- as.numeric(TBcareer$COMP[x])
  bradyPCT[x] <- as.numeric(TBcareer$PCT[x])
  bradyYDS[x] <- as.numeric(TBcareer$YDS[x])
  bradyAVG[x] <- as.numeric(TBcareer$AVG[x])
  bradyLNG[x] <- as.numeric(TBcareer$LNG[x])
  bradyTD[x] <- as.numeric(TBcareer$TD[x])
  bradyINT[x] <- as.numeric(TBcareer$INT[x])
  brady1st[x] <- as.numeric(TBcareer$`1st`[x])
  brady1stPct[x] <- as.numeric(TBcareer$`1st%`[x])
  brady20[x] <- as.numeric(TBcareer$`20+`[x])
  bradySCK[x] <- as.numeric(TBcareer$SCK[x])
  bradySCKY[x] <- as.numeric(TBcareer$SCKY[x])
  bradyRATE[x] <- as.numeric(TBcareer$RATE[x])
}

bradyCareer <- rbind (bradyYear, bradyG, bradyATT, bradyCOMP, bradyPCT, bradyYDS, bradyAVG, bradyLNG, bradyTD, bradyINT,
                brady1st, brady1stPct, brady20, bradySCK, bradySCKY, bradyRATE)

