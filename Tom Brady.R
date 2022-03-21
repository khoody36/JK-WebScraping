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
TBstats <- read_html(link) %>% html_nodes(".nfl-t-stats__col-16") %>% html_text()


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







