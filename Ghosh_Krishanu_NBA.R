library("SportsAnalytics")
library('rvest')
library(googleVis)
library(tidyverse)

# Use the SportsAnalytics API for R, to accomplish several sports analytics tasks.???
# Retrieve the NBA data for the 2007 2008 season.

nba.data <- as_tibble(fetch_NBAPlayerStatistics("07-08"))

# Subset the data for your favorite team. Show the code you used to find:
celtics.data <- as_tibble(subset(nba.data, Team == "BOS"))

# Which player has the best three point percentage?
celtics.data %>% 
  group_by(Name) %>% 
  summarise(three_point_Perc = sum(ThreesMade)/sum(TotalPoints)*100) %>%
  filter (three_point_Perc == max(three_point_Perc)) 
  
# Which player has played the largest number of minutes?
celtics.data %>% 
  group_by(Name) %>% 
  summarise(sumTotalPlayedMinutes = sum(TotalMinutesPlayed)) %>%
  filter (sumTotalPlayedMinutes == max(sumTotalPlayedMinutes)) 

# Which player has the most "Steals"?
celtics.data %>% 
  group_by(Name) %>% 
  summarise(TotalSteals = sum(Steals)) %>%
  filter (TotalSteals == max(TotalSteals)) 

# Show 5 teams for the 2007 2008 season that have the most wins in descending order.
# Data source https://www.kaggle.com/boonpalipatana/nba-season-records-from-every-year#Team_Records.csv
nba.data2 <- read.csv("Team_Records.csv")
nba.data2<-drop_na(nba.data2)
nba.data2 <- as_tibble(nba.data2)
nba.data2$Team <- gsub("\\*", "",nba.data2$Team)

nba.data3 <- subset(nba.data2,nba.data2$ï..Season=="2007-08") 
nba.data3 <- select(nba.data3,Team,W,L,Finish)
nba.data3$Team <- gsub("\\*", "",nba.data3$Team)

nba.data3 %>% arrange(desc(nba.data3$W)) %>% top_n(-5)

top.teams <- select(nba.data3 %>% arrange(desc(nba.data3$W)) %>% top_n(-5),Team)

# chart 1
# Line graph - How has the Top 3 of 2007-08 performed over next years
nba.seasons<-c("2008-09","2009-10","2010-11","2011-12","2012-13")

Boston.Celtics  <- nba.data2$W[nba.data2$Team=="Boston Celtics" & (nba.data2$ï..Season %in% nba.seasons)]
Detroit.Pistons <- nba.data2$W[nba.data2$Team=="Detroit Pistons" & (nba.data2$ï..Season %in% nba.seasons)]
Los.Angeles.Lakers <- nba.data2$W[nba.data2$Team=="Los Angeles Lakers" & (nba.data2$ï..Season %in% nba.seasons)]
New.Orleans.Hornets <- nba.data2$W[nba.data2$Team=="New Orleans Hornets" & (nba.data2$ï..Season %in% nba.seasons)]
Utah.Jazz <- nba.data2$W[nba.data2$Team=="Utah Jazz" & (nba.data2$ï..Season %in% nba.seasons)]

nba.line=data.frame(nba.seasons,Boston.Celtics,Detroit.Pistons,Los.Angeles.Lakers,New.Orleans.Hornets,Utah.Jazz)
plot(gvisLineChart(nba.line))

#chart 2
# Column chart - How has the Top 3 of 2007-08 performed over next years
nba.line=data.frame(nba.seasons,Boston.Celtics,Detroit.Pistons,Los.Angeles.Lakers,New.Orleans.Hornets,Utah.Jazz)
plot(gvisColumnChart(nba.line))

#chart 3
# Bubble chart - Relative stats of 2007-8 Celtic players
Bubble <- gvisBubbleChart(celtics.data, idvar="Name", 
                          xvar="GamesPlayed", yvar="Assists",
                          colorvar="Name", sizevar="TotalPoints",
                          options=list(
                            hAxis='{minValue:0, maxValue:125}',
                            vAxis='{minValue:0, maxValue:500}'))
plot(Bubble)

#chart 4
# Guage plot - Celtic 2007-08 Player's Total scores 
celtic.tot<- celtics.data$TotalPoints
celtics.players <- celtics.data$Name
Gauge <-  gvisGauge(data.frame(celtics.players,celtic.tot), options=list(min=min(celtics.data$TotalPoints),
                                      max=max(celtics.data$TotalPoints),
                                      greenFrom=1200, greenTo=1800, yellowFrom=600, yellowTo=1200,
                                      redFrom=0, redTo=600, width=600, height=600))
plot(Gauge)


# Chart 5
# Histogram of Teams with summary of Total Assists, Steals and Turnovers
nba.data  <- nba.data %>% 
                group_by(nba.data$Team) %>% 
                summarise(TotAssists = sum(Assists),
                          TotSteals = sum(Steals),
                          TotTurnovers = sum(Turnovers))

nba.data <- data.frame(nba.data)
hist<- gvisHistogram(nba.data)
plot(hist)

# Reading the HTML code from the website
# url <- "http://www.landofbasketball.com/world_cup_stats/medals_by_year.htm"

#geoChart on the world map show all of the Basketball World Cup Champion countries
nba.champions <- read.csv("champions.csv")
nba.champions <- lapply(nba.champions, gsub, pattern="USA", replacement="US")
nba.champions <- lapply(nba.champions, gsub, pattern="FR of Yugoslavia", replacement="Yugoslavia")
nba.champions <- lapply(nba.champions, gsub, pattern="Soviet union", replacement="Russia")
Geo <- gvisGeoChart(nba.champions, locationvar="Gold", 
                 options=list(projection="kavrayskiy-vii"))
plot(Geo)
