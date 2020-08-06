library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(RCurl)
library(grid)
library(jpeg)
library(stringr)
library(rapport)
library(rapportools)
library(plotly)
library(tidyverse)
library(ballr)
library(tictoc)



library(hexbin)
headers = c(
  "accept-encoding" = "gzip, deflate, br",
  "accept-language" = "en-US,en;q=0.9",
  "cache-control" = "no-cache",
  "connection" = "keep-alive",
  "host" = "stats.nba.com",
  "pragma" = "no-cache",
  "referer" = "https://www.nba.com/",
  "upgrade-insecure-requests" = "1",
  "user-agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_2) AppleWebKit/601.3.9 (KHTML, like Gecko) Version/9.0.2 Safari/601.3.9"
)

getNames <- function() {
  url <- "https://stats.nba.com/stats/commonallplayers?LeagueID=00&Season=2015-16&IsOnlyCurrentSeason=0"
  request = GET(url,  add_headers(headers))
  content = fromJSON(content(request, as = "text"))
  players <- filter(data.frame(content$resultSets$rowSet[[1]], stringsAsFactors = FALSE),X6 > 2009)
  colnames(players) <- content$resultSets$headers[[1]]
  return (players)
}

seasonPart <- function(season){
  originalSeason <- season
  newSeason <- 0
  season <- season + 1
  for (i in c(0,1)){
    newSeason = newSeason + (season %% 10) * 10^ i
    season = floor(season / 10)
  }
  return (paste0(originalSeason,"-",newSeason))
}
listofSeasons <- function(start=2009,end=2019) {
  listofSeasons = c()
  for (i in c(start:end)){
    listofSeasons <- c(listofSeasons, seasonPart(i))
  }
  return (listofSeasons)
  
}


playerShotData <- function(playerId,season=""){
  url = paste0("https://stats.nba.com/stats/shotchartdetail?AheadBehind=&ClutchTime=&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&EndPeriod=&EndRange=&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&Period=0&PlayerID=",playerId,"&PlayerPosition=&PointDiff=&Position=&RangeType=&RookieYear=&Season=",season,"&SeasonSegment=&SeasonType=Regular+Season&StartPeriod=&StartRange=&TeamID=0&VsConference=&VsDivision=")
  request = GET(url,  add_headers(headers))
  content = fromJSON(content(request, as = "text"))
  if (is.empty(content$resultSets$rowSet[[1]])){
    return(0)
  }
  else {
    shots <- filter(data.frame(content$resultSets$rowSet[[1]], stringsAsFactors = FALSE))
    
    colnames(shots) <- content$resultSets$headers[[1]]
    return(shots)
    
  }
  
}


gameLogs <-  function(season){
  url = paste0("https://stats.nba.com/stats/leaguegamelog?Counter=0&DateFrom=&DateTo=&Direction=ASC&LeagueID=00&PlayerOrTeam=T&Season=",season,"&SeasonType=Regular+Season&Sorter=DATE")
  request = GET(url,  add_headers(headers))
  content = fromJSON(content(request, as = "text"))
  
  games <- data.frame(content$resultSets$rowSet[[1]], stringsAsFactors = FALSE)
  colnames(games) <- content$resultSets$headers[[1]]
  return(games)
}
roster <- function(teamName,season){
  
  seasonGameLogs <- read.csv(paste0("datasets/gameLogs/NBA",extractDash(season),".csv"),stringsAsFactors = FALSE)
  x <- seasonGameLogs %>% filter(TEAM_ABBREVIATION == teamName)
  if (nrow(x)==0) {
    return(0)
  }
  else {
    teamId <- x$TEAM_ID[[1]]
    
    url = paste0("https://stats.nba.com/stats/commonteamroster?LeagueID=&Season=",season,"&TeamID=",teamId)
    request = GET(url,  add_headers(headers))
    content = fromJSON(content(request, as = "text"))
    roster <- data.frame(content$resultSets$rowSet[[1]], stringsAsFactors = FALSE)

    colnames(roster) <- content$resultSets$headers[[1]]
    
    return(roster)
    
  }
  
}
extractDash <-function(date){
  season <- str_replace(date,"-","")
  return(season)
}

LeagueAverages <- function(season) {
  url = paste0("https://stats.nba.com/stats/shotchartdetail?AheadBehind=&ClutchTime=&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&EndPeriod=&EndRange=&GameID=&GameSegment=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&Period=0&PlayerID=","204054","&PlayerPosition=&PointDiff=&Position=&RangeType=&RookieYear=&Season=",season,"&SeasonSegment=&SeasonType=Regular+Season&StartPeriod=&StartRange=&TeamID=0&VsConference=&VsDivision=")
  request = GET(url,  add_headers(headers))
  content = fromJSON(content(request, as = "text"))
  roster <- data.frame(content$resultSets$rowSet[[2]],stringsAsFactors = FALSE)
  colnames(roster) <- content$resultSets$headers[[2]]
  roster$FGA <- as.numeric(roster$FGA)
  roster$FG_PCT <- as.numeric(roster$FG_PCT)
  roster$FGM <- as.numeric(roster$FGM)

  
  return(roster)
  
}
#' Generate a heat map for a given player and season 
#' 
#' @param playerName Name of player
#' @param season Season
#' @param clarity how focused the heatmap will be
#' @param restrictedArea Include shots in the restricted area or not
#' @param quarter Quarter, 5 is equal to all the quarters
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' playerHeatMap("LeBron James", "2018-19",FALSE,1)
playerHeatMap <- function(playerName, season, clarity, restrictedArea, quarter) {

  courtImg.URL <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
  court <- rasterGrob(readJPEG(getURLContent(courtImg.URL)),
                      width=unit(1,"npc"), height=unit(1, "npc"))
  playerdf <- getPlayerdf(playerName, season)
  if (quarter != 5) {
    
    playerdf <- filter(playerdf, PERIOD == quarter)
  
  }
  
  if (!restrictedArea) {
    
    playerdf <- filter(playerdf, SHOT_ZONE_BASIC != "Restricted Area")
    
  }
  playerdf$LOC_X <- as.numeric(as.character(playerdf$LOC_X))
  playerdf$LOC_Y <- as.numeric(as.character(playerdf$LOC_Y))
  xList <- c()
  yList <- c()
  count <- c()
  for ( x in seq(250, -250, by= -1 *clarity)) {
    
    for (y in seq(-50, 420, by=clarity)) {
      
      xList<- c(xList, x)
      yList<- c(yList, y)
      count <- c(count, nrow(filter(playerdf, LOC_X >= x - 25 & LOC_X <= x + 25 & LOC_Y >= y - 25 & LOC_Y <= y + 25)))
    }
  }
  
  heat <- data.frame(xList, yList, count)
  return( ggplot(heat, aes(x= xList, y= yList)) +
           annotation_custom(court, -250, 250, -50, 420) +
           xlim(250, -250) +
           ylim(-50, 420) +
           geom_rug(alpha = 0.2) +
           coord_fixed() +
           geom_tile(aes(fill = count)) +
           scale_fill_viridis_c(option = "B", direction = -1,alpha=.7) +
         ggtitle(paste("Shot Chart\n", unique(playerdf$PLAYER_NAME), sep = "")) +
           theme(line = element_blank(),
                 axis.title.x = element_blank(),
                 axis.title.y = element_blank(),
                 axis.text.x = element_blank(),
                 axis.text.y = element_blank(),
                 legend.title = element_blank(),
                 plot.title = element_text(size = 15, lineheight = 0.9, face = "bold"))
  )
}
#' Generate a Boxplot by quarter for a given player and season 
#' 
#' @param name Name of player
#' @param season Season
#' @return A boxplot
#' @examples
#' playerDistributionData("LeBron James, "2018-19")
playerDistributionData <- function(name,season) {
  playerdf <- getPlayerdf(name, season)
  grouped_distance <- group_by(playerdf,PERIOD)
  count <- count(grouped_distance, SHOT_ATTEMPTED_FLAG)[["n"]]
  grouped_distance <- summarise(group_by(playerdf, PERIOD), name = max(PLAYER_NAME), averageShotDistance = mean(SHOT_DISTANCE), numberofCloseRangeShots = sum(SHOT_ZONE_BASIC == "Restricted Area") , numberofMidRangeShots = sum(SHOT_TYPE == "2PT Field Goal" & SHOT_ZONE_BASIC != "Restricted Area"), numberofThrees = sum(SHOT_TYPE == "3PT Field Goal"), PercentageofShots3PointRange =round(numberofThrees/ (numberofThrees + numberofMidRangeShots + numberofCloseRangeShots),digits=2), PercentageofShotsMidRange = round(numberofMidRangeShots/ (numberofThrees + numberofMidRangeShots + numberofCloseRangeShots),digits=2), PercentageofShotsCloseRange = round(numberofCloseRangeShots/ (numberofThrees + numberofMidRangeShots + numberofCloseRangeShots),digits=2), ThreePointShootingPercentage = round(sum(SHOT_MADE_FLAG == 1& SHOT_TYPE == "3PT Field Goal")/numberofThrees,digits=2), LayupShootingPercentage = round(sum(SHOT_MADE_FLAG == 1 & SHOT_ZONE_BASIC == "Restricted Area")/numberofCloseRangeShots,digits=2),MidRangeShootingPercentage = round(sum(SHOT_MADE_FLAG == 1& SHOT_TYPE == "2PT Field Goal" & SHOT_ZONE_BASIC != "Restricted Area")/numberofMidRangeShots,digits=2)  )
  grouped_distance$NumberofShots = count
  grouped_distance
}
playerDistributionPlot <- function(name, season,filter) {
  name <- str_replace_all(name, fixed(" "), "")
  playerdf <- read.csv(paste0("datasets/players/", name, ".csv"), stringsAsFactors = FALSE)
  playerdf <- filter(playerdf, PERIOD <= 4)
  playerdf <- filter(playerdf, SHOT_ZONE_AREA != "Back Court(BC)")
  if (season != "") {
    seasonStart = paste0(substring(season, 1, 4) , "1011")
    seasonEnd = paste0("20",substring(season, 6, 7) , "0430")
    playerdf <- filter(playerdf, GAME_DATE < as.numeric(seasonEnd) & GAME_DATE > seasonStart)
    
  }
  
  
  ggplot(playerdf, aes(x=as.factor(PERIOD), y = as.numeric(SHOT_DISTANCE))) + geom_violin() + ggtitle(paste(unique(playerdf$PLAYER_NAME), " Shot Distribution by Quarter")) + xlab("Quarter") +ylab("Shot Distance") 
  
  
  
}

pps <- function(name, season, clarity, quarter) {
  playerdf <- getPlayerdf(name, season)
  playerdf$LOC_X <- as.numeric(as.character(playerdf$LOC_X))
  playerdf$LOC_Y <- as.numeric(as.character(playerdf$LOC_Y))
  playerdf$SHOT_TYPE[playerdf$SHOT_TYPE == "2PT Field Goal"] <- 2
  playerdf$SHOT_TYPE[playerdf$SHOT_TYPE == "3PT Field Goal"] <- 3
  playerdf$SHOT_TYPE <- as.numeric(playerdf$SHOT_TYPE) 
  playerdf$EVENT_TYPE[playerdf$EVENT_TYPE == "Missed Shot"] <- 0
  
  playerdf$EVENT_TYPE[playerdf$EVENT_TYPE == "Made Shot"] <- 1
  playerdf$EVENT_TYPE <- as.numeric(playerdf$EVENT_TYPE) 
  playerdf <- mutate(playerdf, Shot_value=SHOT_TYPE * EVENT_TYPE )
  if (quarter != 5) {
    
    playerdf <- filter(playerdf, PERIOD == quarter)
    
  }
  
  xList <- c()
  yList <- c()
  count <- c()
  pps <- c()
  for ( x in seq(250, -250, by = -1 *clarity)) {
    for (y in seq(-50, 420, by = clarity)) {
      
      xList<- c(xList, x)
      yList<- c(yList, y)
      tempPPS <- mean(filter(playerdf, LOC_X >= x - 25 & LOC_X <= x + 25 & LOC_Y >= y - 25 & LOC_Y <= y + 25)$Shot_value )
      if (is.nan(tempPPS) ) {
        tempPPS <- 0
      }
      pps <- c(pps,tempPPS)
      
    }
  }
  
  heat <- data.frame(xList,yList,pps)
  
  courtImg.URL <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
  court <- rasterGrob(readJPEG(getURLContent(courtImg.URL)),
                      width=unit(1,"npc"), height=unit(1,"npc"))
  playerdf
  return(ggplot(heat, aes(x=xList, y=yList)) +
           annotation_custom(court, -250, 250, -50, 420) +
           coord_fixed() +
           
           xlim(250, -250) +
           ylim(-50, 420) +
           
           geom_tile(aes(fill = pps)) +
           scale_fill_viridis_c(option = "B", direction = -1,alpha=.8) +
           ggtitle(paste("Points Per Shot for\n", unique(playerdf$PLAYER_NAME), sep = "")) +
           theme(line = element_blank(),
                 axis.title.x = element_blank(),
                 axis.title.y = element_blank(),
                 axis.text.x = element_blank(),
                 axis.text.y = element_blank(),
                 legend.title = element_blank(),
                 plot.title = element_text(size = 15, lineheight = 0.9, face = "bold"))
  )
}


#' Generate a player dataframe based on name and season
#' 
#' @param name Name of player
#' @param season Season

#' @return A player's dataframe for a given season
#' @examples
#' getPlayerdf("LeBron James, "2018-19")
getPlayerdf <- function(name, season) {
  name <- str_replace_all(name, fixed(" "), "")
  
  playerdf <- read.csv(paste0("datasets/players/", name, ".csv"), stringsAsFactors = FALSE)
  if (season != "") {
    seasonStart = paste0(substring(season, 1, 4) , "1011")
    seasonEnd = paste0("20",substring(season, 6, 7) , "0430")
    playerdf <- filter(playerdf, GAME_DATE < as.numeric(seasonEnd) & GAME_DATE > seasonStart)
    
  }
}

facetTest <- function(teamName,season, filter) {
  teamdf <- read.csv(paste0("datasets/rosters/",teamName,season,".csv"), stringsAsFactors = FALSE)
  gameLogdf <- read.csv(paste0("datasets/gameLogs/NBA",str_replace(season, "-", ""),".csv"), stringsAsFactors = FALSE)
  temp = 1
  if (filter == "W" | filter == "L") {
    gameLogdf <- select(filter(gameLogdf, TEAM_ABBREVIATION == teamName & WL == filter),GAME_ID)
    
    
  }
  if (filter == "3PT under 35") {
    gameLogdf <- select(filter(gameLogdf, TEAM_ABBREVIATION == teamName & as.numeric(FG3_PCT) <=.35),GAME_ID)
    
    
  }
  if (filter == "3PT over 40") {
    gameLogdf <- select(filter(gameLogdf, TEAM_ABBREVIATION == teamName & as.numeric(FG3_PCT) >= .40),GAME_ID)
  }
  for (player in teamdf$PLAYER){
    
    
    if (temp == 1){
      name <- str_replace_all(player, fixed(" "), "")
      playerdf <- read.csv(paste0("datasets/players/",name,".csv"), stringsAsFactors = FALSE)
      seasonStart = paste0(substring(season, 1, 4) ,"1011")
      seasonEnd = paste0("20",substring(season, 6, 7) ,"0430")
      playerdf <- filter(playerdf, GAME_DATE < as.numeric(seasonEnd) & GAME_DATE > seasonStart)
      playerdf <- filter(playerdf, PERIOD <= 4)
      playerdf <- filter(playerdf, SHOT_ZONE_AREA != "Back Court(BC)")
      if (filter == "W" | filter == "L") {
        
        playerdf <- filter(playerdf,GAME_ID %in% gameLogdf[[1]] )
        
      }
      if (filter == "3PT under 35") {
        
        playerdf <- filter(playerdf,GAME_ID %in% gameLogdf[[1]] )
        
      }
      if (filter == "3PT over 40") {
        
        playerdf <- filter(playerdf,GAME_ID %in% gameLogdf[[1]] )
        
      }
      result <- playerdf
      temp = temp + 1
    }
    else {
      name <- str_replace_all(player, fixed(" "), "")
      if (paste0(name,".csv") %in% list.files("datasets/players")) {
        playerdf <- read.csv(paste0("datasets/players/",name,".csv"), stringsAsFactors = FALSE)
        seasonStart = paste0(substring(season,1,4) ,"1011")
        seasonEnd = paste0("20",substring(season,6,7) ,"0430")
        playerdf <- filter(playerdf, GAME_DATE < as.numeric(seasonEnd) & GAME_DATE > seasonStart)
        playerdf <- filter(playerdf, PERIOD <= 4)
        playerdf <- filter(playerdf, SHOT_ZONE_AREA != "Back Court(BC)")
        if (filter == "W" | filter == "L") {
          
          playerdf <- filter(playerdf,GAME_ID %in% gameLogdf[[1]] )
          
        }
        if (filter == "3PT under 35") {
          
          playerdf <- filter(playerdf,GAME_ID %in% gameLogdf[[1]] )
          
        }
        if (filter == "3PT over 40") {
          
          playerdf <- filter(playerdf,GAME_ID %in% gameLogdf[[1]] )
          
        }
        
        temp2 <- playerdf
        result <- rbind(result,temp2)
        
      }
      
    }
    
  }
  ggplot(result, aes(x=as.factor(PERIOD), y = as.numeric(SHOT_DISTANCE),fill = as.factor(PERIOD))) + geom_violin()+facet_wrap(~ PLAYER_NAME) + scale_x_discrete(name = "Quarter") +
    scale_y_continuous(name = "Distrbution of Shots by Distance(ft)")
}



teamHeatMap <- function(teamName,season,clarity,restrictedArea,quarter,filter) {
  courtImg.URL <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
  court <- rasterGrob(readJPEG(getURLContent(courtImg.URL)),
                      width=unit(1,"npc"), height=unit(1, "npc"))
  gameLogdf <- read.csv(paste0("datasets/gameLogs/NBA",str_replace(season, "-", ""),".csv"), stringsAsFactors = FALSE)
  
  teamdf <- read.csv(paste0("datasets/rosters/",teamName,season,".csv"), stringsAsFactors = FALSE)
  if (filter == "W" | filter == "L") {
    gameLogdf <- select(filter(gameLogdf, TEAM_ABBREVIATION == teamName & WL == filter),GAME_ID)
    
    
  }
  if (filter == "3PT under 35") {
    gameLogdf <- select(filter(gameLogdf, TEAM_ABBREVIATION == teamName & as.numeric(FG3_PCT) <=.35),GAME_ID)
    
    
  }
  if (filter == "3PT over 40") {
    gameLogdf <- select(filter(gameLogdf, TEAM_ABBREVIATION == teamName & as.numeric(FG3_PCT) >= .40),GAME_ID)
  }
  temp = 1
  for (player in teamdf$PLAYER){
    if (temp == 1){
      name <- str_replace_all(player, fixed(" "), "")
      playerdf <- read.csv(paste0("datasets/players/",name,".csv"), stringsAsFactors = FALSE)
      seasonStart = paste0(substring(season,1,4) ,"1011")
      seasonEnd = paste0("20",substring(season,6,7) ,"0430")
      playerdf <- filter(playerdf, GAME_DATE < as.numeric(seasonEnd) & GAME_DATE > seasonStart)
      if (filter == "W" | filter == "L") {
        
        playerdf <- filter(playerdf,GAME_ID %in% gameLogdf[[1]] )
        
      }
      if (filter == "3PT under 35") {
        
        playerdf <- filter(playerdf,GAME_ID %in% gameLogdf[[1]] )
        
      }
      if (filter == "3PT over 40") {
        
        playerdf <- filter(playerdf,GAME_ID %in% gameLogdf[[1]] )
        
      }
      result <- playerdf
      temp = temp +1
    }
    else {
      name <- str_replace_all(player, fixed(" "), "")
      if (paste0(name,".csv") %in% list.files("datasets/players")) {
      playerdf <- read.csv(paste0("datasets/players/",name,".csv"), stringsAsFactors = FALSE)
      seasonStart = paste0(substring(season,1,4) ,"1011")
      seasonEnd = paste0("20",substring(season,6,7) ,"0430")
      playerdf <- filter(playerdf, GAME_DATE < as.numeric(seasonEnd) & GAME_DATE > seasonStart)
      if (filter == "W" | filter == "L") {
        
        playerdf <- filter(playerdf,GAME_ID %in% gameLogdf[[1]] )
        
      }
      if (filter == "3PT under 35") {
        
        playerdf <- filter(playerdf,GAME_ID %in% gameLogdf[[1]] )
        
      }
      if (filter == "3PT over 40") {
        
        playerdf <- filter(playerdf,GAME_ID %in% gameLogdf[[1]] )
        
      }
      temp2 <- playerdf
      result <- rbind(result,temp2)
      }
    }
    
  }
  shotDataf <- result
  if (quarter != 5) {
    
    playerdf <- filter(shotDataf, PERIOD == quarter)
    
  }
  if (!restrictedArea) {
    shotDataf <- filter(shotDataf,SHOT_ZONE_BASIC != "Restricted Area")
    
  }
  shotDataf$LOC_X <- as.numeric(as.character(shotDataf$LOC_X))
  shotDataf$LOC_Y <- as.numeric(as.character(shotDataf$LOC_Y))
  xList <- c()
  yList <- c()
  count <- c()
  
  
  for ( x in seq(-250,250,by=clarity)) {
    for (y in seq(-50,420,by=clarity)) {
      
      xList<- c(xList,x)
      yList<- c(yList,y)
      count <- c(count,nrow(filter(shotDataf, LOC_X >= x-25 & LOC_X <= x+25 & LOC_Y >= y-25 & LOC_Y <= y+25)))
    }
  }
  
  
  heat <- data.frame(xList,yList,count)
  
  
  
  return( ggplot(heat, aes(x= xList, y= yList)) +
            annotation_custom(court, -250, 250, -50, 420) +
            xlim(250, -250) +
            ylim(-50, 420) +
            geom_rug(alpha = 0.2) +
            coord_fixed() +
            geom_tile(aes(fill = count)) +
            scale_fill_viridis_c(option = "B", direction = -1,alpha=.7) +
            ggtitle(paste(teamName,"Heat Map for",season , sep = " ")) +
            theme(line = element_blank(),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_blank(),
                  legend.title = element_blank(),
                  plot.title = element_text(size = 15, lineheight = 0.9, face = "bold")))
}
teamDistributionData2 <- function(teamName,season) {
  courtImg.URL <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
  court <- rasterGrob(readJPEG(getURLContent(courtImg.URL)),
                      width=unit(1,"npc"), height=unit(1, "npc"))
  gameLogdf <- read.csv(paste0("datasets/gameLogs/NBA",str_replace(season, "-", ""),".csv"), stringsAsFactors = FALSE)
  
  teamdf <- read.csv(paste0("datasets/rosters/",teamName,season,".csv"), stringsAsFactors = FALSE)
  
  temp = 1
  for (player in teamdf$PLAYER){
    if (temp == 1){
      name <- str_replace_all(player, fixed(" "), "")
      playerdf <- read.csv(paste0("datasets/players/",name,".csv"), stringsAsFactors = FALSE)
      seasonStart = paste0(substring(season,1,4) ,"1011")
      seasonEnd = paste0("20",substring(season,6,7) ,"0430")
      playerdf <- filter(playerdf, GAME_DATE < as.numeric(seasonEnd) & GAME_DATE > seasonStart)
      
      
      result <- playerdf
      temp = temp +1
    }
    else {
      name <- str_replace_all(player, fixed(" "), "")
      if (paste0(name,".csv") %in% list.files("datasets/players")) {
        playerdf <- read.csv(paste0("datasets/players/",name,".csv"), stringsAsFactors = FALSE)
        seasonStart = paste0(substring(season,1,4) ,"1011")
        seasonEnd = paste0("20",substring(season,6,7) ,"0430")
        playerdf <- filter(playerdf, GAME_DATE < as.numeric(seasonEnd) & GAME_DATE > seasonStart)
        
        
        temp2 <- playerdf
        result <- rbind(result,temp2)
      }
    }
    
  }
  grouped_distance <- group_by(result,PERIOD)
  count <- count(grouped_distance, SHOT_ATTEMPTED_FLAG)[["n"]]
  grouped_distance <- summarise(group_by(result, PERIOD), averageShotDistance = mean(SHOT_DISTANCE), numberofCloseRangeShots = sum(SHOT_ZONE_BASIC == "Restricted Area") , numberofMidRangeShots = sum(SHOT_TYPE == "2PT Field Goal" & SHOT_ZONE_BASIC != "Restricted Area"), numberofThrees = sum(SHOT_TYPE == "3PT Field Goal"), PercentageofShots3PointRange =round(numberofThrees/ (numberofThrees + numberofMidRangeShots + numberofCloseRangeShots),digits=2), PercentageofShotsMidRange = round(numberofMidRangeShots/ (numberofThrees + numberofMidRangeShots + numberofCloseRangeShots),digits=2), PercentageofShotsCloseRange = round(numberofCloseRangeShots/ (numberofThrees + numberofMidRangeShots + numberofCloseRangeShots),digits=2), ThreePointShootingPercentage = round(sum(SHOT_MADE_FLAG == 1& SHOT_TYPE == "3PT Field Goal")/numberofThrees,digits=2), LayupShootingPercentage = round(sum(SHOT_MADE_FLAG == 1 & SHOT_ZONE_BASIC == "Restricted Area")/numberofCloseRangeShots,digits=2),MidRangeShootingPercentage = round(sum(SHOT_MADE_FLAG == 1& SHOT_TYPE == "2PT Field Goal" & SHOT_ZONE_BASIC != "Restricted Area")/numberofMidRangeShots,digits=2)  )
  grouped_distance$NumberofShots = count
  
  
  
  return(grouped_distance)
  
  
  
  
}

