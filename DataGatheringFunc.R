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


