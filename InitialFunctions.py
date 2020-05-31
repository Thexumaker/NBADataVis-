import requests
import json
url = "https://stats.nba.com/stats/commonallplayers?LeagueID=00&Season=2015-16&IsOnlyCurrentSeason=0"
headers = {
		'Host': 'stats.nba.com',
		'Connection': 'keep-alive',
		'Accept': 'application/json, text/plain, */*',
		'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.130 Safari/537.36',
		'Referer': 'https://stats.nba.com/',
		"x-nba-stats-origin": "stats",
		"x-nba-stats-token": "true",
		'Accept-Encoding': 'gzip, deflate, br',
		'Accept-Language': 'en-US,en;q=0.9',
	}
#Player names
def getNames():
    response = requests.get(url,  headers=headers)
    content = json.loads(response.content)
    #print(len(content['resultSets'][0]['rowSet']))
    playerList = [player for player in content['resultSets'][0]['rowSet'] if int(player[5]) >= 2009]
    return playerList

def seasonPart(season):
    originalSeason = season
    newSeason = 0
    season +=1
    for i in range(2):
        newSeason += (season % 10) * 10** i
        season = season //10
    return "{}-{}".format(originalSeason,newSeason)



#Player shot data
def playerShotData(playerId):
    """Variables: Player ID
    Returns:
    ['GRID_TYPE', 'GAME_ID', 'GAME_EVENT_ID', 'PLAYER_ID', 'PLAYER_NAME', 'TEAM_ID', 'TEAM_NAME',
     'PERIOD', 'MINUTES_REMAINING', 'SECON
    DS_REMAINING', 'EVENT_TYPE', 'ACTION_TYPE', 'SHOT_TYPE', 'SHOT_ZONE_BASIC', 'SHOT_ZONE_AREA',
     'SHOT_ZONE_RANGE', 'SHOT_DISTANCE', '
    LOC_X', 'LOC_Y', 'SHOT_ATTEMPTED_FLAG', 'SHOT_MADE_FLAG', 'GAME_DATE', 'HTM', 'VTM']
    """
    url = "https://stats.nba.com/stats/shotchartdetail?AheadBehind=&ClutchTime=&ContextFilter=\
    &ContextMeasure=FGA&DateFrom=&DateTo=&EndPeriod=&EndRange=&GameID=&GameSegment=&LastNGames=\
    0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&Period=0&PlayerID={}&PlayerPosition=\
    &PointDiff=&Position=&RangeType=&RookieYear=&Season=&SeasonSegment=&SeasonType=Regular+Season&StartPeriod=\
    &StartRange=&TeamID=0&VsConference=&VsDivision=".format(playerId)
    response2 = requests.get(url,  headers=headers)
    print(json.loads(response2.content)['resultSets'][0]['rowSet'][0])

def listofSeasons(start=2009,end=2020):
    listofSeasons = []
    for i in range(start,end):


        listofSeasons.append("{}-{}".format(start,start+1))
    return listofSeasons

def gameLogs(season):
    """Variables:
    Seasons:
    Returns:
    ['SEASON_ID', 'TEAM_ID', 'TEAM_ABBREVIATION', 'TEAM_NAME', 'GAME_ID', 'GAME_DATE', 'MATCHUP',
    'WL', 'MIN', 'FGM', 'FGA', 'FG_PCT',
    'FG3M', 'FG3A', 'FG3_PCT', 'FTM', 'FTA', 'FT_PCT', 'OREB', 'DREB', 'REB',
    'AST', 'STL', 'BLK', 'TOV', 'PF', 'PTS', 'PLUS_MINUS', 'V
    IDEO_AVAILABLE']
    """
    url = "https://stats.nba.com/stats/leaguegamelog?Counter=0&DateFrom=&DateTo=&Direction=ASC&League\
    ID=00&PlayerOrTeam=T&Season={}&SeasonType=Regular+Season&Sorter=DATE".format(season)
    response3 = requests.get(url,  headers=headers)
    print(json.loads(response3.content)['resultSets'][0]['rowSet'][0])
print(seasonPart(2010))
