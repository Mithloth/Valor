library(jsonlite)
library(httr)

#Set some global parameters
default_key <- ""
#Current rate limit is 10 requests per 10 seconds & 500 requests per 10 minutes
rate_limit <- 500/600

rest <- FALSE

Valor_CallResults <- function(api_call) {
     #This function makes an API call and checks the header returned by any given call for validity/errors
     #Once it clears all errors, it will return the contents of the call
     #In addition, it suspends the system for long enough to ensure that the rate limit is not exceeded
     
     #Current header codes:
     error_200 <- 'No error' #Continue as normal
     error_400 <- 'Bad request' #Call problem
     error_422 <- 'Summoner has entry, but hasn\'t played since the start of 2013' #Should forget this guy and move on
     error_500 <- 'Internal server error' #Try again after a second
     error_429 <- 'Rate limit exceeded' #Check how long we need to wait and wait that long
     error_503 <- 'Service unavailable' #No documentation here? Probably want to terminate and return an error
     error_401 <- 'Unauthorized' #Probably a problem with the key. Terminate and return error
     error_404 <- 'Game not found' #Problem with the call; skip and move on
     
     #Loop until we say stop
     try_again <- TRUE
     while (try_again == TRUE) {
          results <- GET(api_call)
          header <- results$status_code
          
          #If no error, wait the average time and return the call results
          if (header == 200) {
               Sys.sleep(rate_limit)
               return(results)
          }
          #On error 500, wait a second
          else if (header == 500) {
               print(error_500)
               Sys.sleep(1)
               try_again <- TRUE
          }
          #On error 429, wait the required time
          else if (header == 429) {
               print(error_429)
               wait <- ValorCallResults$headers$Retry_After
               Sys.sleep(wait)
               try_again <- TRUE
          }
          #On error 400, stop calling
          else if (header == 400){
               print(error_400)
               try_again <- FALSE
          }
          #On error 422, stop calling
          else if (header == 422){
               print(error_422)
               try_again <- FALSE
          }
          #On error 503, stop calling
          else if (header == 503){
               print(error_503)
               try_again <- FALSE
          }
          #On error 401, stop calling
          else if (header == 401){
               print(error_401)
               try_again <- FALSE
          }
          #On error 404, stop calling
          else if (header == 404){
               print(error_404)
               try_again <- FALSE
          }
     }
     
}

Valor_SummonerByName <- function(summonernames,api_key = default_key,test = rest) { 
     #This function calls the Riot API to get summoner objects mapped by standardized summoner name 
     #for a given list of summoner names
     
     #summonernames is a vector containing all of the names for summoners to be retrieved
     #api_key is a character vector containing the developer's API key
     
     #Initialize the string of summoner names to be passed into the API call
     summonerstring <- summonernames[1]
     
     #If we only have one summoner, skip the loop
     if (length(summonerstring) > 1) {
     #Append all of the summonerids into one string that can be recognized by the API
          for (i in 2:length(summonernames)){
               summonerstring <- paste0(summonerstring,",%20",summonernames[i])
          }
     }
     
     api_call <- paste0("https://na1.api.riotgames.com/lol/summoner/v3/summoners/by-name/",summonerstring,"?api_key=",api_key)
     gottendata <- Valor_CallResults(api_call)
     return(gottendata)
     
}

Valor_MatchList <- function(summonerid,api_key=default_key,begin_time = 1481108400000) { 
     #This function calls the Riot API to get the matches for a particular summoner id
     #As of 04/23/2017 the season functionality was bugged, so the default value for "begin_time" is the start of season 7
     #Use a begin_time of 0 for a complete match history
     
     api_call <- paste0("https://na.api.riotgames.com/api/lol/NA/v2.2/matchlist/by-summoner/",summonerid,"?beginTime=",begin_time
                              ,"&api_key=",api_key)
     gottendata <- Valor_CallResults(api_call)
     return(gottendata)
     
}

Valor_MatchInfo <- function(matchid,api_key=default_key) { 
     #This function calls the Riot API to get the match info for a particular match id
     
     api_call <- paste0("https://na.api.riotgames.com/api/lol/NA/v2.2/match/",matchid
                        ,"?api_key=",api_key)
     gottendata <- Valor_CallResults(api_call)
     return(gottendata)
     
}