####################
## IMPORT LIBRARIES
####################

#' @importFrom jsonlite fromJSON
#' @importFrom logging loginfo logerror
#' @importFrom utils URLencode
#' @importFrom bit64 as.integer64
#' @importFrom leaflet leaflet addTiles addMarkers markerClusterOptions

####################
## AUTHENTICATION
####################

#' @title Authenticate Twitter session using OAuth 2.0
#'
#' @description
#' This function creates a hidden environment that holds the authentication token required for making calls to the Twitter API.
#'
#' @param consumer_api_key String. The consumer api key provided for using your app. Default = Sys.getenv('TWITTER_CONSUMER_API_KEY')
#' @param consumer_secret_api_key String. The consumer api secret key provided for using your app. Default = Sys.getenv('TWITTER_CONSUMER_SECRET_API_KEY')
#'
#' @examples
#' twitter_oauth2() # by default,
#' # twitter_oauth2('<YOUR API KEY>', '<YOUR SECRET API KEY>') # Not run - input your own api keys
#'
#' @details
#' To use the twitter api, you will need to an approved account with an app (that has both an api key and a secret api key)
#' You can find your approved apps at \url{https://developer.twitter.com/en/apps/} (if logged in)
#' This function is called when running other functions (such as \code{\link{generic_api_call}})
#'
#' @keywords twitter api authenticate
#'
#' @seealso \url{https://stackoverflow.com/a/32534239}
#'
#' @export

twitter_oauth2 <- function(consumer_api_key = Sys.getenv('TWITTER_CONSUMER_API_KEY')
                           , consumer_secret_api_key = Sys.getenv('TWITTER_CONSUMER_SECRET_API_KEY')) {

  ## Check if hidden twitter environment exists
  if(!(exists('.env_twitter') & is.character(consumer_api_key) & is.character(consumer_secret_api_key))) {
    loginfo('Making first authentication in this session: building hidden environment')
    .env_twitter <<- new.env(parent=baseenv())
  }

  ## Retrieve access token
  .env_twitter$access_token <- fromJSON(system(paste0("curl -u '"
                                                      , consumer_api_key, ":", consumer_secret_api_key
                                                      , "' --data 'grant_type=client_credentials' 'https://api.twitter.com/oauth2/token'")
                                               , intern = T))
  if('errors' %in% names(.env_twitter$access_token)) {
    logerror('Authentication failed. Have you input the correct api and secret api key from your app (https://developer.twitter.com/en/apps/)')
    stop()
  } else if ('access_token' %in% names(.env_twitter$access_token)) {
    ## Create header for requests
    .env_twitter$request_header <- paste0('curl -X GET -H "Authorization: Bearer ', .env_twitter$access_token$access_token, '" "')
    loginfo('Successful authentication')
  } else {
    logerror('Unknown error')
    stop()
  }
}

####################
## GENERIC TWITTER API CALL
####################

#' @title Generic API call to Twitter
#'
#' @description
#' This function is a generic wrapper for API calls to twitter (any url and list of specified parameters to send to the API)
#'
#' @param api String. The url of the API to call
#' @param param_list List. List of keys and values to provide as parameters to the API call. See details.
#'
#' @return List of results from the API call
#'
#' @examples
#'
#' twitter_oauth2() # one-time run for authentication
#' result <- generic_api_call(api = 'https://api.twitter.com/labs/1/users', param_list = list(usernames = 'ChelseaFC,ManUtd', format = 'detailed'))
#' print(cbind(result$data$name, result$data$stats))
#'
#' # makes the call 'curl -X GET -H "Authorization: Bearer <BEARER_TOKEN>" "https://api.twitter.com/labs/1/users?usernames=ChelseaFC,ManUtd&format=detailed"'
#' # parameter options can be found at https://developer.twitter.com/en/docs/labs/tweets-and-users/api-reference/get-users-v1
#'
#' @details
#' This function can take an api and list of parameters for use in making API calls to Twitter.
#' The url used for the api argument will depend on the type of information the user wants to recieve
#' For example for tweet times, see https://developer.twitter.com/en/docs/tweets/timelines/overview
#' The param_list is a list of the specified keys and values the user wants to specify when making the the API call.
#' This function requires \code{\link{twitter_oauth2}} to be run first.
#'
#' @keywords twitter api call generic
#'
#' @export

generic_api_call <- function(api = 'https://api.twitter.com/labs/1/users'
                             , param_list = list(usernames = 'ChelseaFC'
                                                 , format = 'detailed')) {
  if(!(exists('.env_twitter'))) {
    logerror('Not authenticated - run twitter_oauth2() with valid credentials.')
    stop()
  }
  params <- lapply(param_list, FUN = function(param) gsub(pattern = '#', replacement = '%23', URLencode(as.character(param))))
  result <- fromJSON(system(paste0(.env_twitter$request_header
                                   , api, '?'
                                   , paste0(paste0(names(params), '=', params), collapse = '&')
                                   , '"')
                            , intern = T))
  return(result)
}

####################
## GENERIC TWITTER API LOOP
####################

#' @title Generic looped API calls to Twitter
#'
#' @description
#' Twitter has request limits for each API call.
#' For example, you might want the most recent 1000 requests for a user, but the recent tweets by user API is limited at 200 tweets per request (see \href{https://developer.twitter.com/en/docs/tweets/timelines/api-reference/get-statuses-user_timeline}{get-statuses-user_timeline})
#' To help with this, after the first call is sent, twitter gives back some information that informs what parameter the next api call should use to retrieve the next set (e.g. for tweets 201-400, take the the minimum id recieved in the previous call minus one, and set the max_id parameter to that).
#' This function automatically takes that information sent back, evaluates the operation specified for \code{value_iteration_operation} and applies that into the next API call under the key in \code{key_to_iterate_to}
#'
#' @param api String. The url of the API to call
#' @param param_list List. List of keys and values to provide as parameters to the first API call. See details.
#' @param key_to_iterate_to String. The key of the parameter to assign the new looped value to.
#' @param value_iteration_operation String. The operation to apply to give the value for the iteration operation.
#' @param retry_sleep Number. The amount of time to wait before retrying call if errors found when calling (due to request wait limits). Default = NA (do not retry call)
#' @param max_retries Number of times to retry the API call after waiting before stopping
#' @param loops Integer. Number of loops to run before stopping.
#'
#' @return List of looped results from the API call
#'
#' @examples
#' # (Examples not run due to need for authentication)
#' twitter_oauth2() # one-time run for authentication
#'
#' # (Standard): Recent tweets per user (200 per request, but 3200 available to scrape)
#' api_1 <- 'https://api.twitter.com/1.1/statuses/user_timeline.json'
#' params_1 <- list(screen_name = 'ChelseaFC', count = 200, tweet_mode = 'extended')
#' result_1 <-  generic_loop_api_call(key_to_iterate_to = 'max_id', value_iteration_operation = 'toString(as.integer64(min(id)-1))', loops = 5, api = api_1, param_list = params_1)
#' # Gets the most recent 1000 tweets from  user "ChelseaFC"
#'
#' # (Premium): Search tweets
#' api_2 <- 'https://api.twitter.com/1.1/tweets/search/fullarchive/development.json' # note this app already has a sandbox environment set up, called 'development'
#' params_2 <- list(query = 'from:ChelseaFC', maxResults = 100, toDate = format(Sys.Date(), '%Y%m%d%H%M'), fromDate = format(Sys.Date()-365, '%Y%m%d%H%M'))
#' result_2 <-  generic_loop_api_call(key_to_iterate_to = 'next', value_iteration_operation = 'min(`next`)', loops = 2, api = api_2, param_list = params_2)
#' # Gets the most relevant 200 tweets when searching using operator "from:ChelseaFC"
#'
#' @details
#' Twitter has specified custom functionality for 'pagination' that this function addresses.
#' See \href{https://developer.twitter.com/en/docs/tweets/timelines/guides/working-with-timelines}{'working with timelines'} for more details
#'
#' @seealso
#' \href{https://developer.twitter.com/en/docs/tweets/timelines/guides/working-with-timelines}{Twitter pagination}
#' \href{https://developer.twitter.com/en/docs/tweets/timelines/api-reference/get-statuses-user_timeline}{Twitter recent tweets by user}
#' \href{https://developer.twitter.com/en/docs/tweets/search/overview/premium}{Twitter premium search}
#'
#' @keywords twitter api call generic
#'
#' @export

generic_loop_api_call <- function(api = 'https://api.twitter.com/1.1/statuses/user_timeline.json'
                                  , param_list = list(screen_name = 'ChelseaFC'
                                                      , count = 200
                                                      , tweet_mode = 'extended')
                                  , key_to_iterate_to = 'max_id'
                                  , value_iteration_operation = 'toString(bit64::as.integer64(min(id)-1))'
                                  , retry_sleep = NA
                                  , max_retries = 15
                                  , loops = 5) {
  result_list <- list()
  for(i in 1:loops) {
    result <- generic_api_call(api = api, param_list = param_list)
    if(!is.na(retry_sleep)) {
      j <- 1
      while('errors' %in% names(call) & j <= max_retries) {
        loginfo('Request limit exceeded, waiting 60s')
        Sys.sleep(retry_sleep)
        result <- generic_api_call(api = api, param_list = param_list)
        j <- j+1
      }
    }
    result_list[[paste0('loop_',i)]] <- result
    param_list[[key_to_iterate_to]] <- eval(parse(text = paste0("with(result_list[[paste0('loop_',i)]], "
                                                                  , value_iteration_operation
                                                                  , ")")))
    loginfo(paste0('Loop ', i, ' complete'))
  }
  return(result_list)
}

####################
## GENERIC LOCATION RETRIEVAL
####################

#' @title Find geo-locations searched from a string
#'
#' @description
#' This uses the free api from locationIQ. It is free to get a token and make a call to this API without the need for approval.
#' (Note the rate limit for this API is 2 seconds)
#'
#' @param string String. The string to search in the API. Default = "Lagos, Nigeria"
#' @param token String. The token to use to make the API call. Default = Sys.getenv('TWITTER_CONSUMER_API_KEY')
#'
#' @return Dataframe of the response from the API.
#'
#' @examples
#' response <- geocode_request('Big Ben, London')
#'
#' @details
#' Most twitter users haven't opted in to using location data for their tweets, but many have location descriptions.
#' This function is a helper to scrape those locations.
#'
#' @seealso
#' Visit \href{https://locationiq.com/register}{LocationIQ website} to sign up for a free token.
#'
#' @keywords twitter api call generic
#'
#' @export
#'

geocode_request <- function(string = 'Lagos, Nigeria', token = Sys.getenv('LOCATIONIQ_TOKEN')) {
  if(token == "") {
    logerror('Token is missing, cannot make call to API.')
  } else {
    response <- system(paste0("curl --request GET   --url 'https://us1.locationiq.com/v1/search.php?"
                              , "key="
                              , token
                              , "&q=", URLencode(string)
                              , "&format=json'")
                       , intern = T)
    response <- fromJSON(response)
    return(response)
  }
}

####################
## GENERIC GEOCODE REQUEST LOOP
####################

#' @title Find multiple geo-locations from a string
#'
#' @description
#' This uses the free api from locationIQ. Note the rate limit for this API is 2 requests per second.
#' See \code{\link{geocode_request}}) for more details.
#'
#' @param locations Vector of strings. The strings to search in the API. Default = c('London','Manchester')
#' @param return_map Boolean. Whether to return a map of the collected coordinates or not
#'
#' @return Dataframe of the response from the API.
#'
#' @examples
#' twitter_oauth2()
#' param_list = list(usernames = 'ChelseaFC,ManUtd,LFC,SpursOfficial,Arsenal', format = 'detailed')
#' result <- generic_api_call(api = 'https://api.twitter.com/labs/1/users', param_list = param_list)
#' coordinates <- geocode_request_lat_lon_loop(locations = result$data$location, return_map = TRUE)
#' coordinates$plot # run to see the the plotted results
#'
#' @details
#' Most twitter users haven't opted in to using location data for their tweets, but many have location descriptions.
#' This function is a helper to locate those users.
#' Optionally, the function can plot those locations on a map.
#'
#' @keywords twitter api call generic
#'
#' @export

geocode_request_lat_lon_loop <- function(locations = c('London', 'Manchester')
                                         , return_map = TRUE
                                         , token = Sys.getenv('LOCATIONIQ_TOKEN')) {
  result <- sapply(locations
                   , FUN = function(x) tryCatch(geocode_request(x,token)[1,c('lat', 'lon')]
                                                , error = function(e) {
                                                  tryCatch({Sys.sleep(0.5); geocode_request(x,token)[1,c('lat', 'lon')]}
                                                           , error = function(e) {data.frame(lat = '', long = '')})
                                                  })
                   )
  lat <- unlist(result[1,]); lon <- unlist(result[2,])
  lat_lon <- data.frame(Lat = as.numeric(lat), Long = as.numeric(lon), search = locations)
  if(return_map) {
    leaflet_plot <- leaflet(data = lat_lon) %>%
      addTiles() %>%
      addMarkers(clusterOptions = markerClusterOptions(), label = ~locations)
    return(list(coordinates = lat_lon, plot = leaflet_plot))
  } else {
    return(list(coordinates = lat_lon))
  }
}

##########################################
## Run Shiny App
##########################################

#' @title Twitter scraping shiny app
#'
#' @description
#' This function runs an example shiny app for twitter scraping.
#'
#' @param port Integer: Port to run the shiny app through. Defaults to 5000.
#' @param host String: IP address to host the shiny app. Defaults to 0.0.0.0.
#'
#' @keywords shiny twitter
#'
#' @export
#'

shinyTwitterApp <- function(port = 5000, host = "127.0.0.1") {
  # return(list.files('inst'))
  appDir <- system.file('shiny_app.R', package = "TwitterApi")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `TwitterApi`.", call. = FALSE)
  }
  shiny::runApp(appDir, port = port, host = host, display.mode = "normal")
}

