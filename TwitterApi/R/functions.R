####################
## IMPORT LIBRARIES
####################

#' @importFrom jsonlite fromJSON
#' @importFrom logging loginfo logerror
#' @importFrom utils URLencode

####################
## AUTHENTICATION
####################

#' @title Authenticate Twitter session using OAuth 2.0
#'
#' @description
#' This function creates a hidden environment that holds the authentication token required for making calls to the Twitter API.
#'
#' @param consumer_api_key String. The consumer api key provided for using your app
#' @param consumer_secret_api_key String. The consumer api secret key provided for using your app
#'
#' @examples
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

twitter_oauth2 <- function(consumer_api_key, consumer_secret_api_key) {

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
#' # (Examples not run due to need for authentication)
#'
#' # result <- generic_api_call(api = 'https://api.twitter.com/labs/1/users', param_list = list(usernames = 'ChelseaFC,ManUtd', format = 'detailed'))
#' # print(cbind(result$data$name, result$data$stats))
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
#' For example, you might want the most recent 1000 requests for a user, but the recent tweets by user API is limited at 200 tweets per request (see \url{https://developer.twitter.com/en/docs/tweets/timelines/api-reference/get-statuses-user_timeline}{get-statuses-user_timeline})
#' To help with this, after the first call is sent, twitter gives back some information that informs what parameter the next api call should use to retrieve the next set (e.g. for tweets 201-400, set the max_id parameter to the minimum id recieved minus 1).
#' This function automatically takes that information sent back, evaluates the operation specified for \code{value_iteration_operation} and applies that into the next API call under the key in \code{key_to_iterate_to}
#'
#' @param api String. The url of the API to call
#' @param param_list List. List of keys and values to provide as parameters to the first API call. See details.
#' @param key_to_iterate_to String. The key of the parameter to assign the new looped value to.
#' @param value_iteration_operation String. The operation to apply to give the value for the iteration operation.
#' @param loops Integer. Number of loops to run before stopping.
#'
#' @return List of looped results from the API call
#'
#' @examples
#' # (Examples not run due to need for authentication)
#'
#' # (Standard): Recent tweets per user (200 per request, but 3200 available to scrape)
#' # api_1 <- 'https://api.twitter.com/1.1/statuses/user_timeline.json'
#' # params_1 <- list(screen_name = 'ChelseaFC', count = 200, tweet_mode = 'extended')
#' # result_1 <-  generic_loop_api_call(key_to_iterate_to = 'max_id', value_iteration_operation = 'min(id)-1', loops = 5, api = api_1, param_list = params_1)
#' # Gets the most recent 1000 tweets from  user "ChelseaFC"
#'
#' # (Premium): Search tweets
#' # api_2 <- 'https://api.twitter.com/1.1/tweets/search/fullarchive/development.json' # note this app already has a sandbox environment set up, called 'development'
#' # params_2 <- list(query = 'from:ChelseaFC', maxResults = 100, toDate = format(Sys.Date(), '%Y%m%d%H%M'), fromDate = format(Sys.Date()-365, '%Y%m%d%H%M'))
#' # result_2 <-  generic_loop_api_call(key_to_iterate_to = 'next', value_iteration_operation = 'min(`next`)', loops = 2, api = api_2, param_list = params_2)
#' # Gets the most relevant 200 tweets when searching using operator "from:ChelseaFC"
#'
#' @details
#' Twitter has specified custom functionality for 'pagination' that this function addresses.
#' See  \url{https://developer.twitter.com/en/docs/tweets/timelines/guides/working-with-timelines}{'working with timelines'} for more details
#'
#' @seealso
#' \url{https://developer.twitter.com/en/docs/tweets/timelines/guides/working-with-timelines}{Twitter pagination}
#' \url{https://developer.twitter.com/en/docs/tweets/timelines/api-reference/get-statuses-user_timeline}{Twitter recent tweets by user}
#' \url{https://developer.twitter.com/en/docs/tweets/search/overview/premium}{Twitter premium search}
#'
#' @keywords twitter api call generic
#'
#' @export

generic_loop_api_call <- function(api = 'https://api.twitter.com/1.1/statuses/user_timeline.json'
                                  , param_list = list(screen_name = 'ChelseaFC'
                                                      , count = 200
                                                      , tweet_mode = 'extended')
                                  , key_to_iterate_to = 'max_id'
                                  , value_iteration_operation = 'min(id)-1'
                                  , loops = 1:16) {
  result_list <- list()
  for(i in 1:loops) {
    result_list[[paste0('loop_',i)]] <- generic_api_call(api = api, param_list = param_list)
    param_list[[key_to_iterate_to]] <- eval(parse(text = paste0("with(result_list[[paste0('loop_',i)]], "
                                                                  , value_iteration_operation
                                                                  , ")")))
    loginfo(paste0('Loop ', i, ' complete'))
  }
  return(result_list)
}

