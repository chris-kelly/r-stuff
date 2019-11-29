####################
## IMPORT LIBRARIES
####################

require(jsonlite)
require(logging)

####################
## DEFINE FUNCTIONS
####################

## AUTHENTICATION

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
  
  ## Create header for requests
  .env_twitter$request_header <- paste0('curl -X GET -H "Authorization: Bearer ', .env_twitter$access_token$access_token, '" "')
  
}

## GENERIC API CALL

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

## LOOP FOR RECENT TWEETS FOR USER/MENTIONS/HOME

max_id_api_call <- function(api = 'https://api.twitter.com/1.1/statuses/user_timeline.json'
                                , param_list = list(screen_name = 'ChelseaFC'
                                                    , count = 200
                                                    , tweet_mode = 'extended')
                                , loops = 1:16) {
  result_list <- list()
  for(i in loops) {
    result_list[[paste0('loop_',i)]] <- generic_api_call(api = api
                                                         , param_list = param_list)
    param_list$max_id <- min(result_list[[paste0('loop_',i)]]$id) - 1
    loginfo(paste0('Loop ', i, ' complete'))
  }
  return(result_list)
}

# LOOP FOR PREMIUM SEARCH QUERY (USING PAGINATION)

pagination_api_call <- function(api = 'https://api.twitter.com/1.1/tweets/search/fullarchive/development.json' # for monthly limits, see https://developer.twitter.com/en/account/subscriptions/
                                , param_list = list(query = 'from:ChelseaFC' # for query types, see https://developer.twitter.com/en/docs/tweets/search/guides/premium-operators
                                                    , maxResults = 100 # default max for sandbox environment
                                                    , toDate = format(Sys.Date(), '%Y%m%d%H%M')
                                                    , fromDate = format(Sys.Date()-365, '%Y%m%d%H%M'))
                                , loops = 1:2) {
  result_list <- list()
  for(i in loops) {
    result_list[[paste0('loop_',i)]] <- generic_api_call(api = api
                                                         , param_list = param_list)
    param_list[['next']] <- min(result_list[[paste0('loop_',i)]][['next']])
    loginfo(paste0('Loop ', i, ' complete'))
  }
  return(result_list)
}

# get_channel_stats
# Returns a variety of information about one or more Users specified
# https://developer.twitter.com/en/docs/labs/tweets-and-users/api-reference/get-users-v1
# 100 users per request
result <- generic_api_call(api = 'https://api.twitter.com/labs/1/users'
                           , param_list = list(usernames = 'ChelseaFC,ManUtd'
                                               , format = 'detailed'))
# LEAGUE TABLE OF RESULTS
cbind(result$data$name, result$data$stats)

# get_twitter_channel_tweets (max num_tweets per request is 200, maxing out at total number of 3200)
# Returns a collection of the most recent Tweets posted by the user
# https://developer.twitter.com/en/docs/tweets/timelines/api-reference/get-statuses-user_timeline
result <- generic_api_call(api = 'https://api.twitter.com/1.1/statuses/user_timeline.json'
                           , param_list = list(screen_name = 'ChelseaFC'
                                               , count = 200
                                               , tweet_mode = 'extended')) 
result <- max_id_api_call(api = 'https://api.twitter.com/1.1/statuses/user_timeline.json'
                          , param_list = list(screen_name = 'ChelseaFC'
                                              , count = 200
                                              , tweet_mode = 'extended') # tweet_mode=extended to prevent tweet truncation
                          , loops = 1:16)

# Returns a collection of relevant Tweets matching a specified query (Standard search api)
result <- generic_api_call(api = 'https://api.twitter.com/1.1/search/tweets.json'
                           , param_list = list(q = '#CFC'
                                               , count = 200
                                               , until = Sys.Date()-7
                                               , tweet_mode = 'extended'))


# Returns a cursored collection of user IDs for every user following the specified user.
result <- generic_api_call(api = 'https://api.twitter.com/1.1/followers/ids.json'
                           , param_list = list(screen_name = 'ChelseaFC'
                                               , cursor = -1
                                               , count = 5000))

# Returns a cursored collection of user IDs for every user the specified user is following (otherwise known as their "friends").
result <- generic_api_call(api = 'https://api.twitter.com/1.1/friends/ids.json'
                           , param_list = list(user_id = '1188535771910885376' # tweet id
                                               , cursor = -1
                                               , count = 5000))

# Returns a collection of up to 100 user IDs belonging to users who have retweeted the Tweet specified by the id parameter.
result <- generic_api_call(api = 'https://api.twitter.com/1.1/statuses/retweeters/ids.json'
                           , param_list = list(id = '327473909412814850' # tweet id
                                               , cursor = -1
                                               , count = 100))


########################################
## EXAMPLE CHELSEAFC TWEET TRACKING
########################################

recent_tweets <- generic_api_call(api = 'https://api.twitter.com/1.1/statuses/user_timeline.json'
                                  , param_list = list(screen_name = 'ChelseaFC'
                                                      , count = 200
                                                      , tweet_mode = 'extended'
                                                      , exclude_replies = 'true'))

convert_dates <- function(date) {
  return(as.POSIXct(date, format = '%a %b %e %H:%M:%S %z %Y', ))
}

recent_tweets$created_at_posixct <- sapply(recent_tweets$created_at, FUN = function(x) convert_dates(x))
class(recent_tweets$created_at_posixct) <- c('POSIXt','POSIXct')

require(plotly)

plot_ly(type = 'scatter', mode = 'bar') %>%
  add_trace(x = recent_tweets$created_at_posixct
            , y = recent_tweets$favorite_count
            , text = recent_tweets$full_text
            , name = 'favorite_count') %>%
  add_trace(x = recent_tweets$created_at_posixct
            , y = recent_tweets$retweet_count
            , text = recent_tweets$full_text
            , name = 'retweet_count'
            , yaxis = "y2") %>%
  layout(yaxis2 = list(overlaying = "y", side = "right")
         , legend = list(orientation = 'h'))


########################################
## EXAMPLE CHELSEAFC FOLLOWER LOCATIONS
########################################

# Returns a cursored collection of user IDs for every user following the specified user.
# Can run 15 per 15 mins

max_iterations = 15+(10^6)/5000

all_chelsea_fc_followers <- list()
cursor <- -1
i <- 1

# while(length(call$ids) > 0) {
for(i in 1:max_iterations) {
  loginfo(paste0('Starting iteration ', i))
  call <- generic_api_call(api = 'https://api.twitter.com/1.1/followers/list.json'
                           , param_list = list(screen_name = 'ChelseaFC'
                                               , cursor = as.character(cursor)
                                               , count = 5000))
  while('errors' %in% names(call)) {
    loginfo('Request limit exceeded, waiting 60s')
    Sys.sleep(60)
    call <- generic_api_call(api = 'https://api.twitter.com/1.1/followers/list.json'
                             , param_list = list(screen_name = 'ChelseaFC'
                                                 , cursor = as.character(cursor)
                                                 , count = 5000))
  }
  all_chelsea_fc_followers[[paste0('cursor_', cursor)]] <- call$users
  saveRDS(object = all_chelsea_fc_followers, file = paste0('followers_so_far_', i, '.rds'))
  cursor <- call$next_cursor_str
  loginfo(paste0('Iteration ', i, ' complete'))
  # i <- i+1
}


a <- unlist(lapply(all_chelsea_fc_followers, FUN = function(x) x['location']))

a <- unlist(all_chelsea_fc_followers)
length(a)

b <- do.call(c, all_chelsea_fc_followers)

tryCatch(expr = a$errors2, error = function(e) {'hello'})

https://api.twitter.com/1.1/followers/list.json


call <- generic_api_call(api = 'https://api.twitter.com/1.1/statuses/user_timeline.json'
                         , param_list = list(user_id = '1134796201281433600'))


cursor <- -1
call <- generic_api_call(api = 'https://api.twitter.com/1.1/followers/list.json'
                         , param_list = list(screen_name = 'ChelseaFC'
                                             , cursor = as.character(cursor)
                                             , count = 5000))

a <- call

exists(x = 'a$errors')

# https://developer.twitter.com/en/docs/tweets/search/api-reference/premium-search#Methods
# https://developer.twitter.com/en/docs/tweets/search/overview/premium#AvailableOperators
result <- generic_api_call(api = 'https://api.twitter.com/1.1/tweets/search/fullarchive/development.json'
                           , param_list = list(query = 'from:ChelseaFC'
                                               , maxResults = 100))
# DATA PAGINATION: result[['next']]

all_twitter_ids <- unlist(unique(do.call(c, all_followers)))

i = 3
# for(i in 1:ceiling(length(all_twitter_ids)/100)) {
  user_ids <- paste0(all_twitter_ids[(i*100-99):(i*100)], collapse = ',')
# }

a <- generic_api_call(api = 'https://api.twitter.com/1.1/users/lookup.json'
                      , param_list = list(user_id = user_ids, include_entities = 'True')
                      )

a$location

tweet_id <- a$id_str[2]

get_retweet_user_details <- function(tweet_id) {
  user_ids <- generic_api_call(api = 'https://api.twitter.com/1.1/statuses/retweeters/ids.json'
                               , param_list = list(id = tweet_id
                                                   , count = 100))
  retweets <- generic_api_call(api = 'https://api.twitter.com/1.1/users/lookup.json'
                               , param_list = list(user_id = paste0(user_ids$ids, collapse = ',')
                                                   , include_entities = 'true'))
  
  
  return(cbind(retweets[,c("screen_name"
                           , "location"
                           , "followers_count"
                           , "friends_count"
                           , "favourites_count"
                           , "statuses_count")]
               , retweets$status$text))
}
# get user ids who liked a certain tweet





# get user ids who liked a certain tweet
b <- generic_api_call(api = 'https://api.twitter.com/1.1/statuses/retweeters/ids.json'
                      , param_list = list(id = a$id_str[1]
                                          , count = 100
                                          , cursor = -1))


a <- generic_api_call(api = 'https://api.twitter.com/1.1/statuses/user_timeline.json'
                                  , param_list = list(screen_name = 'ChelseaFC'
                                                      , count = 200
                                                      , tweet_mode = 'extended'
                                                      , exclude_replies = 'true'))

a[,c('id', "created_at", 'retweet_count', 'favorite_count')] # 'full_text'

1196473642001063936

do.call(rbind, a$entities$hashtags)

b <- lapply(a$entities$hashtags, FUN = function(x) {
  tryCatch(c(unique(x['text']))
           , error = fun(e) {data.frame('')})
  })

a <- generic_api_call(api = 'https://api.twitter.com/1.1/statuses/retweets/:id.json'
                      , param_list = list(screen_name = 'ChelseaFC'
                                          , count = 200
                                          , tweet_mode = 'extended'
                                          , exclude_replies = 'true'))


require(TwitterApi)
api_1 <- 'https://api.twitter.com/1.1/statuses/user_timeline.json'
params_1 <- list(screen_name = 'ChelseaFC', count = 200, tweet_mode = 'extended')
result_1 <-  generic_loop_api_call(key_to_iterate_to = 'max_id'
                                   , value_iteration_operation = 'min(id)-1'
                                   , loops = 1
                                   , api = api_1
                                   , param_list = params_1)


recent_tweets_count <- as.numeric(input$recent_tweets_count)
# recent_tweets <- generic_api_call(api = 'https://api.twitter.com/1.1/statuses/user_timeline.json'
#                                   , param_list = list(screen_name = input$recent_tweets_channels
#                                                       , count = 200
#                                                       , tweet_mode = 'extended'
#                                                       , exclude_replies = 'true'))
api_1 <- 'https://api.twitter.com/1.1/statuses/user_timeline.json'
params_1 <- list(screen_name = input$recent_tweets_channels
                 , count = ceiling(recent_tweets_count/ceiling(recent_tweets_count/200))
                 , tweet_mode = 'extended'
                 , exclude_replies = 'true')
result_1 <-  generic_loop_api_call(key_to_iterate_to = 'max_id'
                                   , value_iteration_operation = 'min(id)-1'
                                   , loops = ceiling(recent_tweets_count/200)
                                   , api = api_1
                                   , param_list = params_1)
recent_tweets <- lapply(result_1, FUN = function(x) x[,c("id_str", "created_at", "favorite_count", "retweet_count", "full_text")])
recent_tweets <- do.call(rbind, recent_tweets)
recent_tweets <- recent_tweets[1:input$recent_tweets_count,]
recent_tweets$created_at_posixct <- sapply(recent_tweets$created_at, FUN = function(x) convert_dates(x))
class(recent_tweets$created_at_posixct) <- c('POSIXt','POSIXct')

result <- list()
recent_tweets$id_str

for(i in recent_tweets$id_str[1:10]) {
  user_ids <- generic_api_call(api = 'https://api.twitter.com/1.1/statuses/retweeters/ids.json'
                               , param_list = list(id = i
                                                   , count = 100))
  retweets <- generic_api_call(api = 'https://api.twitter.com/1.1/users/lookup.json'
                               , param_list = list(user_id = paste0(user_ids$ids, collapse = ',')
                                                   , include_entities = 'true'))
  result[[paste0('t_', i)]] <- cbind(retweets[,c("screen_name"
                                                 , "location"
                                                 , "followers_count"
                                                 , "friends_count"
                                                 , "favourites_count"
                                                 , "statuses_count")]
                                     , sapply(retweets$status$text, FUN = function(x) {substr(x,1,50)})
                                     )
}

all_results <- lapply(result, FUN = function(x) {x['location']})
all_results <- unlist(do.call(rbind,all_results))

geocode_request <- function(string = 'Lagos, Nigeria', token = '9098dba1ee9a41') {
  response <- system(paste0("curl --request GET   --url 'https://us1.locationiq.com/v1/search.php?"
                            , "key="
                            , token
                            , "&q=", URLencode(string)
                            , "&format=json'")
                     , intern = T)
  response <- fromJSON(response)
  return(response)
}

all_results <- data.frame(all_results)

for (i in unique(all_results[all_results != ""])) {
  all_results$geocode <- 
  geocode_request(i)
}

table(all_results[all_results != ""])

geocodes <- sapply(all_results[all_results != ""], FUN = function(x) tryCatch(geocode_request(x)[1,c('lat', 'lon')]
                                                                              , error = function(e) {
                                                                                data.frame(lat = '', long = '')
                                                                                # Sys.sleep(5); geocode_request(x)[1,c('lat', 'lon')]
                                                                                })
                   )

lat <- unlist(sapply(1:length(geocodes)/2, FUN = function(i) geocodes[,i]$lat))
lon <- unlist(sapply(1:length(geocodes)/2, FUN = function(i) geocodes[,i]$lon))

lat_lon <- data.frame(Lat = as.numeric(lat)
                      , Long = as.numeric(lon))

data(quakes)



leaflet_plot <- 
# Show first 20 rows from the `quakes` dataset
leaflet(data = lat_lon) %>% addTiles() %>% addMarkers(
  clusterOptions = markerClusterOptions()
)

saveRDS(object = leaflet_plot, file = 'leaflet_plot.rds')

  # addMarkers(~Long, ~Lat)

library(maps)
mapStates = map("state", fill = TRUE, plot = FALSE)
leaflet(data = mapStates) %>% addTiles() %>%
  addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)

leaflet(data = ) %>% addCircles()

geocodes[1,]

curl --request GET   --url 'https://us1.locationiq.com/v1/search.php?key=9098dba1ee9a41&q=Empire%20State%20Building&format=json'


