# TwitterApi

This is an R package holding functions to make requests to the twitter API.

To use the twitter api, you will need a twitter-approved account with an app (that has both an api key and a secret api key). You can find your approved apps at https://developer.twitter.com/en/apps/ (if you are logged in).

## Installation

To install from github, use the `remotes` package:

```
remotes::install_github('chris-kelly/r-stuff/TwitterApi')
```

## Authentication

To first authenticate the session, use:

```
twitter_oauth2()
```

By default, it uses the environment variables `TWITTER_CONSUMER_API_KEY` and `TWITTER_CONSUMER_SECRET_API_KEY` for authentication.

## API call

Once the R session is authenticated, you can make generic API calls to Twitter using `generic_api_call()`. For example:

```
url <- 'https://api.twitter.com/labs/1/users'
params <- list(usernames = 'ChelseaFC,ManUtd', format = 'detailed')
result <- generic_api_call(api = url, param_list = params)
print(cbind(result$data$name, result$data$stats))

# This returns the following:
#    result$data$name followers_count following_count tweet_count listed_count
# 1        Chelsea FC        13560848             258       91650        23624
# 2 Manchester United        20529473             127       55902        21979
```

## Looping API call

Twitter has request limits for each API call. For example, you might want the most recent 1000 requests for a user, but the recent tweets by user API is limited at 200 tweets per request (see get-statuses-user_timeline).

To help with this, after the first call is sent, twitter gives back some information that informs what parameter the next api call should use to retrieve the next set (e.g. for tweets 201-400, take the the minimum id recieved in the previous call minus one, and set the max_id parameter to that). 

This function automatically takes that information sent back, evaluates the operation specified under `value_iteration_operation` and applies that into the next API call under the key `key_to_iterate_to`

```
# Gets the most recent 1000 tweets from  user "ChelseaFC"
api_1 <- 'https://api.twitter.com/1.1/statuses/user_timeline.json'
params_1 <- list(screen_name = 'ChelseaFC', count = 200, tweet_mode = 'extended')
result_1 <-  generic_loop_api_call(key_to_iterate_to = 'max_id'
                                   , value_iteration_operation = 'min(id)-1'
                                   , loops = 5
                                   , api = api_1
                                   , param_list = params_1)
```

## Geocoding

Most users do not attach geo-coordinates to their tweets/have not opted in the provide geo-location data.
Some user do have a 'location' in their description field though, although this is free text and not very useful.
To help combat this, the package can make API calls to <a href="https://locationiq.com/">LocationIQ</a> (a free service to register a token for).
As a bonus, a map of all the geo-locations obtained can also be plotted using the <a href="https://rstudio.github.io/leaflet/map_widget.html">leaflet package</a> using this function.

```
# simple example
example_1 <- geocode_request(string = "Lagos, Nigeria")

# map plot of multiple locations
param_list = list(usernames = 'ChelseaFC,ManUtd,LFC,SpursOfficial,Arsenal', format = 'detailed')
result <- generic_api_call(api = 'https://api.twitter.com/labs/1/users', param_list = param_list)
coordinates <- geocode_request_lat_lon_loop(locations = result$data$location, return_map = TRUE)
coordinates$plot # run to see the the plotted results
```

