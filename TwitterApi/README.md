# TwitterApi

This is an R package holding functions to make requests to the twitter API.

To use the twitter api, you will need to a twitter-approved account with an app (that has both an api key and a secret api key)
You can find your approved apps at https://developer.twitter.com/en/apps/ (if you are logged in).

## Installation

To install, use the `remotes` package:

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

