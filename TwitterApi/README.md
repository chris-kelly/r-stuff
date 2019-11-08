# TwitterApi

This is an R package holding functions to make requests to the twitter API.

To use the twitter api, you will need to a twitter-approved account with an app (that has both an api key and a secret api key)
You can find your approved apps at https://developer.twitter.com/en/apps/ (if you are logged in).

To install, use the `remotes` package:

```remotes::install_github('chris-kelly/r-stuff/TwitterApi')```

To first authenticate the session, use:

```twitter_oauth2()```

By default, it calls the environment variables `TWITTER_CONSUMER_API_KEY` and `TWITTER_CONSUMER_SECRET_API_KEY`.
