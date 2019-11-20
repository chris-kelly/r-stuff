#
# This is a Shiny web application.
#

library(TwitterApi)
library(shiny)
library(leaflet)
library(plotly)
library(shinyalert)
library(DT)

convert_dates <- function(date) {
    return(as.POSIXct(date, format = '%a %b %e %H:%M:%S %z %Y', ))
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    useShinyalert()

    # Application title
    , titlePanel("Twitter: ChelseaFC")

    # Sidebar for input parameters
    , sidebarLayout(
        sidebarPanel(

            ## AUTHENTICATION
              textInput(inputId = "key"
                        , label = "Consumer API key"
                        , value = ""
                        , width = "100%"
                        , placeholder = "Enter API key")
            , textInput(inputId = "secret"
                        , label = "Consumer Secret API key"
                        , value = ""
                        , width = "100%"
                        , placeholder = "Enter secret API key")
            , actionButton(
                inputId = "authenticate_button"
                , label = 'Authenticate'
                # , icon = icon("fingerprint")
                , width = '50%')
            , hr()

            ## SUMMARY CHANNEL STATS
            , textInput(inputId = "league_table_channels"
                        , label = "Channels to retrieve"
                        , value = "ChelseaFC,ManUtd,ManCity,LFC,SpursOfficial,Arsenal,PSG_English,FCBayernEN,realmadriden,FCBarcelona"
                        , width = "100%"
                        , placeholder = "Channels to scrape")
            , actionButton(
                inputId = "league_table_button"
                , label = 'Make league table'
                # , icon = icon("fingerprint")
                , width = '50%')
            , hr()

            ## LAST 1-3200 tweets
            , textInput(inputId = "recent_tweets_channels"
                        , label = "Channel to scrape"
                        , value = "ChelseaFC"
                        , width = "100%"
                        , placeholder = "Channels to scrape")
            , textInput(inputId = "recent_tweets_count"
                        , label = "# tweets to scrape"
                        , value = "200"
                        , width = "100%"
                        , placeholder = "# tweets to scrape")
            , actionButton(
                inputId = "recent_tweets_button"
                , label = 'Get channel tweets'
                # , icon = icon("fingerprint")
                , width = '50%')
            , hr()

            ## USERS BY RETWEETS
            , textInput(inputId = "retweeters"
                        , label = "Retweeters info"
                        , value = "1193060654174879745"
                        , width = "100%"
                        , placeholder = "Retweeters info")
            , actionButton(
                inputId = "retweeters_button"
                , label = 'Get retweeters'
                # , icon = icon("fingerprint")
                , width = '50%')
            , textInput(inputId = "loc_token"
                        , label = "LocationIQ token"
                        , value = ""
                        , width = "100%"
                        , placeholder = "LocationIQ token")
            , actionButton(
                inputId = "retweeters_loc"
                , label = 'Get locations'
                # , icon = icon("fingerprint")
                , width = '50%')
            , hr()

            ## SEARCH TWEETS BY TERM
            # , textInput(inputId = "tweet_search"
            #             , label = "Search Tweets"
            #             , value = "Pulisic"
            #             , width = "100%"
            #             , placeholder = "Retweeters info")
            # , actionButton(
            #     inputId = "tweet_search_button"
            #     , label = 'Search'
            #     # , icon = icon("fingerprint")
            #     , width = '50%')
            # , hr()
        )
        # Show a plot of the generated distribution
    , mainPanel(
        tabsetPanel(
              tabPanel("League Table"
                       , dataTableOutput("league_table"))
            , tabPanel("Recent Tweets"
                       , plotlyOutput("recent_tweets_graph")
                       , dataTableOutput("recent_tweets_table")) # table in here of tweets and ids
            , tabPanel("Retweeters info"
                       , dataTableOutput("retweeters_DT"))
            , tabPanel("Retweeters locations"
                       , leafletOutput("retweet_map"))
            # , tabPanel("Generic search"
            #            ,)
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    ## RUN AUTHENTICATION ON BUTTON PRESS
    observeEvent(input$authenticate_button, {
        tryCatch({twitter_oauth2(input$key, input$secret); shinyalert('Successfully authenticated Twitter ;)')}
                 , error = function(e) {
                     shinyalert('Error'
                                , 'Twitter authentication failed. Have you input the correct credentials?'
                                , type = "error")
                     }
                 )
        print(.env_twitter$access_token)
    })

    ## CREATE LEAGUE TABLE RESULT
    observeEvent(input$league_table_button, {
        url <- 'https://api.twitter.com/labs/1/users'
        params <- list(usernames = input$league_table_channels, format = 'detailed')
        result <- generic_api_call(api = url, param_list = params)
        result <- data.frame(cbind(result$data$name, result$data$stats))
        result <- result[order(result$followers_count, decreasing = T),]
        names(result[1]) <- 'Channel Name'
        output$league_table <- DT::renderDataTable({result}, rownames= FALSE);
    })

    ## SCRAPE LAST 200 TWEETS
    observeEvent(input$recent_tweets_button, {
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
                                           , value_iteration_operation = 'toString(bit64::as.integer64(min(id)-1))'
                                           , loops = ceiling(recent_tweets_count/200)
                                           , api = api_1
                                           , param_list = params_1)
        recent_tweets <- lapply(result_1, FUN = function(x) x[,c("id_str", "created_at", "favorite_count", "retweet_count", "full_text")])
        recent_tweets <- do.call(rbind, recent_tweets)
        recent_tweets <- recent_tweets[1:input$recent_tweets_count,]
        recent_tweets$created_at_posixct <- sapply(recent_tweets$created_at, FUN = function(x) convert_dates(x))
        class(recent_tweets$created_at_posixct) <- c('POSIXt','POSIXct')
        output$recent_tweets_graph <- renderPlotly({
            plot_ly(type = 'scatter', mode = 'bar') %>%
                add_trace(x = recent_tweets$created_at_posixct
                          , y = recent_tweets$favorite_count
                          , text = recent_tweets$full_text
                          , name = 'favourite_count') %>%
                add_trace(x = recent_tweets$created_at_posixct
                          , y = recent_tweets$retweet_count
                          , text = recent_tweets$full_text
                          , name = 'retweet_count'
                          , yaxis = "y2") %>%
                layout(yaxis2 = list(overlaying = "y", side = "right")
                       , legend = list(orientation = 'h'))
            })
        output$recent_tweets_table <- DT::renderDataTable({recent_tweets[,c('id_str', 'created_at', 'full_text')]}, rownames= FALSE)
    })

    ## SCRAPE USER INFO WHO RETWEETED
    observeEvent(input$retweeters_button, {
        user_ids <- generic_api_call(api = 'https://api.twitter.com/1.1/statuses/retweeters/ids.json'
                                     , param_list = list(id = as.character(input$retweeters)
                                                         , count = 100))
        retweets <- generic_api_call(api = 'https://api.twitter.com/1.1/users/lookup.json'
                                     , param_list = list(user_id = paste0(user_ids$ids, collapse = ',')
                                                         , include_entities = 'true'))
        result <- cbind(retweets[,c("screen_name"
                                    , "location"
                                    , "followers_count"
                                    , "friends_count"
                                    , "favourites_count"
                                    , "statuses_count")]
                        , sapply(retweets$status$text, FUN = function(x) {substr(x,1,50)})
                        )
        output$retweeters_DT <- DT::renderDataTable({result})
    })

    observeEvent(input$retweeters_loc, {
        user_ids <- generic_api_call(api = 'https://api.twitter.com/1.1/statuses/retweeters/ids.json'
                                     , param_list = list(id = bit64::as.integer64(input$retweeters)
                                                         , count = 100))
        retweets <- generic_api_call(api = 'https://api.twitter.com/1.1/users/lookup.json'
                                     , param_list = list(user_id = paste0(user_ids$ids, collapse = ',')
                                                         , include_entities = 'true'))
        result <- cbind(retweets[,c("screen_name"
                                    , "location"
                                    , "followers_count"
                                    , "friends_count"
                                    , "favourites_count"
                                    , "statuses_count")]
                        , sapply(retweets$status$text, FUN = function(x) {substr(x,1,50)})
                        )
        retweet_result <- result
        locations <- retweet_result$location[retweet_result$location != ""]
        coordinates <- tryCatch({geocode_request_lat_lon_loop(locations
                                                              , return_map = T
                                                              , token = input$loc_token);}
                                , error = function(e) {
                                    shinyalert('Error'
                                               , 'LocationIQ API request failed. Have you input the correct credentials?'
                                               , type = "error")
                                    })
        output$retweet_map <- renderLeaflet({coordinates$plot})
        })
}

# Run the application
shinyApp(ui = ui, server = server)
