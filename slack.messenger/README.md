# slack.messenger

Send messages via incoming webhook to slack users or channels

Once you have your webhook set up on slack (<a href="https://slack.com/intl/en-gb/help/articles/115005265063-incoming-webhooks-for-slack">see this link"</a>), writing messages from R is as easy as: 

```
write_to_slack(param_list = list(text = 'Hello world', channel = '#slackR'))
```
<img src="https://raw.githubusercontent.com/chris-kelly/r-stuff/master/slack.messenger/inst/readme_pics/Picture_3.png"></img>

(By default, the `write_to_slack()` function uses the URL loaded as the environment variable `SLACK_WEBHOOK_URL`, but you can specify this manually in the function)

The parameter list is flexible, and can accomdate attachments too, e.g.:

```
attachments <- list(fallback = paste0("Example link: ", link(text_to_display = "Click here", link = 'https://google.co.uk'))
                     , pretext = paste0("Example link: ", link(text_to_display = "Click here", link = 'https://google.co.uk'))
                     , color = "#d12626"
                     , footer = "Attachment footer"
                     , fields = list(
                         list(
                           title = "Title of the attachment"
                           , value = "Text in the attachment"
                           , short = "false"
                         )
                       )
                     )
param_list <- list(text = 'The text to display in the body of the message'
                    , username = 'The name to give the "bot" that sends the message'
                    , channel = "@chris.kelly"
                    , icon_emoji = ':ghost:'
                    , attachments = list(attachments))
write_to_slack(param_list = param_list)
```

<img src="https://raw.githubusercontent.com/chris-kelly/r-stuff/master/slack.messenger/inst/readme_pics/Picture_2.png"></img>

As well as the `link()` function in the example above, this package also has a helper function `capture_r_output` to capture any r output printed to the console as convert to slack message format:

```
sum_lm <- summary(lm(dist ~ speed, data=cars))
sum_lm <- capture_r_output(sum_lm)
param_list <- list(username = 'Name of bot', channel = '@chris.kelly', icon_emoji = ':call_me_hand:')
param_list$text <- sum_lm
write_to_slack(param_list = param_list)
```

<img src="https://raw.githubusercontent.com/chris-kelly/r-stuff/master/slack.messenger/inst/readme_pics/Picture_4.png"></img>
