########################################
## Load libraries
########################################

#' @importFrom httr POST
#' @importFrom jsonlite toJSON
#' @importFrom utils capture.output

########################################
## Create link
########################################

#' @title Encode text as link
#'
#' @description Shortcut to write syntax for encoding text as link
#'
#' @param text_to_display The text to display in the message
#' @param link The link to direct the user to
#'
#' @examples
#' paste0('Hi there! '
#' , link('Check out our link', 'https://google.co.uk'))
#' # returns [1] "Hi there! <https://google.co.uk|Check out our link>"
#'
#' @return A string that is compatible with writing links in slack messages
#' @keywords slack
#'
#' @export

link <- function(text_to_display = 'Click here', link = 'https://google.co.uk') {
  return(paste0("<", link, "|", text_to_display, ">"))
}

########################################
## Create payload
########################################

#' @title Helper function to write payload to slack message
#' @param param_list list of keys and values to create json payload
#' @export

payload <- function(param_list) {
  return(
    toJSON(
      param_list
      , auto_unbox = T, pretty = T)
  )
}

########################################
## Write slack message to individual or channel
########################################

#' @title Encode text as link
#'
#' @description Shortcut to write syntax for encoding text as link
#'
#' @param param_list List: List of keys and values to feed into the json payload to be
#' @param url String: The url of the webhook. By default, takes the SLACK_WEBHOOK_URL environment variable
#'
#' @examples
#' attachments <- list(fallback = paste0("Example link: ", link(text_to_display = "Click here", link = 'https://google.co.uk'))
#'                     , pretext = paste0("Example link: ", link(text_to_display = "Click here", link = 'https://google.co.uk'))
#'                     , color = "#d12626"
#'                     , footer = "Attachment footer"
#'                     , fields = list(
#'                         list(
#'                           title = "Title of the attachment"
#'                           , value = "Text in the attachment"
#'                           , short = "false"
#'                         )
#'                       )
#'                     )
#' param_list <- list(text = 'The text to display in the body of the message'
#'                    , username = 'The name to give the "bot" that sends the message'
#'                    , channel = "@chris.kelly"
#'                    , icon_emoji = ':ghost:'
#'                    , attachments = list(attachments))
#' write_to_slack(param_list = param_list)
#'
#' @return A string that is compatible with writing links in slack messages
#' @keywords slack
#'
#' @seealso \href{https://api.slack.com/docs/messages/builder}{Sdlack message builder}
#'
#' @export

write_to_slack <- function(url = Sys.getenv('SLACK_WEBHOOK_URL')
                           , param_list = list(channel = "@chris.kelly", text = 'Hello world')) {
  httr::POST(url = url, body = payload(param_list))
}

########################################
## Send nicely formatted outputs in slack
########################################

#' @title Capture nicely formatted R outputs to send in slack
#'
#' @description Function to capture R outputs to send them as slack messages
#'
#' @param print_object An object that has a print method() associated with it
#'
#' @examples
#' df <- data.frame(a = 1:5, d = 96:100)
#' print_df <- capture_r_output(df)
#' param_list <- list(username = 'Name of bot', channel = '@chris.kelly', icon_emoji = ':call_me_hand:')
#' param_list$text <- print_df
#' write_to_slack(param_list = param_list)
#'
#' @return A string that is compatible with sending R outputs in slack messages
#' @keywords slack
#'
#' @export

capture_r_output <- function(print_object) {
  print_df <- capture.output(print(print_object))
  print_df <- paste0(print_df, collapse = '\n')
  print_df <- paste0('```\n', print_df, '\n```')
  return(print_df)
}
