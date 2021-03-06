% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{generic_api_call}
\alias{generic_api_call}
\title{Generic API call to Twitter}
\usage{
generic_api_call(api = "https://api.twitter.com/labs/1/users",
  param_list = list(usernames = "ChelseaFC", format = "detailed"))
}
\arguments{
\item{api}{String. The url of the API to call}

\item{param_list}{List. List of keys and values to provide as parameters to the API call. See details.}
}
\value{
List of results from the API call
}
\description{
This function is a generic wrapper for API calls to twitter (any url and list of specified parameters to send to the API)
}
\details{
This function can take an api and list of parameters for use in making API calls to Twitter.
The url used for the api argument will depend on the type of information the user wants to recieve
For example for tweet times, see https://developer.twitter.com/en/docs/tweets/timelines/overview
The param_list is a list of the specified keys and values the user wants to specify when making the the API call.
This function requires \code{\link{twitter_oauth2}} to be run first.
}
\examples{

twitter_oauth2() # one-time run for authentication
result <- generic_api_call(api = 'https://api.twitter.com/labs/1/users', param_list = list(usernames = 'ChelseaFC,ManUtd', format = 'detailed'))
print(cbind(result$data$name, result$data$stats))

# makes the call 'curl -X GET -H "Authorization: Bearer <BEARER_TOKEN>" "https://api.twitter.com/labs/1/users?usernames=ChelseaFC,ManUtd&format=detailed"'
# parameter options can be found at https://developer.twitter.com/en/docs/labs/tweets-and-users/api-reference/get-users-v1

}
\keyword{api}
\keyword{call}
\keyword{generic}
\keyword{twitter}
