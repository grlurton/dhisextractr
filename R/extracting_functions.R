#'Generic xml page parsing function
#'
#' @param url The url of the page to parse in the DHIS api, as a character string. The
#' function is made to parse json pages, so input url should have a json extension.
#' @param userID your username in the given DHIS2 setting, as a character string
#' @param password your password for this DHIS2 setting, as a character string
#' flexibility in the kind of parsable urls.
parse_page <- function(url, userID, password){
  url <- as.character(url)
 # {if(substr(url, nchar(url) - 4, nchar(url)) != '.json')
  #  stop('This does not appear to be a json address')}

  # create username and password
  userpwd <- paste(userID, password , sep = ':')

  # Read Page
  response <- RCurl::getURL(url, userpwd = userpwd, httpauth = 1L,
                     header=FALSE, ssl.verifypeer = FALSE)

  # Parse the json
  parsed_page <- jsonlite::fromJSON(response)

  return(parsed_page)
}
