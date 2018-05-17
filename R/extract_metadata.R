#'Generic function to extract json metadta file
#'
#' @param url --> The url of the DHIS2 you want to extract data from, as a character string.
#' @param userID --> Your username in the given DHIS2 setting, as a character string
#' @param password --> Your password for this DHIS2 setting, as a character string
#' @return Returns a list of metadata 

  extract_metdata <- function(url, userID, password) {
    tmp_url <- paste0(url, "api/metadata.json&paging=false")
    r <- httr::GET(tmp_url, httr::authenticate(username,password),
                   httr::timeout(500))
    r <- httr::content(r, "text")
    d <- jsonlite::fromJSON(r, flatten=TRUE)
    return(d)
  }