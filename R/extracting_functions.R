#'Generic xml page parsing function
#'
#' @param url The url of the page to parse in the DHIS api, as a character string. The
#' function is made to parse json pages, so input url should have a json extension.
#' @param userID your username in the given DHIS2 setting, as a character string
#' @param password your password for this DHIS2 setting, as a character string
#' flexibility in the kind of parsable urls.
parse_page <- function(url, userID, password){
  url <- as.character(url)
  {if(substr(url, nchar(url) - 4, nchar(url)) != '.json')
    stop('This does not appear to be a json address')}

  # create username and password
  userpwd <- paste(userID, password , sep = ':')

  # Read Page
  response <- getURL(url, userpwd = userpwd, httpauth = 1L,
                     header=FALSE, ssl.verifypeer = FALSE)

  # Parse the json
  parsed_page <- fromJSON(response)

  return(parsed_page)
}


#'Generic function to extract relevant nodes in DHIS element
#' \code{extract_info} goes to a specific DHIS2 element url, and extracts attributes
#' name , id and href. It can extract elements that span on multiple pages
#'
#' @param url_page The default url of the elemtns to parse in the DHIS api, as a
#' character string function is made to parse xml pages, so input url should be an xml
#' adress or a generic web adress without extension.
#' @param root root of this page, as extracted by \code{\link{parse_page}}
#' @param node_name the name of the name we wish to extract
#' @param out an empty dataframe in which to return the output (there are more elegant ways to do it for sure, see it later).
extract_info <- function(url_page, node_name, userID , password , monitor = F){
  url_page <- as.character(url_page)
  page <- parse_page(url_page, userID, password)

  # Get the number of pages to extract
  NPages <- as.numeric(page$pager$pageCount)
  # Turn NPages to 1 if there are no pages
  NPages[is.na(NPages)] <- 1
  # Set up and output matrix
  out <- data.frame(matrix(ncol = 2, nrow = 0))

  ## Extraction
  for (page in 1:NPages){
    if(monitor == TRUE){
      print(paste('Parsing page' , page , 'out of' , NPages , sep = ' '))
    }
    # Setting the page to extract
    if(NPages > 1){
      url_read <- paste(url_page , '?page=' , page , sep = '')
      if(substr(url_read, nchar(url_read) - 4, nchar(url_read)) != '.json'){
        url_read <- paste0(url_read, '.json')
      }
    }
    else{
      url_read <- url_page
      if(substr(url_read, nchar(url_read) - 4, nchar(url_read)) != '.json'){
        url_read <- paste0(url_read, '.json')
      }
    }

    # Read and Parse page
    page <- parse_page(url_read , userID , password)
    # Access node containing the elements we want to extract
    node_elements <- page[[node_name]]

    data <- data.frame(node_elements)
  }
  out <- rbind(out, data)
  return(out)
}

#' Extracting the list of datasets in DHIS
#'
#' \code{extract_dhis_datasets} goes to a specific DHIS2 implementation, and extracts
#' its full list of data sets
#'
#' @param url The url of the datasets list in the DHIS web api, as a character string.
#' The function is made to parse xml pages, so input url should be an xml adress or a
#' generic web adress without extension.
#' @param userID your username in the given DHIS2 setting, as a character string
#' @param password your password for this DHIS2 setting, as a character string
#' @return Returns a data frame with each dataset as a line and for each data set, its
#' unique ID, its name and its url.

extract_dhis_datasets <- function(url , userID , password){
  extract_info(url , root , node_name = 'dataSets' , userID , password)
}

#'Extract the list of data elements in a DHIS data set
#'
#' \code{extract_data_elements} extracts the data elements recorded in a given dataset.
#'
#' @param url The url of the dataset page in the DHIS api, from which we want to
#' extract the data elements. The function is made to parse xml pages, so input url
#' should be an xml adress or a generic web adress without extension.
#' @param userID your username in the given DHIS2 setting, as a character string
#' @param password your password for this DHIS2 setting, as a character string
#' @return Returns a data frame with each data element as a line and for each data
#' element, its unique ID, its name and its url.
extract_data_elements <- function(dataset_url, userID, password){
  extract_info(dataset_url , node_name = 'dataElements',  userID , password)
}

#'Extract the list of Organisation Units in the DHIS setting
#'
#' \code{extract_orgunits_list} extracts the list of Organisation Units recorded in a
#' DHIS setting
#'
#' @param url The url of the organisation units page in the DHIS api. The function is
#' made to parse xml pages, so input url should be an xml adress or a generic web
#' adress without extension.
#' @param userID your username in the given DHIS2 setting, as a character string
#' @param password your password for this DHIS2 setting, as a character string
#' @return Returns a data frame with each organisation unit as a line and for each
#' organisation unit, its unique ID, its name and its url.
extract_orgunits_list <- function(org_unit_page_url, userID, password){
  extract_info(org_unit_page_url, node_name = 'organisationUnits' , userID , password , TRUE)
}


#'Extract information about an Orgunit
#'
#' \code{extract_org_unit} extracts all the information about
#'
#' @param url The url of the organisation unit for which we want to extract
#' information. The function is made to parse xml pages, so input url should be an xml
#' adress or a generic web adress without extension.
#' @param userID your username in the given DHIS2 setting, as a character string
#' @param password your password for this DHIS2 setting, as a character string
#' @return Returns a list with three elements :
#' * __Metadata__ For each organization unit, includes its geolocalization and
#' reference to parent unit
#'
#' * __Group __ Groups in which the organization unit is included. This is where the
#' type of organization unit is stored
#'
#' * __Datasets__ Datasets for which the organisation unit should communicate data
extract_org_unit <- function(org_unit_url, userID, password){
  root <- parse_page(org_unit_url , userID , password)

  ##Extraction of org units metadata
    parent_id <- parent_name <- parent_url <- NA

    id <- xmlAttrs(root)[['id']]
    coordinates <- xmlValue(root[['coordinates']])
    opening_date <- xmlValue(root[['openingDate']])
    name <- xmlValue(root[['displayName']])
  if (!is.null(root[['parent']])){
    parent_id <- xmlAttrs(root[['parent']])[['id']]
  }
  org_unit_metadata <- data.frame(id , coordinates , opening_date , name , parent_id)

  ##Extraction of org units groups
  if (!is.null(root[['organisationUnitGroups']])){
    Groups <- root[['organisationUnitGroups']]
    org_unit_group <- xmlSApply(Groups , xmlGetAttr , 'id')
  }

  ##Extraction of org units datasets
  if (!is.null(root[['dataSets']])){
    Datasets <- root[['dataSets']]
    org_unit_dataset <- xmlSApply(Datasets , xmlGetAttr , 'id')
  }

  out <- list(org_unit_metadata , org_unit_group , org_unit_dataset)
  out
}

#'Extract the categories for data elements
#'
#' \code{extract_categories} extracts the list of categories that are used for different
#' data elements.
#'
#' @param categories_url The url of the categories page in the DHIS api. The function is
#' made to parse xml pages, so input url should be an xml adress or a generic web
#' adress without extension.
#' @param userID your username in the given DHIS2 setting, as a character string
#' @param password your password for this DHIS2 setting, as a character string
#' @return Returns a data frame with each category as a line and for each
#' category, its unique ID, its name and its url.
extract_categories <- function(categories_url, userID, password){
  out <- data.frame(org_unit_ID = character() ,
                    org_unit_name = character()  ,
                    org_unit_url = character() )
  root <- parse_page(categories_url , userID , password)
  extract_info(categories_url , root , 'categoryOptionCombos' , 'categoryOptionCombo'  , userID , password)
}


#'Make relevant urls in DHIS web api
#'
#' \code{make_dhis_urls} takes the main adress of a DHIS implementation and returns
#' the relevant adresses in the web api that will be used for extracting data.
#'
#' @param base_url The url of the DHIS implementation
make_dhis_urls <- function(base_url){
  data_sets_url <- paste(base_url , '/api/dataSets.xml' , sep = '')
    data_elements_url <- paste(base_url , '/api/dataElements.xml' , sep = '')
  org_units_url <- paste(base_url , '/api/organisationUnits.xml' , sep = '')
  data_elements_categories <- paste(base_url , '/api/categoryOptionCombos.xml' , sep = '')
  data.frame(data_sets_url , data_elements_url , data_elements_categories , org_units_url)
}
