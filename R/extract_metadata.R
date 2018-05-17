#'Generic function to extract json metadta file from DHIS2 API
#'
#' @param url --> The url of the DHIS2 you want to extract data from, as a character string.
#' @param userID --> Your username in the given DHIS2 setting, as a character string
#' @param password --> Your password for this DHIS2 setting, as a character string
#' @return Returns a list of metadata 

  extract_metdata <- function(url, userID, password) {
    tmp_url <- paste0(url, "/api/metadata.json&paging=false")
    r <- httr::GET(tmp_url, httr::authenticate(userID,password),
                   httr::timeout(500))
    r <- httr::content(r, "text")
    list_metadata <- jsonlite::fromJSON(r, flatten=TRUE)
    return(list_metadata)
  }
  

#'Generic function to extract DEG meatadata from metadata list
#'
#' @param list_metadata --> The list containing all metadata from a DHIS2.
#' @return Returns a dataframe containing DEG metadata 
  
  extract_metadata_DEG <- function(list_metdata) {
    
    DEG_metadata <- as.data.frame(list_metdata$dataElementGroups,stringsAsFactors=FALSE)
    DE_metadata <- as.data.frame(list_metdata$dataElements,stringsAsFactors=FALSE)
    
    DEG_content <- data.frame(matrix(ncol = 2, nrow = 0))
    colnames(DEG_content) <- c("DE_id", "DEG_id")
    
    for(i in 1:nrow(DEG_metadata)) {
      tmp <- DEG_metadata$dataElements[[i]]
      tmp$DEG_id <- DEG_metadata$id[i]
      DEG_content <- rbind(DEG_content, tmp)
      tmp <- NULL
    }
    
    DEG_metadata_short <- DEG_metadata %>% select(name, id) %>% rename(DEG_name = "name")
    DEG_content <- merge(DEG_content, DEG_metadata_short, by.x = "DEG_id", by.y = "id", all.x = T)
    DE_metadata_short <- DE_metadata %>% select(name, id, domainType, zeroIsSignificant, categoryCombo.id) %>% rename(DE_name = "name", DE_id = "id")
    DEG_metadata_out <- merge(DEG_content, DE_metadata_short, by.x = "DE_id", by.y = "DE_id", all.x = T)
    DEG_metadata_out <- DEG_metadata_out %>% select(DEG_name, DEG_id, DE_name, DE_id, domainType, zeroIsSignificant, categoryCombo.id) %>%
                                      arrange(DEG_name, DE_name)
    return(DEG_metadata_out)
  } 
  
  
#'Generic function to extract DataSet meatadata from metadata list
#'
#' @param list_metadata --> The list containing all metadata from a DHIS2.
#' @return Returns a dataframe containing DataSet metadata 
  
  extract_metadata_DataSet <- function(list_metdata) {

    DS_metadata <- as.data.frame(list_metdata$dataSets,stringsAsFactors=FALSE)
    DE_metadata <- as.data.frame(list_metdata$dataElements,stringsAsFactors=FALSE)
    
    DS_content <- data.frame(matrix(ncol = 2, nrow = 0))
    colnames(DS_content) <- c("DE_id", "DS_id")

    for(i in 1:nrow(DS_metadata)) {
      tmp <- DS_metadata$dataSetElements[[i]] %>% select(dataElement.id, dataSet.id) ## category combo information is removed (need to be reconsidered)
      colnames(tmp) <- c("DE_id", "DS_id")
      DS_content <- rbind(DS_content, tmp)
      tmp <- NULL
    }
    
    DS_metadata_short <- DS_metadata %>% select(name, id) %>% rename(DS_name = "name")
    DS_content <- merge(DS_content, DS_metadata_short, by.x = "DS_id", by.y = "id", all.x = T)
    DE_metadata_short <- DE_metadata %>% select(name, id, domainType, zeroIsSignificant, categoryCombo.id) %>% rename(DE_name = "name", DE_id = "id")
    DS_metadata_out <- merge(DS_content, DE_metadata_short, by.x = "DE_id", by.y = "DE_id", all.x = T)
    DS_metadata_out <- DS_metadata_out %>% select(DS_name, DS_id, DE_name, DE_id, domainType, zeroIsSignificant, categoryCombo.id) %>%
      arrange(DS_name, DE_name)
    return(DS_metadata_out)
  } 
  
  
  
#'Generic function to extract OrgUnit metadata from DHIS2 API
#'
#' @param url --> The url of the DHIS2 you want to extract data from, as a character string.
#' @param userID --> Your username in the given DHIS2 setting, as a character string
#' @param password --> Your password for this DHIS2 setting, as a character string
#' @return Returns a dataframe containing Orgunit metadata

  
  extract_metadata_OrgUnit <- function(url, userID, password, list_metdata) {
    
    require(stringr)
    OU_metadata <- as.data.frame(list_metdata$organisationUnits, stringsAsFactors=FALSE)
    OU_metadata$level <- str_count(OU_metadata$path, "/")
    max_level <- max(OU_metadata$level)
    parent_string <- paste0(paste(rep("id,name,parent[", max_level-2), collapse = ""), "id,name", paste(rep("]", max_level-2), collapse = ""))
    
    tmp_url <- paste0(url,"/api/organisationUnits.json?fields=level,",parent_string,"&paging=false")
    r <- httr::GET(tmp_url, httr::authenticate(userID,password),
                   httr::timeout(60))
    r <- httr::content(r, "text")
    d <- jsonlite::fromJSON(r, flatten=TRUE)
    
    OU_metadata_out <- as.data.frame(d$organisationUnits,stringsAsFactors=FALSE)
    return(OU_metadata_out)
  } 
 
  
  #'Generic function to show dataset available in Orgunits
  #'
  #' @param list_metadata --> The list containing all metadata from a DHIS2
  #' @param OrgUnit_pyr --> The dataframe containing Orgunit pyramid (should be the ouput of "extract_metadata_OrgUnit" function)
  #' @return Returns a dataframe containing Orgunit metadata related to datasets
  
  
  extract_metadata_DataSet_OrgUnit <- function(list_metadata=d, OrgUnit_pyr=OU_metadata) {
    
    DS_metadata <- as.data.frame(list_metdata$dataSets,stringsAsFactors=FALSE)

    DS_content <- data.frame(matrix(ncol = 2, nrow = 0))
    colnames(DS_content) <- c("OU_id", "DS_id")

    for(i in 1:nrow(DS_metadata)) {
      tmp <- DS_metadata$organisationUnits[[i]]
      tmp$DS_id <- DS_metadata$id[i]
      colnames(tmp) <- c("OU_id", "DS_id")
      DS_content <- rbind(DS_content, tmp)
      tmp <- NULL
    }
    
    DS_metadata_short <- DS_metadata %>% select(name, id) %>% rename(DS_name = "name")
    DS_content <- merge(DS_content, DS_metadata_short, by.x = "DS_id", by.y = "id", all.x = T)
    
    DS_metadata_out <- merge(DS_content, OrgUnit_pyr, by.x = "OU_id", by.y = "id", all.x = T)
    return(DS_metadata_out)
  } 
  