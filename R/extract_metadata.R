#'Generic function to extract json metadta file
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
#' @param list_metadatt --> The list containing all metadata from a DHIS2.
#' @return Returns a dataframe containing DEG metadata 
  
  extract_metadata_DEG <- function(list_metdata = d) {
    
    DEG_metadata <- as.data.frame(list_metdata$dataElementGroups,stringsAsFactors=FALSE)
    DE_metadata <- as.data.frame(list_metdata$dataElements,stringsAsFactors=FALSE)
    CatCombo_metadata <- as.data.frame(list_metdata$categoryCombos,stringsAsFactors=FALSE)
    CatComboOpt_metadata <- as.data.frame(list_metdata$categoryOptionCombos,stringsAsFactors=FALSE)
    
    DEG_content <- data.frame(matrix(ncol = 2, nrow = 0))
    colnames(DEG_content) <- c("id", "DEG_id")
    
    for(i in 1:nrow(DEG_metadata)) {
      tmp <- DEG_metadata$dataElements[[i]]
      tmp$DEG_id <- DEG_metadata$id[i]
      DEG_content <- rbind(DEG_content, tmp)
      tmp <- NULL
    }
    
    colnames(DEG_content) <- c("DE_id", "DEG_id")
    
    DEG_metadata_short <- DEG_metadata %>% select(name, id) %>% rename(DEG_name = "name")
    DEG_content <- merge(DEG_content, DEG_metadata_short, by.x = "DEG_id", by.y = "id", all.x = T)
    DE_metadata_short <- DE_metadata %>% select(name, id, domainType, zeroIsSignificant, categoryCombo.id) %>% rename(DE_name = "name", DE_id = "id")
    DE_metadata_out <- merge(DEG_content, DE_metadata_short, by.x = "DE_id", by.y = "DE_id", all.x = T)
    DE_metadata_out <- DE_metadata_out %>% select(DEG_name, DEG_id, DE_name, DE_id, domainType, zeroIsSignificant, categoryCombo.id) %>%
                                      arrange(DEG_name, DE_name)
    return(DE_metadata_out)
  } 
  
  
  #'Generic function to extract Dataset meatadata from metadata list
  #'
  #' @param list_metadatt --> The list containing all metadata from a DHIS2.
  #' @return Returns a dataframe containing DEG metadata 
  
  extract_metadata_DEG <- function(list_metdata = d) {
    
    DEG_metadata <- as.data.frame(list_metdata$dataElementGroups,stringsAsFactors=FALSE)
    DE_metadata <- as.data.frame(list_metdata$dataElements,stringsAsFactors=FALSE)
    CatCombo_metadata <- as.data.frame(list_metdata$categoryCombos,stringsAsFactors=FALSE)
    CatComboOpt_metadata <- as.data.frame(list_metdata$categoryOptionCombos,stringsAsFactors=FALSE)
    
    DEG_content <- data.frame(matrix(ncol = 2, nrow = 0))
    colnames(DEG_content) <- c("id", "DEG_id")
    
    for(i in 1:nrow(DEG_metadata)) {
      tmp <- DEG_metadata$dataElements[[i]]
      tmp$DEG_id <- DEG_metadata$id[i]
      DEG_content <- rbind(DEG_content, tmp)
      tmp <- NULL
    }
    
    colnames(DEG_content) <- c("DE_id", "DEG_id")
    
    DEG_metadata_short <- DEG_metadata %>% select(name, id) %>% rename(DEG_name = "name")
    DEG_content <- merge(DEG_content, DEG_metadata_short, by.x = "DEG_id", by.y = "id", all.x = T)
    DE_metadata_short <- DE_metadata %>% select(name, id, domainType, zeroIsSignificant, categoryCombo.id) %>% rename(DE_name = "name", DE_id = "id")
    DE_metadata_out <- merge(DEG_content, DE_metadata_short, by.x = "DE_id", by.y = "DE_id", all.x = T)
    DE_metadata_out <- DE_metadata_out %>% select(DEG_name, DEG_id, DE_name, DE_id, domainType, zeroIsSignificant, categoryCombo.id) %>%
      arrange(DEG_name, DE_name)
    return(DE_metadata_out)
  } 
  
  
  
  
  
  
  