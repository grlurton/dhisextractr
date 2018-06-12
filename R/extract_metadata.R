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
  

#'Generic function to extract Category combo meatadata from metadata list
#'
#' @param list_metadata --> The list containing all metadata from a DHIS2.
#' @return Returns a dataframe containing Category combo metadata   
  
  extract_metadata_CC <- function(list_metdata) {
    
    CatCombo_metadata <- as.data.frame(list_metdata$categoryCombos,stringsAsFactors=FALSE)
    CatComboOpt_metadata <- as.data.frame(list_metdata$categoryOptionCombos,stringsAsFactors=FALSE)
    CatComboOpt_metadata_short <- CatComboOpt_metadata %>% dplyr::select(id, name, categoryCombo.id) %>% dplyr::rename(CatComboOpt_id="id", CatComboOpt_name="name", CatCombo_id="categoryCombo.id")
    CatOpt_metadata <- as.data.frame(list_metdata$categoryOptions,stringsAsFactors=FALSE)
    CatOpt_metadata_short <- CatOpt_metadata %>% dplyr::select(id, name)
    Cat_metadata <- as.data.frame(list_metdata$categories,stringsAsFactors=FALSE)

    CatCombo_content <- data.frame(matrix(ncol = 3, nrow = 0))
    for(i in 1:nrow(CatComboOpt_metadata)) {
      tmp <- CatComboOpt_metadata$categoryOptions[[i]]
      if(nrow(tmp) > 0){
        tmp$CatComboOpt_id <- CatComboOpt_metadata$id[i]
        tmp$CatCombo_id <- CatComboOpt_metadata$categoryCombo.id[i]
        CatCombo_content <- rbind(CatCombo_content, tmp)
        tmp <- NULL
      }
    }
    CatCombo_content <- CatCombo_content %>% dplyr::rename(CatOpt_id="id") %>% dplyr::arrange(CatCombo_id, CatOpt_id)
    
    CatCombo_Cat <- data.frame(matrix(ncol = 2, nrow = 0))
    for(i in 1:nrow(CatCombo_metadata)) {
      tmp <- CatCombo_metadata$categories[[i]]
      if(nrow(tmp) > 0){
        tmp$CatCombo_id <- CatCombo_metadata$id[i]
        CatCombo_Cat <- rbind(CatCombo_Cat, tmp)
        tmp <- NULL
      }
    }
    CatCombo_Cat <- CatCombo_Cat %>% dplyr::rename(Cat_id="id") %>% dplyr::arrange(CatCombo_id, Cat_id)
    
    Cat_content <- data.frame(matrix(ncol = 2, nrow = 0))
    for(i in 1:nrow(Cat_metadata)) {
      tmp <- Cat_metadata$categoryOptions[[i]]
      if(nrow(tmp) > 0){
        tmp$Cat_id <- Cat_metadata$id[i]
        Cat_content <- rbind(Cat_content, tmp)
        tmp <- NULL
      }
    }
    Cat_content <- Cat_content %>% dplyr::rename(CatOpt_id="id") %>% dplyr::arrange(Cat_id)
    
    CatCombo_Cat_CatOpt <- merge(CatCombo_Cat, Cat_content, by = "Cat_id", all.x = T) %>% dplyr::arrange(CatCombo_id, Cat_id, CatOpt_id)
    CatCombo_content_Cat <- merge(CatCombo_content, CatCombo_Cat_CatOpt, by = c("CatCombo_id", "CatOpt_id"), all.x = T) %>% dplyr::arrange(CatCombo_id, Cat_id, CatOpt_id)

    CatCombo_content_Cat$col_num <- NA
    CatCombo_content_Cat$col_num[1] <- 1
    for(i in 2:nrow(CatCombo_content_Cat)) {
      if(CatCombo_content_Cat$CatCombo_id[i]==CatCombo_content_Cat$CatCombo_id[i-1]) {
        if(is.na(CatCombo_content_Cat$Cat_id[i])) { CatCombo_content_Cat$col_num[i] <- 1 }
        else {
          if(CatCombo_content_Cat$Cat_id[i]==CatCombo_content_Cat$Cat_id[i-1]) { CatCombo_content_Cat$col_num[i] <- CatCombo_content_Cat$col_num[i-1] }
          else { CatCombo_content_Cat$col_num[i] <- CatCombo_content_Cat$col_num[i-1] + 1 }
        }
      }
      else { CatCombo_content_Cat$col_num[i] <- 1 }
    }
    
    CatCombo_content_Cat <- CatCombo_content_Cat %>% dplyr::select(-Cat_id)
    
    tmp_wide <- reshape(CatCombo_content_Cat, idvar = c("CatCombo_id", "CatComboOpt_id"), timevar = "col_num", direction = "wide")
    tmp_width <- ncol(tmp_wide)
    tmp_cols <- c(-1, 0)
    for(i in 1:(tmp_width-2)){
      tmp_wide <- merge(tmp_wide, CatOpt_metadata_short, by.x = paste0("CatOpt_id.", i), by.y = "id", all.x = T)
      colnames(tmp_wide)[colnames(tmp_wide)=="name"] <- paste0("CatOpt_name.", i)
      tmp_cols <- c(tmp_cols, i, -i-1)
    }
    tmp_cols <- tmp_cols + tmp_width
    tmp_wide <- tmp_wide[,tmp_cols]
    
    CC_metadata_out <- tmp_wide %>% dplyr::arrange(CatCombo_id)
      
    return(CC_metadata_out)
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
      if (!is.null(tmp)) {
          if(nrow(tmp) > 0) {
            if(ncol(tmp) > 1) {                           # Update to take into account older versions of DHIS2 (up to 2.20)
              tmp <- as.data.frame(tmp$id)
              tmp$DEG_id <- DEG_metadata$id[i]
            } else {
              tmp$DEG_id <- DEG_metadata$id[i]
            }
          colnames(tmp) <- c("DE_id", "DEG_id")
          DEG_content <- rbind(DEG_content, tmp)
          tmp <- NULL
        }
      }
    }
    
    DEG_metadata_short <- DEG_metadata %>% dplyr::select(name, id) %>% dplyr::rename(DEG_name = "name")
    DEG_content <- merge(DEG_content, DEG_metadata_short, by.x = "DEG_id", by.y = "id", all.x = T)
    DE_metadata_short <- DE_metadata %>% dplyr::select(name, id, domainType, 
                                                zeroIsSignificant, 
                                                categoryCombo.id) %>% dplyr::rename(DE_name = "name", DE_id = "id")
    DEG_metadata_out <- merge(DEG_content, DE_metadata_short, by.x = "DE_id", by.y = "DE_id", all.x = T)
    DEG_metadata_out <- DEG_metadata_out %>% dplyr::select(DEG_name, DEG_id, DE_name, DE_id, domainType, zeroIsSignificant, 
                                                           categoryCombo.id) %>%dplyr::arrange(DEG_name, DE_name)
    return(DEG_metadata_out)
  } 
  
  
#'Generic function to extract DataSet meatadata from metadata list
#'
#' @param list_metadata --> The list containing all metadata from a DHIS2.
#' @return Returns a dataframe containing DataSet metadata 
  
  extract_metadata_DS <- function(list_metdata=d) {

    DS_metadata <- as.data.frame(list_metdata$dataSets,stringsAsFactors=FALSE)
    DE_metadata <- as.data.frame(list_metdata$dataElements,stringsAsFactors=FALSE)
    
    DS_content <- data.frame(matrix(ncol = 2, nrow = 0))
    colnames(DS_content) <- c("DE_id", "DS_id")
    
    DS_cols <- colnames(DS_metadata)
    
    for(i in 1:nrow(DS_metadata)) {
      if("dataSetElements" %in% DS_cols) {
        tmp <- DS_metadata$dataSetElements[[i]]
        if(!is.null(tmp)) {
          if(nrow(tmp) > 0){
            tmp <- tmp %>% dplyr::select(dataElement.id, 
                                        dataSet.id) ## category combo information is removed (need to be reconsidered)
          }
          colnames(tmp) <- c("DE_id", "DS_id")
          DS_content <- rbind(DS_content, tmp)
        }
      } else if("dataElements" %in% DS_cols) {        # Update to take into account older versions of DHIS2 (up to 2.20)
        tmp <- DS_metadata$dataElements[[i]]
        if(!is.null(tmp)){
          if(nrow(tmp) > 0){
            tmp <- as.data.frame(tmp$id)
            tmp$DS_id <- DS_metadata$id[i]
          }
          colnames(tmp) <- c("DE_id", "DS_id")
          DS_content <- rbind(DS_content, tmp)
        }
      }
      tmp <- NULL
    }
    
    DS_metadata_short <- DS_metadata %>% dplyr::select(name, id) %>% dplyr::rename(DS_name = "name")
    DS_content <- merge(DS_content, DS_metadata_short, by.x = "DS_id", by.y = "id", all.x = T)
    DE_metadata_short <- DE_metadata %>% dplyr::select(name, id, domainType, zeroIsSignificant, categoryCombo.id) %>% dplyr::rename(DE_name = "name", DE_id = "id")
    DS_metadata_out <- merge(DS_content, DE_metadata_short, by.x = "DE_id", by.y = "DE_id", all.x = T)
    DS_metadata_out <- DS_metadata_out %>% dplyr::select(DS_name, DS_id, DE_name, DE_id, domainType, zeroIsSignificant, categoryCombo.id) %>%
      dplyr::arrange(DS_name, DE_name)
    return(DS_metadata_out)
  } 
  
  
  
#'Generic function to extract OrgUnit metadata from DHIS2 API
#'
#' @param url --> The url of the DHIS2 you want to extract data from, as a character string.
#' @param userID --> Your username in the given DHIS2 setting, as a character string
#' @param password --> Your password for this DHIS2 setting, as a character string
#' @return Returns a dataframe containing Orgunit metadata

  
  extract_metadata_OrgUnit <- function(url, userID, password, list_metdata) {
    
    OU_metadata <- as.data.frame(list_metdata$organisationUnits, stringsAsFactors=FALSE)
    OU_metadata$level <- stringr::str_count(OU_metadata$path, "/")
    max_level <- max(OU_metadata$level)
    parent_string <- paste0(paste(rep("id,name,parent[", max_level-1), collapse = ""), "id,name", paste(rep("]", max_level-1), collapse = ""))
    
    tmp_url <- paste0(url,"/api/organisationUnits.json?fields=level,",parent_string,"&paging=false")
    r <- httr::GET(tmp_url, httr::authenticate(userID,password),
                   httr::timeout(100))
    r <- httr::content(r, "text")
    d <- jsonlite::fromJSON(r, flatten=TRUE)
    
    OU_metadata_out <- as.data.frame(d$organisationUnits,stringsAsFactors=FALSE)
    return(OU_metadata_out)
  } 
 
  
#'Generic function to show dataset available in Orgunits
#'
#' @param list_metadata --> The list containing all metadata from a DHIS2
#' @param metadata_OrgUnit --> The dataframe containing Orgunit metadata (should be the ouput of "extract_metadata_OrgUnit" function)
#' @return Returns a dataframe containing Orgunit metadata related to datasets
  
  
  extract_metadata_DS_OrgUnit <- function(list_metdata, metadata_OrgUnit) {
    
    DS_metadata <- as.data.frame(list_metdata$dataSets,stringsAsFactors=FALSE)

    DS_content <- data.frame(matrix(ncol = 2, nrow = 0))
    colnames(DS_content) <- c("OU_id", "DS_id")

    for(i in 1:nrow(DS_metadata)) {
      tmp <- DS_metadata$organisationUnits[[i]]
      if(nrow(tmp) > 0){
        tmp$DS_id <- DS_metadata$id[i]
        colnames(tmp) <- c("OU_id", "DS_id")
        DS_content <- rbind(DS_content, tmp)
      }
      tmp <- NULL
    }
    
    DS_metadata_short <- DS_metadata %>% dplyr::select(name, id) %>% dplyr::rename(DS_name = "name")
    DS_content <- merge(DS_content, DS_metadata_short, by.x = "DS_id", by.y = "id", all.x = T)
    
    DS_metadata_out <- merge(DS_content, metadata_OrgUnit, by.x = "OU_id", by.y = "id", all.x = T) %>% 
      dplyr::rename(OU_level = "level", OU_name = "name") %>%
      dplyr::arrange(DS_id)
    return(DS_metadata_out)
  } 


  
#'Generic function to show reshape the orgUnit hierarchy presentation
#'
#' @param metadata_OrgUnit --> The dataframe containing Orgunit metadata (should be the ouput of "extract_metadata_OrgUnit" function)
#' @return Returns a dataframe containing the same Orgunit metadata but with levels as columns (instead of parents) 

  flatten_hierarchy <- function(metadata_OrgUnit, clean = TRUE){
  for (level in unique(metadata_OrgUnit$level)){
    for(i in seq(1,level-1)){
      if (i > 0){
        col1 <- match(i, sort(seq(1,level-1), decreasing = TRUE))
        metadata_OrgUnit[metadata_OrgUnit$level == level , paste0('level_', i , '_name')] <- metadata_OrgUnit[metadata_OrgUnit$level == level , 4 + 2*(col1 - 1)]
        metadata_OrgUnit[metadata_OrgUnit$level == level , paste0('level_', i , '_id')] <- metadata_OrgUnit[metadata_OrgUnit$level == level , 4 + 2*(col1 - 1) + 1]
      }
    }
  }
  if (clean){
    to_drop <- grep('parent', colnames(metadata_OrgUnit))
    metadata_OrgUnit <- metadata_OrgUnit[, -to_drop]
    }
  return(metadata_OrgUnit)
}
