#'Generic function to build expactations for all datasets
#'
#' @param dataset_metadata --> The dataframe containing dataset metadata (should be the ouput of "extract_metadata_DS" function)
#' @param catcombo_metadata --> The dataframe containing dataset metadata (should be the ouput of "extract_metadata_DS" function)
#' @return Returns a dataframe containing the number of values expected in a dataset
  
  
  # expectations per dataset
  
  build_DS_exp <- function(dataset_metadata, catcombo_metadata) {
  
    DS_exp <- dataset_metadata %>% select(DS_id, DS_name) %>% 
                            group_by(DS_id, DS_name) %>% 
                            summarise(exp=length(DS_name))
    
    DS_tmp <- merge(dataset_metadata, catcombo_metadata, by.x = "categoryCombo.id", by.y = "CatCombo_id", all.x = T)
    DS_exp_withCC <- DS_tmp %>% select(DS_id) %>% 
                                group_by(DS_id) %>% 
                                summarise(exp_withCC=length(DS_id))
    
    DS_exp_out <- merge(DS_exp, DS_exp_withCC, by.x = "DS_id", by.y = "DS_id", all.x = T)
    return(DS_exp_out)
    
  }
    

  
  
  
  