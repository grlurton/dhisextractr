#'Generic function to build expactations for all datasets (number of DEs per dataset)
#'
#' @param dataset_metadata --> The dataframe containing dataset metadata (should be the ouput of "extract_metadata_DS" function)
#' @param catcombo_metadata --> The dataframe containing dataset metadata (should be the ouput of "extract_metadata_DS" function)
#' @return Returns a dataframe containing the number of values expected in a dataset

  
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
    

  
#'Generic function to build expactations for all datasets (number of DEs per dataset)
#'
#' @param dataset_id --> id of the dataset
#' @param metadata_OrgUnit_datasetinfo --> The dataframe containing orgunit_dataset metadata (should be the ouput of "extract_metadata_DS_OrgUnit" function)
#' @param dataset_exp --> The dataframe containing expactations for all datasets (should be the ouput of "build_DS_exp" function)
#' @param period --> should be a vector with a monthly range such as "c(201701, 201712)"
#' @return Returns a dataframe with the number of expected values for each OrgUnit, dataset and period
  
  build_OU_period_exp <- function(dataset_id, metadata_OrgUnit_datasetinfo, dataset_exp, period) {

    tmp <- metadata_OrgUnit_datasetinfo %>% filter(DS_id==dataset_id)
    dataset_exp_short <- dataset_exp %>% select(-DS_name)
    tmp_exp1 <- merge(tmp, DS_exp_short, by.x = "DS_id", by.y = "DS_id")
    
    periods <- period_to_months(period[1], period[2], "")
    tmp_exp2 <- tmp_exp1[rep(rownames(tmp_exp1) , length(periods)) , ]
    tmp_exp2$period <- periods
    
    return(tmp_exp2)
    
  } 
  
  