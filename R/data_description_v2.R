#'Generic function to build expectations for all datasets (number of DEs per dataset)
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
    

  
#'Generic function to build expectations for a scpecifc dataset (for each orgunit and period)
#'
#' @param dataset_id --> id of the dataset
#' @param metadata_OrgUnit_datasetinfo --> The dataframe containing orgunit_dataset metadata (should be the ouput of "extract_metadata_DS_OrgUnit" function)
#' @param dataset_exp --> The dataframe containing expactations for all datasets (should be the ouput of "build_DS_exp" function)
#' @param period --> should be a vector with a monthly range such as "c(201701, 201712)"
#' @return Returns a dataframe with the number of expected values for each OrgUnit and period
  
  build_OU_period_exp_DSlevel <- function(dataset_id, metadata_OrgUnit_datasetinfo, dataset_exp, period) {

    tmp <- metadata_OrgUnit_datasetinfo %>% filter(DS_id==dataset_id)
    dataset_exp_short <- dataset_exp %>% select(-DS_name)
    tmp_exp1 <- merge(tmp, dataset_exp_short, by.x = "DS_id", by.y = "DS_id")
    
    periods <- period_to_months(period[1], period[2], "")
    vect_periods <- c()
    for(i in 1:length(periods)) { vect_periods <- c(vect_periods, rep(periods[i], nrow(tmp_exp1))) }
    tmp_exp2 <- tmp_exp1[rep(rownames(tmp_exp1) , length(periods)) , ]
    tmp_exp2$period <- vect_periods
    
    tmp_exp2 <- tmp_exp2 %>% arrange(OU_id, period)
    
    return(tmp_exp2)
    
  } 
  
  
  
  
#'Generic function to build expectations for a scpecifc dataelement (for each orgunit and period)
#'
#' @param dataelement_id --> id of the dataelement
#' @param metadata_OrgUnit_datasetinfo --> The dataframe containing orgunit_dataset metadata (should be the ouput of "extract_metadata_DS_OrgUnit" function)
#' @param metadata_dataset --> The dataframe containing dataset metadata (should be the ouput of "extract_metadata_DS" function)
#' @param metadata_CatCombo --> The dataframe containing CatCombo metadata (should be the ouput of "extract_metadata_CC" function)
#' @param period --> should be a vector with a monthly range such as "c(201701, 201712)"
#' @return Returns a dataframe with the number of expected values for each OrgUnit, period and CatCombo
  
  build_OU_period_exp_DElevel <- function(dataelement_id, metadata_OrgUnit_datasetinfo, metadata_dataset, metadata_CatCombo, period) {
    
    # dataelement_id="UxD03qX5O0t"
    # metadata_OrgUnit_datasetinfo=OU_metadata_DSinfo
    # metadata_dataset=DS_metadata
    # metadata_CatCombo=CC_metadata
    # period=c("201701", "201803")
    
    tmp <- metadata_dataset %>% filter(DE_id==dataelement_id) %>% select(DS_id, DE_name, DE_id, categoryCombo.id)
    tmp_CatCombo <- merge(tmp, CC_metadata, by.x = "categoryCombo.id", by.y = "CatCombo_id", all.x = T)
    tmp_exp1 <- merge(metadata_OrgUnit_datasetinfo, tmp_CatCombo, by.x = "DS_id", by.y = "DS_id")
    
    periods <- period_to_months(period[1], period[2], "")
    vect_periods <- c()
    for(i in 1:length(periods)) { vect_periods <- c(vect_periods, rep(periods[i], nrow(tmp_exp1))) }
    tmp_exp2 <- tmp_exp1[rep(rownames(tmp_exp1) , length(periods)) , ]
    tmp_exp2$period <- vect_periods
    
    rep(1, 3)
    
    tmp_exp2 <- tmp_exp2 %>% arrange(OU_id, period)
    
    return(tmp_exp2)
    
  } 
