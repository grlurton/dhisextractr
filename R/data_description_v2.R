
  
  
  setkey(dcir_prs, "key_jointure")
  prs_ete <- merge(dcir_prs, dcir_ete, all.x = TRUE)
  
  consult <- as.data.table(consult)
  cols <- colnames(consult)
  agg_cols <- cols[-which(cols == "PRS_ACT_QTE" | cols =="PSE_SPE_COD" | cols =="PSE_STJ_COD" | cols =="PRS_PDS_QCP")]
  # agg_cols: "NUMERO_ENQ"  "EXE_SOI_DTD" "EXE_SOI_DTF" "PFS_EXE_NUM" "PFS_PRE_NUM" "PSP_SPE_COD" "PSP_STJ_COD" 
  # "ETB_PRE_FIN" "PRS_MTT_NUM" "PRS_NAT_REF" "ORG_CLE_NUM" "PRE_PRE_DTD" "ETB_EXE_FIN"
  consult <- consult[, .(PRS_ACT_QTE = sum(PRS_ACT_QTE), PSE_SPE_COD = paste0(PSE_SPE_COD, collapse = "_"), PSE_STJ_COD = paste0(PSE_STJ_COD, collapse = "_"), PRS_PDS_QCP = paste0(PRS_PDS_QCP, collapse = "_")), by=agg_cols]
  
  
  
  library(data.table)
  
  OU_metadata_DSinfo <- as.data.table(OU_metadata_DSinfo)
  DS_metadata <- as.data.table(DS_metadata)
  
  setkey(OU_metadata_DSinfo, "DS_id")
  setkey(DS_metadata, "DS_id")
  
  to_get_full <- merge(DS_metadata, OU_metadata_DSinfo, all = TRUE, allow.cartesian=TRUE)

  names(OU_metadata_DSinfo)
  
  
  
  
  
############################################################################################################
  
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
    

  
  
  
  