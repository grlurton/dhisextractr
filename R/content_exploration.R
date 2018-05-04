look_up_data_element_term <- function(term){
  out <- data_elements[grep(term, data_elements$name, ignore.case = TRUE), ]
  return(as.character(out$id))
}

look_up_data_set_term <- function(term){
  de_id <- look_up_data_element_term(term)
  ds_id <- unique(data_elements_sets$dataSet_id[data_elements_sets$id %in% de_id])
  return(as.character(ds_id))
}

get_de_reporting_facilities <- function(ds_list){
  ou_list <- org_units_reports[org_units_reports$id_report %in% ds_list ,]
  return(unique(as.character(ou_list$id)))
}


# Completude / par data set / bloc de data set

# modalities

# Somme by level -> donner la completude


# Donnees brutes


## ou est la data



## canvas pdss => Naike

## canvas SNIS


## Status
#  NA =>
#  1 =>
#  0 =>
## Table analytics => Martin


## Map rupture donnees SNIS zone
