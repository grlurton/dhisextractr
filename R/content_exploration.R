look_up_data_element_term <- function(term){
  out <- data_elements[grep(term, data_elements$name, ignore.case = TRUE), ]
  return(out)
}

look_up_data_set_term <- function(term){
  de <- look_up_data_element_term(term)
  de_id <- de$id
  ds_id <- data_elements_sets$dataSet_id[data_elements_sets$id %in% de_id]
  return(ds_id)
}

get_de_reporting_facilities <- function(de_list){
  ds_list <- data_elements_sets$dataSet_id[data_elements_sets$id %in% de_list]
  print(ds_list)
  ou_list <- org_units_reports[org_units_reports$id_report %in% ds_list ,]
  return(unique(ou_list$id))
}



#data_elements <- read.csv('data_elements_metadata.csv')
#3data_elements_sets <- read.csv('data_elements_sets.csv')
#org_units_reports <- read.csv('org_units_report.csv')
#look_up_data_element_term('penta')

#look_up_data_set_term('penta')
#get_de_reporting_facilities('')



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
