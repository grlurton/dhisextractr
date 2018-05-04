df_from_list <- function(list_data, n_element){
    out <- plyr::ldply(list_data, function(list_data){
      if(class(list_data) == "list"){
      if(!is.null(list_data[[n_element]])){
        data.frame(list_data[[n_element]])
      }
      }
    }
  )
  return(out)
}


##TODO Not efficient, should be extracted as a df from the beginning
df_from_org_unit_description <- function(org_units_description_list){
  out <- plyr::ldply(org_units_description_list, function(org_units_description_list){
    if(class(org_units_description_list[[1]]) == "data.frame"){
    id <- org_units_description_list[[1]][[1]]
    if(!is.na(org_units_description_list[[1]][[3]])){
      date_opening <- org_units_description_list[[1]][[3]]
    }
    name <- org_units_description_list[[1]][[4]]
    coordinates <- 'no gps'
    if(!is.na(org_units_description_list[[1]][[2]])){
      coordinates <- as.character(org_units_description_list[[1]][[2]])
    }
    if(!is.null(org_units_description_list[[1]][[5]])){
      parent <- org_units_description_list[[1]][[5]]
    }
    return(data.frame(id, date_opening, name, parent, coordinates))
    }
  }
  )
  return(out)
}


load_env <- function(file = '.env'){
  tmp <- readLines(file)
  for(l in tmp){
    if(substr(l , 1, 1) != '#'){
      obj <- strsplit(l , "=")
      assign(obj[[1]][1], obj[[1]][2] , envir=globalenv())
    }
  }
}
