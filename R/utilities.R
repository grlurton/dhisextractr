df_from_list <- function(list_data, n_element){
  out <- ldply(list_data, function(list_data){
    if(!is.null(list_data[[n_element]])){
      data.frame(list_data[[n_element]])
    }
  }
  )
  return(out)
}


##TODO Not efficient, should be extracted as a df from the beginning
df_from_org_unit_description <- function(org_units_description_list){
  out <- ldply(org_units_description_list, function(org_units_description_list){
    id <- org_units_description_list[[1]][[1]]
    date_opening <- org_units_description_list[[1]][[3]]
    name <- org_units_description_list[[1]][[4]]
    if(!is.null(org_units_description_list[[1]][[2]])){
      coordinates <- org_units_description_list[[1]][[2]]
    }
    if(!is.null(org_units_description_list[[1]][[5]])){
      parent <- org_units_description_list[[1]][[5]]
    }
    return(data.frame(id, date_opening, name, parent, coordinates))
  }
  )
  return(out)
}
