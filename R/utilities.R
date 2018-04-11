df_from_list <- function(list_data, n_element){
  out <- ldply(data_elements, function(list_data){
    if(!is.null(list_data[[n_element]])){
      data.frame(list_data[[n_element]])
    }
  }
  )
  return(out)
}
