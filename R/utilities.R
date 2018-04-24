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


period_to_months <- function(period_start, period_end){
  year_start <- substr(period_start, 1, 4)
  year_end <- substr(period_end, 1, 4)
  years <- seq(year_start, year_end, 1)
  months <- seq(1,12)
  month_year_1 <- seq(substr(period_start, 5, 6) , 12)
  month_year_n <- seq(1, substr(period_end, 5, 6))
  if (length(years) == 1){
    months <- seq(substr(period_start, 5, 6) , substr(period_end, 5, 6))
    out <- paste0(year_start, months , concatenate = "")
  }
  if (length(years) == 2){
    out_1 <- paste0(year_start, month_year_1)
    out_2 <- paste0(year_end, month_year_n)
    out <- c(out_1, out_2)
  }
  if (length(years) > 2){
    years <- years[-c(1,length(years))]
    out_1 <- paste0(year_start, month_year_1)
    out_2 <- apply(expand.grid(years, months), 1, paste, collapse="")
    out_n <- paste0(year_end, month_year_n)
    out <- c(out_1, out_2, out_n)

  }
  out[nchar(out) == 5] <- paste0(substr(out[nchar(out) == 5],1,4) , '0' , substr(out[nchar(out) == 5],5,5))
  return(sort(out))
}


period_to_quarter <- function(period_start, period_end){
  year_start <- substr(period_start, 1, 4)
  year_end <- substr(period_end, 1, 4)
  years <- seq(year_start, year_end, 1)
  quarters <- seq(1,4)
  quarter_1 <- seq(as.numeric(substr(period_start, 5, 6)) %/% 4 +1 , 4)
  quarter_n <- seq(1, as.numeric(substr(period_end, 5, 6)) %/% 4 +1 )
  if (length(years) == 1){
    out <- paste0(year_start, 'Q' , quarter_1 , concatenate = "")
  }
  if (length(years) == 2){
    out_1 <- paste0(year_start, 'Q' , quarter_1 , concatenate = "")
    out_2 <- paste0(year_end, 'Q' , quarter_n , concatenate = "")
    out <- c(out_1, out_2)
  }
  if (length(years) > 2){
    years <- years[-c(1,length(years))]
    out_1 <- paste0(year_start, 'Q' , quarter_1 , concatenate = "")
    out_2 <- apply(expand.grid(years, quarters), 1, paste, collapse="Q")
    out_n <- paste0(year_end , 'Q' , quarter_n , concatenate = "")
    out <- c(out_1, out_2, out_n)

  }
  out[nchar(out) == 5] <- paste0(substr(out[nchar(out) == 5],1,4) , '0' , substr(out[nchar(out) == 5],5,5))
  return(sort(out))
}

period_to_quarter('201205', '201605')
