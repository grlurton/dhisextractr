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



period_to_months <- function(period_start, period_end, sep='-'){
  year_start <- substr(period_start, 1, 4)
  year_end <- substr(period_end, 1, 4)
  years <- seq(year_start, year_end, 1)
  months <- seq(1,12)
  month_year_1 <- seq(substr(period_start, 5, 6) , 12)
  month_year_n <- seq(1, substr(period_end, 5, 6))
  if (length(years) == 1){
    months <- seq(substr(period_start, 5, 6) , substr(period_end, 5, 6))
    out <- paste(year_start, months , sep = sep)
  }
  if (length(years) == 2){
    out_1 <- paste(year_start, month_year_1, sep = sep)
    out_2 <- paste(year_end, month_year_n, sep = sep)
    out <- c(out_1, out_2)
  }
  if (length(years) > 2){
    years <- years[-c(1,length(years))]
    out_1 <- paste(year_start, month_year_1, sep = sep)
    out_2 <- apply(expand.grid(years, months), 1, paste0, collapse = sep)
    out_n <- paste(year_end, month_year_n, sep = sep)
    out <- c(out_1, out_2, out_n)

  }
  sep_n <- nchar(sep)
  len_catch <- 5 + sep_n
  out[nchar(out) == len_catch] <- paste0(substr(out[nchar(out) == len_catch],1,
                                                4 + sep_n) , 
                                         '0' , 
                                 substr(out[nchar(out) == len_catch],len_catch,len_catch))
  return(sort(out))
}


period_to_quarter <- function(period_start, period_end, sep){
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

load_env <- function(file = '.env'){
  tmp <- readLines(file)
  for(l in tmp){
    if(substr(l , 1, 1) != '#'){
      obj <- strsplit(l , "=")
      assign(obj[[1]][1], obj[[1]][2] , envir=globalenv())
    }
  }
}


merge_data_files <- function(data_dir){
  data_files <- list.files(data_dir)
  data <- NA                
  for(dat in seq(1,length(data_files))){
    temp <- read.csv(paste0(data_dir, data_files[dat]))
    print(paste0('Merging ',  round(dat / length(data_files)), '% complete'))
    if(!is.na(data)){
      cols <- colnames(data)
      try(data <- rbind(data, temp[,cols]))}
    if(is.na(data)){
      data <- temp}
    rm(temp)
  }
  print('Printing the combined data')
  write.csv(data, paste0(snis_data_dir, '/data.csv'))
}

extraction_error <- function(out){
  for(col in colnames(out)){
    if(col != 'data_element_ID'){
      print('sfes')
      out[,col] <- 'extraction_error'
    }
  }
  #write.csv(out)
}




make_full_metadata_extract <- function(data_dir, url, login, password){
  setwd(data_dir)
  print('Reading Metadata')
  metadata <- extract_metdata(url=url, userID = login, password = password)
  
  ## EXTRACT DEG AND DS METADATA
  
  print('Extracting Data Elements')
  DEG_metadata <- extract_metadata_DEG(metadata)
  write.csv(DEG_metadata, 'data_elements_group.csv')
  
  print('Extracting Dat Sets')
  DS_metadata <-  extract_metadata_DS(metadata)
  write.csv(DS_metadata, 'data_sets.csv')
  
  print('Extracting Category Combo')
  CC_metadata <- extract_metadata_CC(metadata)
  write.csv(CC_metadata, 'category_combo.csv')
  
  
  print('Extracting Organisation Units')
  OU_metadata <- extract_metadata_OrgUnit(url=url, userID = login, password = password, 
                                          list_metdata = metadata)
  OU_metadata_flat <- flatten_hierarchy(OU_metadata)
  write.csv(OU_metadata, 'org_units.csv')
  
  print('Extracting at reporting requirements')
  OU_metadata_DSinfo <- extract_metadata_DS_OrgUnit(metadata, OU_metadata)
  write.csv(OU_metadata_DSinfo, 'org_units_datasets.csv')
  
  print('Extracting geolocalisation')
  geoloc <- extract_geolocalisation(metadata)
  write_geolocalisation(geoloc)
}
