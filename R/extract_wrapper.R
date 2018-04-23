#'Creating the data call adress
#'
#' \code{make_extract_call} creates a url used to call some data
#'
#' @param base_url The base url of the DHIS2 setting
#' @param data_sets A table of data sets, as extracted by \link{extract_dhis_datasets}
#' @param org_unit A table of organization units, as extracted by \link{extract_org_unit}
#' @param period_start Date of the beginning of the period from which to extract data
#' @param period_end Date of the end of the period from which to extract data
#' @return Returns an url that calls on the data to be extracted based on inputted
#' parameters
make_data_set_extract_call <- function (base_url, data_sets, org_unit, period_start, period_end,
                                        update_date = "2009-01-01"){
  data_set_url <- paste("dataSet=", data_sets,
                        "&", collapse = "", sep = "")
  org_unit_url <- paste("orgUnit=", org_unit, "&",
                        collapse = "", sep = "")
  url_call <- paste(base_url, "/api/dataValueSets.json?", data_set_url,
                    org_unit_url, "startDate=", period_start, "&endDate=",
                    period_end, "&lastUpdated=", update_date, sep = "")
  print(url_call)
  url_call
}

make_data_element_extract_call <- function (base_url, data_elements, org_units, period_start, period_end,
                                        update_date = "2009-01-01"){
  data_elements_url <- paste0("dimension=dx:", paste(data_elements, collapse=";"))
  org_units_url <- paste0("&dimension=ou:", paste(org_units, collapse=";"))
  months <- period_to_months(period_start, period_end)
  dates_url <- paste0("&dimension=pe:", paste(months, collapse=";"))
  url_call <- paste0(base_url, "/api/25/analytics.json?", data_elements_url,
                    org_units_url, dates_url)
  url_call
}

#'Extracting a data value
#'
#' \code{extract_data} extracts data based on a url call
#'
#' @param url_call A data calling url as made by \link{make_extract_call}
#' @param userID your username in the given DHIS2 setting, as a character string
#' @param password your password for this DHIS2 setting, as a character string
#' @return Returns a dataframe with one data value by line, and columns data_element_ID ,
#' period , org_unit_ID , value and category.
extract_data <- function(url_call , userID , password){
  pass <- paste(userID , password , sep = ':')
  response<-getURL(url_call , userpwd=pass , httpauth = 1L ,
                   header=FALSE , ssl.verifypeer = FALSE)

  parsed_page <- fromJSON(response)
  print(parsed_page)
  if(length(parsed_page) > 0){
    if('dataValues' %in% names(parsed_page)){
      out <- parsed_page$dataValues
    }
    if('rows' %in% names(parsed_page)){
      out <- data.frame(parsed_page$rows)
      colnames(out) <- c('data_element_ID', 'org_unit_ID', 'period', 'value')
    }
    return(out)
  }
}





#'Extracting multiple sets of data value
#'
#' \code{extract_all_data} Extracts a data based on list of data sets, organisation units, #' and a period.Can be used to make complete extraction.
#'
#' @param base_url The base url of the DHIS2 setting
#' @param data_sets A table of data sets, as extracted by \link{extract_dhis_datasets}
#' @param org_unit A table of organization units, as extracted by \link{extract_org_unit}
#' @param period_start Date of the beginning of the period from which to extract data
#' @param period_end Date of the end of the period from which to extract data
#' @param userID your username in the given DHIS2 setting, as a character string
#' @param password your password for this DHIS2 setting, as a character string
#' @return Returns an url that calls on the data to be extracted based on inputted
#' parameters
extract_all_data <- function (base_url, data_sets, org_units, deb_period, end_period,
          pace = 1, userID, password, update_date = NULL , type_extract = 'ds'){
  N_units <- length(org_units)
  n_calls <- ceiling(N_units/pace)
  group <- sort(rep(seq(n_calls), pace))[1:N_units]
  org_units <- data.frame(ID=org_units, group=group)
  N_groups <- max(group)
  time_env <- new.env()
  assign("start", Sys.time(), envir = time_env)
  assign("time_remaining", "Unknown", envir = time_env)
  assign("time_remaining_seq", c(), envir = time_env)
  extracted_data <- ddply(org_units, .(group), function(org_units) {
    time_remaining <- time_env$time_remaining
    print(paste("Group", unique(org_units$group), "of", N_groups,
                sep = " "))
    print(paste("Estimation Time Remaining", time_remaining,
                "hours", sep = " "))
    out <- data.frame(data_element_ID = org_units$ID,
                      period = "", org_unit_ID = "", value = "", category = "",
                      last_update = "")
    if (type_extract == 'ds'){
      url_call <- make_data_set_extract_call(base_url, data_sets, org_units$ID,
                                  deb_period, end_period, update_date = update_date)
    }
    if (type_extract == 'de'){
      url_call <- make_data_element_extract_call(base_url, data_sets, org_units$ID,
                                             deb_period, end_period)
    }
    print(url_call)
    try({
      out <- extract_data(url_call, userID, password)
    })
    print(paste0(nrow(out), " Data Points Extracted"))
    time_remaining <- difftime(Sys.time(), time_env$start,
                               units = "hours")/unique(org_units$group) * (N_groups -
                                                                             unique(org_units$group))
    assign("time_remaining", time_remaining, envir = time_env)
    seq <- c(time_env$time_remaining_seq, time_env$time_remaining)
    plot(seq(1, N_groups), c(seq, rep(NA, N_groups - unique(org_units$group))),
         ylim = c(0, max(seq)), xlab = "Group", ylab = "Remaining Time Estimation (Hours)")
    assign("time_remaining_seq", seq, envir = time_env)
    out
  })
  extracted_data
}
