build_expectations <- function(org_units_data_sets, data_elements_data_sets, period){
  to_get <- merge(org_units_data_sets, data_elements_data_sets, all = FALSE)
  periods <- period_to_months(period[1], period[2], "") ## Complete with non monthly periods
  to_get_full <- to_get[rep(rownames(to_get) , length(periods)) , ]
  to_get_full$period <- periods
  return(to_get_full)
}

check_expectations <- function(expectations, data){
  periods <- unique(expectations$period)
  data <- data[data$period %in% periods, ]
  availability_check <- merge(expectations , data  , 
                              by = c('data_element_id', 'period', 'org_unit_id') , 
                              all.x = TRUE)
  return(availability_check)
}

check_report_completeness <- function(report){
  report_id <- unique(as.character(report$data_set_id))
  n_expected <- nrow(report[report$data_set_id == report_id,])
  non_reported <- sum(is.na(report$value[report$data_set_id == report_id]))
  return(1 - (non_reported / n_expected))
}

check_data_element_availability <- function(report){
  n_expected <- nrow(report)
  non_reported <- sum(is.na(report$value))
  return(1 - (non_reported / n_expected))
}


## Checking completeness functions

mean_by_zone 

check_data_distribution <- function(report){
  nat_mean <- mean(report$value)
}

## Execute on slices

## Produce outputs

make_map

make_resume 

print_table

make_timeline

