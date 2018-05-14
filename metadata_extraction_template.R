library(dhisextractr)
library(RCurl)
library(jsonlite)
library(plyr)
library(maptools)
library(shapefiles)
library(rgdal)

load_env('.env')
## Replace '.env' with the file of your choice, which includes at least
## DATA_DIR='the path to the data directory you want to use'
## BASE_URL='the base url of the DHIS implementation you work with'
## USERID='your userID for this DHIS implementation'
## PASSWORD='your password for this DHIS implementation'



setwd(DATA_DIR)

extracted_content <- extract_dhis_content(base_url = BASE_URL ,
                                          userID = USERID , password = PASSWORD)

### Making shapefiles ###
org_units_list <- read.csv('org_units_list.csv')
### Rewrite for initial extration outside of debugging
extracted_orgunits <- dlply(org_units_list , .(id) ,
                            function(org_units_list) {
                              try(extract_org_unit(as.character(org_units_list$url_list) ,
                                                   userID , password))
                            },
                            .progress = 'text'
)

org_units_description <- df_from_org_unit_description(extracted_orgunits)
write.csv(org_units_description , 'org_units_description.csv', row.names = FALSE)
org_units_group <- df_from_list(extracted_orgunits, 2)
colnames(org_units_group) <- c('id', 'id_org_units_group')
write.csv(org_units_group , 'org_units_group.csv', row.names = FALSE)
org_units_report <- df_from_list(extracted_orgunits, 3)
colnames(org_units_report) <- c('id', 'id_report')
write.csv(org_units_report , 'org_units_report.csv', row.names = FALSE)
###

shapefiles <- extract_geolocalisation(org_units_description)
write_geolocalisation(shapefiles)



