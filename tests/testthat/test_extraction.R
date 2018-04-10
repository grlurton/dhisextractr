context('Testing basic data extraction functionnalities')
library(RCurl)
library(XML)

base_url <- 'https://play.dhis2.org/2.29'
password = 'district'
userID = 'admin'

dhis_urls <- make_dhis_urls(base_url)

test_that("dhis urls are properly made",
          {expect_equal(as.character(dhis_urls$data_sets_url[1]),
                        'https://play.dhis2.org/2.29/api/dataSets.xml')
            })


dataset_url <- as.character(dhis_urls$data_sets_url[1])
root <- parse_page(dataset_url, userID, password)

test_that("properly reading datasets",
          {expect_is(root[0], "XMLNodeList")})

datasets <- extract_dhis_datasets(dataset_url ,
                                userID ,
                                password)

test_that("extracting data sets",
          {expect_is(datasets, 'data.frame' )
            expect_gte(nrow(datasets), 0)})




