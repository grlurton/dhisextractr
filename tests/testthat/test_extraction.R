context('Testing basic data extraction functionnalities')

load_env("../../.env")

## EXTRACT LIST OF METADATA FROM API
print("Extracting Metadata")
metadata <- extract_metdata(url=snis_url, userID = snis_login, password = snis_password)

test_that("Metadata well extracted",
          {expect_is(metadata, 'list' )
            expect_gte(length(metadata), 0)})

## EXTRACT DEG AND DS METADATA
DEG_metadata  <- extract_metadata_DEG(metadata)
DS_metadata <-  extract_metadata_DS(metadata)

test_that("Data Elements metadata well extracted",
          {expect_is(DEG_metadata, 'data.frame')})
          
test_that("Data Sets metadata well extracted",
          {expect_is(DS_metadata, 'data.frame')})


## EXTRACT ORGUNIT METADATA FROM API
OU_metadata <- extract_metadata_OrgUnit(url=snis_url, userID = snis_login, password = snis_password, 
                                        list_metdata = metadata)
OU_metadata_flat <- flatten_hierarchy(OU_metadata)

test_that("Organisation Units metadata well extracted",
          {expect_is(OU_metadata, 'data.frame')})

n_units <- nrow(OU_metadata)

test_that("Data Sets metadata well extracted",
          {expect_is(OU_metadata_flat, 'data.frame')
            expect_equal(nrow(OU_metadata_flat), n_units)})

## EXTRACT ORGUNIT METADATA FROM API
OU_metadata_DSinfo <- extract_metadata_DS_OrgUnit(metadata, OU_metadata)

test_that("Organisation Units DataSets well extracted",
          {expect_is(OU_metadata, 'data.frame')})


