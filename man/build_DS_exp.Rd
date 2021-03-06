% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_description_v2.R
\name{build_DS_exp}
\alias{build_DS_exp}
\title{Generic function to build expactations for all datasets (number of DEs per dataset)}
\usage{
build_DS_exp(dataset_metadata, catcombo_metadata)
}
\arguments{
\item{dataset_metadata}{--> The dataframe containing dataset metadata (should be the ouput of "extract_metadata_DS" function)}

\item{catcombo_metadata}{--> The dataframe containing dataset metadata (should be the ouput of "extract_metadata_DS" function)}
}
\value{
Returns a dataframe containing the number of values expected in a dataset
}
\description{
Generic function to build expactations for all datasets (number of DEs per dataset)
}
