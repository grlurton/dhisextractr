# Accessing DHIS2 for exploratory data analysis

This project provides a set of R functions designed to allow data analysts interested in using data collected in DHIS2 to effectively access and explore this data.

The usecase for this package seems mainly to be data analysts in organisations partnering with Ministries of Health using DHIS2. A typical relationship in these settings will be an access granted to the analyst, with little insight on what data is being collected, as well as on what data is actually available. Rather than fishing for available data through DHIS2 tools, these functions offer a framework to systematically explore DHIS data, using tools familiar to data analysts (mainly R).

This package started as a set of _ad-hoc_ function for a project at the Institute for Health Metrics and Evaluation, and evolved into a more systematic and structured tool due to the multiple similar use cases identified for different teams. It comprises different types of tools, that do or should exist at some point.


## Different sets of tools
### Extracting metadata

A first set of tools allows for an initial extraction of all relevant metadata (org units, data elements, categories). It writes a set of csv files, as well as two shapefiles for all geolocated entities. See the [dedicated template](metadata_extraction_template.R) for usage.

### Exploring metadata

This set of tool is still very exploratory (feedback and ideas welcome). It should allow the analysts to easily explore what type of information is defined for different themes, and to get an idea of which organisation units should report on each of these themes. 

### Extracting data

There are different ways to access the data. One can choose to extract full data sets (important to measure data completeness) or simple data elements. Also, marginal extraction of data sets is possible, for users only interested in downloading data entered after a certain date.

### Data preprocessing

Once the desired data has been extracted, there is often a need for completeness and quality checks. Some imputation could also be considered.
