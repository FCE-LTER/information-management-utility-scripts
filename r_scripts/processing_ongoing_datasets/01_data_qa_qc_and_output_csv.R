# Last revision date: 2026-07-14
# Author: Gabriel Kamener
# Author email:gkamener@fiu.edu
# Organization:
# Florida Coastal Everglades LTER Program
# (Lead Principal Investigator: John Kominoski, jkominos@fiu.edu)
# Institute of Environment
# Florida International University
# 11200 SW 8th Street, OE 148
# Miami, FL 33199
# Website: https://fcelter.fiu.edu
# GitHub site: https://github.com/FCE-LTER

###### Introduction ###### 

# This script processes submissions of updates for ongoing datasets
# and runs general checks of the data against the metadata. Output
# values are also rounded to the precision specified in the column
# metadata. 

# Note this script currently uses content from a dataset update
# (knb-lter-fce.1073.19) as an example. File paths for input/output files,
# column specifications, and formatting of output data should be revised
# as needed for other datasets or updates.

# Load libraries
library(readr)
library(readxl)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(EML)

# Create project subfolders if they don't already exist
if (!dir.exists("new_submission")) {
  dir.create("new_submission")
}

if (!dir.exists("data")) {
  dir.create("data")
  dir.create("data/output")
}

if (!dir.exists("eml")) {
  dir.create("eml")
  dir.create("eml/01_permits")
  dir.create("eml/02_final")
}

if (!dir.exists("ezeml")) {
  dir.create("ezeml")
}

options(nwarnings = 10000)

###### Load data and metadata ###### 

# Load temp directory
temp_dir <- tempdir()

# List zip file
zip_file <- list.files("ezeml/", pattern = "\\.zip$", full.names = TRUE)

# Target the xml EML file in the zip
target_eml_file_in_zip <- unzip(zip_file, list = TRUE) %>%
  filter(grepl("\\.xml$", Name)) %>%
  pull(Name)

# Target the .csv data file that's in the "data" folder within the zip
target_data_file <- unzip(zip_file, list = TRUE) %>%
  filter(grepl("data/.*\\.csv$", Name)) %>%
  pull(Name)

# Unzip the eml file to temp directory
eml_file <- unzip(zip_file, files = target_eml_file_in_zip, exdir = temp_dir)

# Unzip the data file to temp directory
data_file <- unzip(zip_file, files = target_data_file, exdir = temp_dir)

# Load metadata from the EML file you've updated and exported from ezEML.
# This script will check the metadata against the data file to ensure consistency,
# and will output a data file with standardized formatting and precision.
eml <- EML::read_eml(eml_file)

input_data <- read_csv(data_file,
                       col_types = cols(
                         `SITENAME` = col_character(),
                         `Date` = col_date(format = "%Y-%m-%d"),
                         `Time` = col_time(format = "%H:%M"),
                         `Salinity` = col_double(),
                         `TN` = col_double(),
                         `TP` = col_double(),
                         `TOC` = col_double(),
                         `NH4` = col_double(),
                         `NandN` = col_double(),
                         `NO2` = col_double(),
                         `SRP` = col_double(),
                         `DOC` = col_double(),
                         `NO3` = col_double()
                       ))

# Review any problems with data loading
problems <- problems(input_data)

###### Check input data against metadata ###### 

# Get date range from metadata
date_range_metadata <- data.frame(Meta_date = c(eml$dataset$coverage$temporalCoverage$rangeOfDates$beginDate$calendarDate,
                                                eml$dataset$coverage$temporalCoverage$rangeOfDates$endDate$calendarDate)) %>%
  mutate(Meta_date = as.Date(Meta_date))

# Get date range from data
date_range_data <- input_data %>%
  summarise(Start_date_matches_metadata = min(Date),
            End_date_matches_metadata = max(Date)
  ) %>%
  pivot_longer(1:2,
               names_to = "Check",
               values_to = "Data_Date")

# Combine metadata and data date range data frames and check if dates match
dates_bound <- cbind(date_range_data,
                     date_range_metadata) %>%
  mutate(Pass = if_else(Meta_date == Data_Date,
                        TRUE,
                        FALSE))

dates_bound

# Extract attribute names from metadata
attributes_from_metadata <- get_attributes(eml$dataset$dataTable$attributeList)$attributes %>%
  select(attributeName)

# Extract attribute names from data
attribute_from_data <- as.data.frame(colnames(input_data)) %>%
  rename(Input_Attribute = `colnames(input_data)`)

# Anti-join metadata attributes from metadata vs data. Should generate 0 rows
# if attribute names match.
anti_tabl_meta_to_data <- anti_join(attributes_from_metadata,
                                    attribute_from_data,
                                    by = c("attributeName" = "Input_Attribute"))

# Anti-join metadata attributes from metadata vs data
anti_data_to_table_metadata <- anti_join(attribute_from_data,
                                         attributes_from_metadata,
                                         by = c("Input_Attribute" = "attributeName"))

# Compare output attributes vs those in metadata
check_input_attributes <- cbind(attribute_from_data,
                                attributes_from_metadata) %>%
  mutate(Check = "Output attributes match metadata",
         Pass = if_else(.[[1]] == .[[2]], TRUE, FALSE),
         .before = 1)

# Get list of site names from metadata
geocoverage_list <- (eml$dataset$coverage$geographicCoverage)

geocoverage_unlisted <- geocoverage_list %>%
  lapply(., `[[`, 1) %>%
  unlist(.)

sites_from_metadata <- as.data.frame(geocoverage_unlisted) %>%
  select(Meta_site_name = `geocoverage_unlisted`) %>%
  arrange(Meta_site_name) %>%
  # Remove "FCE LTER Site " from site names to allow for comparison
  mutate(Meta_site_name = str_remove(Meta_site_name, "FCE LTER Site "))

sites_from_metadata

# Get site names from data
sites_from_data <- input_data %>%
  distinct(SITENAME) %>%
  arrange(SITENAME)

sites_from_data

check_input_site_names <- sites_from_data %>%
  distinct(SITENAME) %>%
  arrange(SITENAME) %>%
  cbind(.,
        sites_from_metadata) %>%
  mutate(Check = "Site names match metadata",
         Pass = if_else(.[[1]] == .[[2]], TRUE, FALSE),
         .before = 1)

check_input_site_names

# Get attribute precision from metadata
attribute_precision_from_metadata <- get_attributes(eml$dataset$dataTable$attributeList)$attributes %>%
  select(attributeName,
         precision)

attribute_precision_from_metadata

###### Format output data ###### 

# Set precision to match
# those in metadata file
output <- input_data %>%
  select(SITENAME,
         Date,
         Time,
         Salinity,
         TN,
         TP,
         TOC,
         NH4,
         NandN,
         NO2,
         SRP,
         DOC,
         NO3
         ) %>%
  mutate(Time = str_sub(Time, -8, -4),
         Time = if_else(is.na(Time), "-9999", Time)) %>%
  mutate_if(is.numeric, replace_na, replace = -9999) %>%
  mutate(`Salinity` = sprintf("%.1f", round(`Salinity`, 1)),
         `TN` = sprintf("%.3f", round(`TN`, 3)),
         `TP` = sprintf("%.2f", round(`TP`, 2)),
         `TOC` = sprintf("%.3f", round(`TOC`, 3)),
         `NH4` = sprintf("%.2f", round(`NH4`, 2)),
         `NandN` = sprintf("%.2f", round(`NandN`, 2)),
         `NO2` = sprintf("%.2f", round(`NO2`, 2)),
         `SRP` = sprintf("%.2f", round(`SRP`, 2)),
         `DOC` = sprintf("%.3f", round(`DOC`, 3)),
         `NO3` = sprintf("%.2f", round(`NO3`, 2))
         )

###### Review output data ###### 
check_output_site_names <- input_data %>%
  distinct(Output_site = SITENAME) %>%
  arrange(Output_site) %>%
  cbind(.,
        sites_from_metadata) %>%
  mutate(Check = "Site names match metadata",
         Pass = if_else(.[[1]] == .[[2]], TRUE, FALSE),
         .before = 1)

# Extract attributes from output data frame
attributes_from_output <- as.data.frame(colnames(output)) %>%
  rename(Output_Attribute = `colnames(output)`)

# Compare output attributes vs those in metadata
check_output_attributes <- cbind(attributes_from_output,
                                 attributes_from_metadata) %>%
  mutate(Check = "Output attributes match metadata",
         Pass = if_else(.[[1]] == .[[2]], TRUE, FALSE),
         .before = 1)

# Bind and review full list of checks
review <- data.frame(Check = character(),
                     Pass = logical()) %>%
  rbind.fill(.,
             check_input_site_names,
             dates_bound,
             check_input_attributes)

# Filter only checks that failed to pass
review_filtered <- review %>%
  filter(Pass != TRUE)

# Check that there is 1 record per site/date/time for this dataset.
# 0 records in the result means data are as expected in this case.
# Required grouping and the expected number of rows per group may differ
# for a project depending on how data were collected.
check_records <- output %>%
  # group by site name, date, and time
  group_by(SITENAME,
           Date,
           Time) %>%
  # count number of records per group
  mutate(n = n()) %>%
  # filter for groups with n not equal to 1 record
  filter(n != 1)

###### Write data to file ###### 
write.table(output,
          eol = "\n",
          quote = F,
          sep = ",",
          row.names = F,
          "data/output/LT_ND_Grahl_002.csv")