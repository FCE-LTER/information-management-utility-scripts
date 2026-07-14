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

# Load libraries
library(readr)
library(readxl)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(EML)
library(xml2)
library(keyring)
library(RPostgres)

# Load the EML file, which is in a Zip file located in the "ezeml" folder.
# The EML file should have been updated and exported from ezEML.
# the Zip file will need to be unzipped to a temp directory
# to access the file inside (path inside zip is a file that ends in ".xml")
temp_dir <- tempdir()

zip_file <- list.files("ezeml/", pattern = "\\.zip$", full.names = TRUE)

target_file_in_zip <- unzip(zip_file, list = TRUE) %>%
  filter(grepl("\\.xml$", Name)) %>%
  pull(Name)

eml_file <- unzip(zip_file, files = target_file_in_zip, exdir = temp_dir)

# Load XML file for editing
xml2_input <- read_xml(eml_file)

# Load metadata from the  EML file.
eml <- EML::read_eml(eml_file)

# Retrieve permit list from Postgres database and relock keyring
# Note that you will need to setup a keyring with credentials before
# you can connect for the first time
{con <- dbConnect(RPostgres::Postgres(),
                  dbname = key_get("fce_dbname", keyring = "your_keyring"),
                  host = key_get("fce_dbhost", keyring = "your_keyring"),
                  port = key_get("fce_dbport", keyring = "your_keyring"),
                  user = key_get("fce_dbuser", keyring = "your_keyring"),
                  password = key_get("MY_SECRET", keyring = "your_keyring"))
  
  keyring::keyring_lock("your_keyring")
}

permits <- dbReadTable(con, "permits")

# Disconnect database
dbDisconnect(con)

# Get date range from metadata
date_range_metadata <- data.frame(Meta_date = c(eml$dataset$coverage$temporalCoverage$rangeOfDates$beginDate$calendarDate,
                                                eml$dataset$coverage$temporalCoverage$rangeOfDates$endDate$calendarDate)) %>%
  mutate(Meta_date = as.Date(Meta_date))

# Update permit list
relevant_permits <- permits %>%
  filter(group == "FCE"&
           (between(`start_date`,
                    min(date_range_metadata$Meta_date),
                    max(date_range_metadata$Meta_date))
            |between(`end_date`,
                     min(date_range_metadata$Meta_date),
                     max(date_range_metadata$Meta_date))
           )
  ) %>%
  select(group,
         `permit_number`) %>%
  mutate(permit_type = "National Park Service scientific research and collecting permit")

permits_output <- paste(relevant_permits$`permit_type`, relevant_permits$`permit_number`, collapse = ", ")
permits_output

permit_node <- xml_find_first(xml2_input, xpath = "//permit")

xml_text(permit_node)

xml_text(permit_node) <- permits_output

xml_text(permit_node)

# Write xml to file in eml/01_permits/ with the same name as the original file
write_xml(xml2_input, file.path("eml/01_permits/", basename(eml_file)))