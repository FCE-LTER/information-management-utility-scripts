# Last revision date: 2026-08-04
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

# This script checks a new FCE data file against the last published version in the
# EDI data repository. Portions used to query data from EDI were adapted from
# an article in Smith 2023.

# Note this script currently uses content from an FCE dataset update
# (knb-lter-fce.1073.18) as an example. File paths, package identifiers, and
# entity names, will need to be revised as needed for other dataset updates.

# Citation: 
# Smith C (2023). _EDIutils: An API Client for the Environmental Data
# Initiative Repository in R_. <https://github.com/ropensci/EDIutils>


# Load libraries
library(tidyverse) 
library(EDIutils)

#### Create API key and load it into system environment ####

# Note: See the following link to learn about EDI's
# Identity and Access Manager and how to create an EDI API key:
# https://edirepository.org/resources/iam

# If you created an API key, you can save it on your local computer's credential
# storage by running the commented out code below. You should only need to run these
# lines once per user/per computer as long as the API key is current
#
## First create a keyring. You will be prompted to create a password for the keyring.
## Save the keyring password somewhere safe.
#keyring_create(keyring = "my_keyring")
#
## Save the API Key. You will be prompted to enter the Key as a password
# key_set(service = "edi",
#        username = "edi_im_read_api_key",
#        keyring = "my_keyring")
#

# Login with the API key
login(key = key_get(service = "edi",
                    username = "edi_im_read_api_key",
                    keyring = "my_keyring"))

# Relock the key
keyring_lock("my_keyring")

###### Load data ###### 

# Read in new data file.
# Note that all columns are read in as characters to ensure formatting of 
# values with trailing zeros do not change. It also ensures there are no
# issues created by floating point estimations of numeric values.
new_data <- read_csv("data/output/LT_ND_Grahl_002.csv",
                     col_types = cols(.default = col_character()))

# Declare package scope and identifier, request latest revision number, and paste
# together as packageId.
scope <- "knb-lter-fce"
identifier <- "1073"
revision <- list_data_package_revisions(scope, identifier, filter = "newest")
packageId <- paste(scope, identifier, revision, sep = ".")


# Get list of data files in the last revision of your package in EDI.
res <- read_data_entity_names(packageId)

res

# Download desired file in raw bytes. Use the entityName and entityID as keys.
entityName <- "LT_ND_Grahl_002.csv"

entityId <- res$entityId[res$entityName == entityName]

# Load last revision of published data file from EDI repository
raw <- read_data_entity(packageId, entityId)

# Parse published raw file with a .csv reader, so we can
# compare it to the new data file.
last_rev_data <- readr::read_csv(file = raw,
                                 col_types = cols(.default = col_character()))


# Check old vs new. All records in the old revision
# should match those in the new one, unless data needed
# to be corrected in the new revision. If old records
# appear here, they need to be compared against the respective
# records in the new revision.
anti_old_to_new <- anti_join(last_rev_data,
                             new_data,
                             by = names(last_rev_data))

# Check new vs old. Only new records should appear here,
# unless data needed to be corrected in the new revision.
anti_new_to_old <- anti_join(new_data,
                             last_rev_data,
                             by = names(new_data))

