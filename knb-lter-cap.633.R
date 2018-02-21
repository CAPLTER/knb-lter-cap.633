
# README ------------------------------------------------------------------

# The workflow details the data processing and publication steps for the Tres
# Rios (knb-lter-cap.633) dataset. This workflow begins with version 2 (633.2).
# The first version included the construction of a database, and population of
# that database with version 1 data that were then queried for publication.
# However, it is not clear that the database is appropriate for this project, so
# this version simply munges the Tres Rios data as csv files, which includes
# those data that were published in version 1.


# libraries ---------------------------------------------------------------

library(tidyverse)
library(EML)
library(aws.s3)
# library(RPostgreSQL)
# library(RMySQL)
# library(tools)
# library(readxl)
# library(capeml)


# reml helper functions ---------------------------------------------------

source('~/localRepos/reml-helper-tools/writeAttributesFn.R')
source('~/localRepos/reml-helper-tools/createDataTableFromFileFn.R')
source('~/localRepos/reml-helper-tools/createKMLFn.R')
source('~/localRepos/reml-helper-tools/address_publisher_contact_language_rights.R')
source('~/localRepos/reml-helper-tools/createOtherEntityFn.R')
source('~/localRepos/reml-helper-tools/createPeople.R')
source('~/localRepos/reml-helper-tools/createFactorsDataframe.R')


# connections -------------------------------------------------------------

# Amazon
source('~/Documents/localSettings/aws.s3')
  
# postgres
source('~/Documents/localSettings/pg_prod.R')
source('~/Documents/localSettings/pg_local.R')
  
pg <- pg_prod
pg <- pg_local

# mysql
source('~/Documents/localSettings/mysql_prod.R')
prod <- mysql_prod


# dataset details to set first --------------------------------------------

projectid <- 633
packageIdent <- 'knb-lter-cap.633.2'
pubDate <- '2018-02-20'


# data processing ---------------------------------------------------------


# Function to standardize tsect names, these will be passed to the tibble flow
# for each datasets. Input is a string or vector of strings (from Tibble),
# output is a string or vector of strings standarized to (1) upper-case alpha,
# (2) trimmed of white space, (3) underscore separating each meaningful
# characters.
recodeTransect <- function(unformatted_tsect) {
  
  # uppercase and trim white space
  trimmed <- toupper(str_trim(unformatted_tsect, side = c('both')))
  
  # remove any existing character separators
  separator <- gsub("-|:| ", "", trimmed)
  
  # convert all tsets to format char_char 
  nosep <- gsub("_", "", separator)
  underscore <- gsub("(.)\\B", "\\1_", nosep)
  
  # return standardized tsect
  return(underscore)
  
}


# primary productivity ----------------------------------------------------

# primary productivity: split into typha_leaves and plant attributes
primary_productivity <- read_csv('raw_data/tr_agpp_data_CAPdb.csv')

primary_productivity[primary_productivity == ''] <- NA

primary_productivity <- primary_productivity %>%
  mutate(
    date = as.POSIXct(date, format = "%m/%d/%y"),
    date = format(date, "%Y-%m-%d"),
    transect = recodeTransect(transect),
    species = as.factor(species),
    plant_id = seq_along(date),
    data.book.ID = as.character(data.book.ID),
    quadrat = as.character(quadrat)
  )

# typha

typha_leaves <- primary_productivity %>% 
  select(date:species, plant_id, data.book.ID, Notes, starts_with("leaf")) %>% 
  gather(key = "leaf_num", value = "leaf_length", starts_with("leaf")) %>% 
  filter(!is.na(leaf_length)) %>% 
  mutate(
    plant_id = as.character(plant_id),
    leaf_length = as.numeric(leaf_length)
  ) %>% 
  arrange(plant_id) %>% 
  as.data.frame


writeAttributes(typha_leaves) # write data frame attributes to a csv in current dir to edit metadata

typha_leaves_desc <- "Tres Rios: indices of primary productivity. Measurements of lenghts of Typha spp. leaves located within study transects at the Tres Rios wetlands. These data are used as inputs into biomass models to calculate above-ground primary productivity."

typha_leaves_factors <- factorsToFrame(typha_leaves)

typha_leaves_DT <- createDTFF(dfname = typha_leaves,
                              factors = typha_leaves_factors,
                              description = typha_leaves_desc,
                              dateRangeField = 'date')

# plant attributes

plant_attributes <- primary_productivity %>% 
  select(-X1, -starts_with("leaf")) %>%
  arrange(plant_id) %>% 
  mutate(plant_id = as.character(plant_id)) %>% 
  select(date:species, plant_id, cdb:Notes)

writeAttributes(plant_attributes) # write data frame attributes to a csv in current dir to edit metadata

plant_attributes_desc <- "Tres Rios: indices of primary productivity. Measurements of various physiological characteristics of individual plants located within study transects at the Tres Rios wetlands. These data are used as inputs into biomass models to calculate above-ground primary productivity."

plant_attributes_factors <- factorsToFrame(plant_attributes)

plant_attributes_DT <- createDTFF(dfname = plant_attributes,
                                  factors = plant_attributes_factors,
                                  description = plant_attributes_desc,
                                  dateRangeField = 'date')


# transpiration -----------------------------------------------------------

transpiration <- read_csv('raw_data/tr_irga_fielddata_CAPdb.csv')

transpiration[transpiration == ''] <- NA

transpiration <- transpiration %>%
  mutate(
    date = as.POSIXct(paste(date, time), format = "%m/%d/%y %H:%M:%S"),
    transect = case_when(
      transect == 'boardwalk' ~ 'boardwalk',
      transect != 'boardwalk' ~ recodeTransect(transect)
    ),
    plant_spp = as.factor(plant_spp)
  ) %>% 
  rename(species = plant_spp) %>% 
  select(-time)

plant_spp = replace(plant_spp, plant_spp == "stab", "s_tabernaemontani"),
plant_spp = replace(plant_spp, plant_spp == "smar", "s_maritimus"),
plant_spp = replace(plant_spp, plant_spp == "sam", "s_americanus"),
plant_spp = replace(plant_spp, plant_spp == "scal", "s_californicus"),
plant_spp = replace(plant_spp, plant_spp == "sac/stab", "s_acutus_OR_s_tabernaemontani"),
plant_spp = replace(plant_spp, plant_spp == "hyd", ""),
plant_spp = replace(plant_spp, plant_spp == "typ", ""),
plant_spp = replace(plant_spp, plant_spp == "hyb", ""),


writeAttributes(transpiration) # write data frame attributes to a csv in current dir to edit metadata

transpiration_desc <- "Tres Rios: transpiration. Measurements of leaf-level gas exchange and micro-climate taken using an infrared gas analyzer on individual plant leaves located within study transects at the Tres Rios wetlands. These instantaneous micro-climate and transpiration data are used as inputs into transpiration and evaporation models that scale these data and estimate whole-system atmospheric water losses."

transpiration_factors <- factorsToFrame(transpiration)

transpiration_DT <- createDTFF(dfname = transpiration,
                               factors = transpiration_factors,
                               description = transpiration_desc,
                               dateRangeField = 'date')


# water quality -----------------------------------------------------------

water_quality <- read_csv('raw_data/tr_waterquality_fielddata_CAPdb.csv')

water_quality[water_quality == ''] <- NA

water_quality <- water_quality %>%
  mutate(
    date = as.POSIXct(date, format = "%m/%d/%y"),
    date = format(date, "%Y-%m-%d"),
    transect = case_when(
      transect != 'm_1_e|m_4_n|m_4_s' ~ recodeTransect(transect)
    ),
    location = as.factor(location)
  ) %>% 
  select(-sample_id)


writeAttributes(water_quality) # write data frame attributes to a csv in current dir to edit metadata

water_quality_desc <- "Tres Rios: water_quality. Measurements of water quality taken in situ and from grab samples collected within study transects at the Tres Rios wetlands."

water_quality_factors <- factorsToFrame(water_quality)

water_quality_DT <- createDTFF(dfname = water_quality,
                               factors = water_quality_factors,
                               description = water_quality_desc,
                               dateRangeField = 'date')



# title and abstract ----
title <- 'Long-term monitoring and research of the ecology of the Tres Rios constructed treatment wetland, Phoenix, AZ, ongoing since 2011'

# abstract from file or directly as text
abstract <- as(set_TextType("tr_abstract.md"), "abstract") 


# people ----

# creators
danChilders <- addCreator('d', 'childers')
chrisSanchez <- addCreator('c', 'sanchez')
nichWeller <- addCreator('n', 'weller')

creators <- c(as(danChilders, 'creator'),
              as(chrisSanchez, 'creator'),
              as(nichWeller, 'creator'))

# metadata providers
chrisSanchez <- addMetadataProvider('c', 'sanchez')

metadataProvider <-c(as(chrisSanchez, 'metadataProvider'))


# keywords ----

# CAP IRTs for reference: https://sustainability.asu.edu/caplter/research/
# be sure to include these as appropriate

keywordSet <-
  c(new("keywordSet",
        keywordThesaurus = "LTER controlled vocabulary",
        keyword =  c("wetlands",
                     "biomass",
                     "evaporation",
                     "evapotranspiration",
                     "primary productivity",
                     "water quality",
                     "nutrients",
                     "nitrogen",
                     "phosphorus",
                     "gas flux",
                     "transpiration")),
    new("keywordSet",
        keywordThesaurus = "LTER core areas",
        keyword =  c("primary production",
                     "movement of organic matter",
                     "movement of inorganic matter",
                     "water and fluxes",
                     "parks and rivers")),
    new("keywordSet",
        keywordThesaurus = "Creator Defined Keyword Set",
        keyword =  c("tres rios",
                     "treatment wetlands",
                     "constructed wetlands")),
    new("keywordSet",
        keywordThesaurus = "CAPLTER Keyword Set List",
        keyword =  c("cap lter",
                     "cap",
                     "caplter",
                     "central arizona phoenix long term ecological research",
                     "arizona",
                     "az",
                     "phoenix",
                     "arid land"))
    )

# methods and coverages ----
methods <- set_methods("tr_methods.md")

begindate <- min(
  min(primary_productivity$date),
  min(transpiration$date),
  min(water_quality$date)
)
enddate <- max(
  max(primary_productivity$date),
  max(transpiration$date),
  max(water_quality$date)
)
geographicDescription <- "CAP LTER study area"
coverage <- set_coverage(begin = begindate,
                         end = enddate,
                         sci_names = c('Typha latifolia',
                                       'Typha domingensis',
                                       'Schoenoplectus americanus',
                                       'Schoenoplectus acutus',
                                       'Schoenoplectus tabernaemontani',
                                       'Schoenoplectus maritimus',
                                       'Schoenoplectus californicus'),
                         geographicDescription = geographicDescription,
                         west = -111.949, east = -111.910,
                         north = +33.437, south = +33.430)

# project ----

# construct the dataset ----

# from sourced file:
  # address
  # publisher
  # contact
  # rights
  # distribution

# DATASET
dataset <- new("dataset",
               title = title,
               creator = creators,
               pubDate = pubDate,
               metadataProvider = metadataProvider,
               intellectualRights = rights,
               abstract = abstract,
               keywordSet = keywordSet,
               coverage = coverage,
               contact = contact,
               methods = methods,
               distribution = metadata_dist,
               dataTable = c(typha_leaves_DT,
                             plant_attributes_DT,
                             
                             ))
               # otherEntity = c(core_arthropod_locations)) # if other entity is relevant

# ls(pattern= "_DT") # can help to pull out DTs

# construct the eml ----

# CUSTOM UNITS
# standardUnits <- get_unitList()
# unique(standardUnits$unitTypes$id) # unique unit types

custom_units <- rbind(
  data.frame(id = "micromolesPerMeterSquaredPerSecond",
             parentSI = "molesPerMeterSquaredPerSecond",
             unitType = "arealAmountOfSubstanceConcentrationRate",
             multiplierToSI = "1000000",
             description = ""),
  data.frame(id = "molesPerMeterSquaredPerSecond",
             parentSI = "molesPerMeterSquaredPerSecond",
             unitType = "arealAmountOfSubstanceConcentrationRate",
             multiplierToSI = "1",
             description = ""),
  data.frame(id = "micromolesPerMole",
             parentSI = "",
             unitType = "",
             multiplierToSI = "",
             description = ""),
  data.frame(id = "millimolesPerMeterSquarePerSecond",
             parentSI = "molesPerMeterSquaredPerSecond",
             unitType = "arealAmountOfSubstanceConcentrationRate",
             multiplierToSI = "",
             description = ""),
  data.frame(id = "micromolesPerSecond",
             parentSI = "molePerSecond",
             unitType = "mole flow rate",
             multiplierToSI = "",
             description = "")
)
unitList <- set_unitList(custom_units)

# note schemaLocation is new, not yet tried!
eml <- new("eml",
           schemaLocation = "eml://ecoinformatics.org/eml-2.1.1  http://nis.lternet.edu/schemas/EML/eml-2.1.1/eml.xsd",
           packageId = packageIdent,
           scope = "system",
           system = "knb",
           access = lter_access,
           dataset = dataset,
           additionalMetadata = as(unitList, "additionalMetadata"))


# write the xml to file ----
write_eml(eml, "knb-lter-cap.633.2.xml")


# S3 functions ----

# misc commands
 
  # get list of buckets
  # bucketlist()
  # 
  # add an object to S3 - datasets
  # put_object(file = '649_maintenance_log_dd68e293482738ac6f05303d473687a2.csv',
  #            object = '/datasets/cap/649_maintenance_log_dd68e293482738ac6f05303d473687a2.csv',
  #            bucket = 'gios-data')
  # 
  # add an object to S3 - metadata
  # put_object(file = '~/Dropbox/development/knb-lter-cap.650.1/knb-lter-cap.650.1.xml',
  #            object = '/metadata/knb-lter-cap.650.1.xml',
  #            bucket = 'gios-data')
  # 
  # get files in the gios-data bucket with the prefix datasets/cap/650
  # get_bucket(bucket = 'gios-data',
  #            prefix = 'datasets/cap/650')

# data file to S3
dataToAmz <- function(fileToUpload) {
  
  put_object(file = fileToUpload,
             object = paste0('/datasets/cap/', basename(fileToUpload)),
             bucket = 'gios-data') 
  
}

# example
# dataToAmz('~/Dropbox/development/knb-lter-cap.650.1/CAP 30m Landsat Series Submit/650_CAP_1985_0c95b18e82df5eb0302a46e5967bb1e1.zip')


# metadata file to S3
emlToAmz <- function(fileToUpload) {
  
  put_object(file = fileToUpload,
             object = paste0('/metadata/', basename(fileToUpload)),
             bucket = 'gios-data') 
  
}

# example
# emlToAmz('~/localRepos/cap-data/cap-data-eml/knb-lter-cap.650.1.xml')