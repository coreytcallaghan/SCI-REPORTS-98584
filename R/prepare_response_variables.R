## This is an R script to prepare the response variables
## and create one dataframe with the potential necessary response
## variables, including both eBird and iNat data

## packages
library(readr)
library(dplyr)
library(tidyr)

# read data in
inat <- readRDS("Data/clean_inat_data.RDS")
ebird <- readRDS("Data/clean_ebird_data.RDS")
taxonomic_key <- readRDS("Data/taxonomic_key.RDS")

# prepare inat data to be joined with eBird
inat.2 <- inat %>%
  left_join(., taxonomic_key, by="species") %>%
  dplyr::select(-matched, - species) %>%
  dplyr::filter(complete.cases(ebird_species)) %>%
  rename(SCIENTIFIC_NAME = ebird_species) %>%
  rename(STATE=stateProvince) %>%
  rename(inat_species_obs=species_obs) %>%
  rename(inat_total_obs=total_obs) %>%
  rename(inat_percent_obs=percent_obs) %>%
  rename(inat_presence=presence) %>% 
  dplyr::select(1, 6, 2:5)

# prepare ebird data to be joined with inat
ebird.2 <- ebird %>%
  rename(ebird_species_obs=number_obs) %>%
  rename(ebird_number_checklists=number_checklists) %>%
  rename(ebird_percent_checklists=percent_lists) %>%
  dplyr::select(5, 1:4, 6, 8, 7)

# join data to create a dataframe for response variables
response_variables <- ebird.2 %>%
  left_join(., inat.2) %>%
  replace_na(list(inat_species_obs=0,
                  inat_presence=0,
                  inat_percent_obs=0)) %>%
  arrange(STATE, inat_total_obs) %>% 
  fill(inat_total_obs) %>%
  arrange(STATE, ebird_species_obs)

saveRDS(response_variables, "Data/response_variables_df.RDS")


