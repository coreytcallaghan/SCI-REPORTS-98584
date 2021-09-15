## This is an R script to rectify any taxonomic issues between
## the eBird and inaturalist data
## the goal here is to create a 'key' 

## packages
library(readr)
library(dplyr)
library(tidyr)


# get a list of distinct inat species
inat_species <- readRDS("Data/clean_inat_data.RDS") %>%
  dplyr::select(species) %>%
  distinct()


# get a list of distinct eBird species
ebird_species <- readRDS("Data/clean_ebird_data.RDS") %>%
  ungroup() %>%
  dplyr::select(SCIENTIFIC_NAME) %>%
  rename(species = SCIENTIFIC_NAME) %>%
  distinct()

# join the two species lists
joined_species <- ebird_species %>%
  mutate(matched="yes") %>%
  right_join(., inat_species, by="species") %>%
  replace_na(list(matched="no"))

# filter out any species which matched
matched_species <- joined_species %>%
  dplyr::filter(matched=="yes")

non_matched_species <- joined_species %>%
  dplyr::filter(matched=="no")

write_csv(non_matched_species, "Data/non_matched_species.csv")
write_csv(non_matched_species, "Data/non_matched_species_fixed.csv")

# Now I manually added a 'fix' for each species
# so here I read these data back in
matched_species_fixed <- read_csv("Data/non_matched_species_fixed.csv")

species_lookup_key <- matched_species %>%
  mutate(ebird_species=species) %>%
  bind_rows(matched_species_fixed)

saveRDS(species_lookup_key, "Data/taxonomic_key.RDS")








