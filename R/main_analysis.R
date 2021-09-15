## This is an R script to analyze biases/preferences of citizen scientists
## from an opportunistic citizen science program (iNaturalist)
## compared with a semi-structured citizen science program (eBird)
## the two key dfs read in here have been created in a suite of other R scripts
## I'll start with some overall summary statistics which will probably kick-off the results
## and then do some models
## and make some figures along the way that may be either supplementary or main paper figures


# packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(tibble)
library(scales)
library(broom)
library(patchwork)
library(rphylopic)
library(png)
library(grid)
library(lme4)
library(lmerTest)

# read data in
response <- readRDS("Data/response_variables_df.RDS")
predictors <- readRDS("Data/predictor_variables_df.RDS")


#### summary stuff of the response variables ####
# number of total species found in eBird
length(unique(response$COMMON_NAME))

# number of species found on greater than
# 1% of checklists
response %>%
  ungroup() %>%
  dplyr::filter(ebird_percent_checklists>=1) %>%
  dplyr::select(COMMON_NAME) %>%
  distinct() %>%
  nrow()

# number of species found on greater than
# 5% of checklists
response %>%
  ungroup() %>%
  dplyr::filter(ebird_percent_checklists>=5) %>%
  dplyr::select(COMMON_NAME) %>%
  distinct() %>%
  nrow()

# number of total species found in iNaturalist
response %>%
  ungroup() %>%
  dplyr::filter(inat_presence==1) %>%
  dplyr::select(COMMON_NAME) %>%
  distinct() %>%
  nrow()

# there is some weird taxonomy that I could not fix
# for example Arizona Woodpecker is in iNaturalist
# with the same scientific name as eBird
# but GBIF has a different scientific name
# and for some reason it thus does not appear in the iNat download from GBIF
# as such, any species that have 0 observations
# from iNat I am assuming are some weird taxonomic edge cases
# and looking at it, it does appear this may be the case
# therefore I am removing them here
species_to_remove <- response %>%
  ungroup() %>%
  dplyr::filter(ebird_percent_checklists >=1) %>%
  group_by(COMMON_NAME) %>%
  summarize(ebird_obs=sum(ebird_species_obs),
            inat_obs=sum(inat_species_obs)) %>%
  dplyr::filter(inat_obs==0) %>%
  dplyr::select(COMMON_NAME) %>%
  distinct()

response2 <- response %>%
  dplyr::filter(!COMMON_NAME %in% species_to_remove$COMMON_NAME)

# are the observations of a species
# grouped across all states
# correlated between eBird and iNaturalist
# do this for all eBird species
# get R2 of relationship
r2_all <- response2 %>%
  ungroup() %>%
  group_by(COMMON_NAME) %>%
  summarize(ebird_obs=sum(ebird_species_obs),
            inat_obs=sum(inat_species_obs)) %>%
  lm(log10(inat_obs+1) ~ log10(ebird_obs), data=.) %>%
  summary() %>%
  .$adj.r.squared %>%
  round(digits=1)

species_all <- response2 %>%
  ungroup() %>%
  group_by(COMMON_NAME) %>%
  summarize(ebird_obs=sum(ebird_species_obs),
            inat_obs=sum(inat_species_obs)) %>%
  ggplot(., aes(x=ebird_obs, y=inat_obs+1))+
  geom_point(color="blue")+
  xlab("eBird observations")+
  ylab("iNaturalist observations")+
  scale_x_log10(labels=comma)+
  scale_y_log10(labels=comma)+
  geom_smooth(method="lm", color="orange")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  annotate("text", x = 10, y = 18000, label = paste0("adjusted r squared: ", r2_all))+
  ggtitle("a) all eBird species included")+
  theme(plot.margin = margin(10, 20, 20, 10))

species_all

# and for species which are found on >1% of eBird checklists
r2_1 <- response2 %>%
  ungroup() %>% 
  dplyr::filter(ebird_percent_checklists >=1) %>%
  group_by(COMMON_NAME) %>%
  summarize(ebird_obs=sum(ebird_species_obs),
            inat_obs=sum(inat_species_obs)) %>%
  lm(log10(inat_obs+1) ~ log10(ebird_obs), data=.) %>%
  summary() %>%
  .$adj.r.squared %>%
  round(digits=1)

species_1 <- response2 %>%
  ungroup() %>%
  dplyr::filter(ebird_percent_checklists >=1) %>%
  group_by(COMMON_NAME) %>%
  summarize(ebird_obs=sum(ebird_species_obs),
            inat_obs=sum(inat_species_obs)) %>%
  ggplot(., aes(x=ebird_obs, y=inat_obs))+
  geom_point(color="blue")+
  xlab("eBird observations")+
  ylab("iNaturalist observations")+
  scale_x_log10(labels=comma)+
  scale_y_log10(labels=comma)+
  geom_smooth(method="lm", color="orange")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  annotate("text", x = 2000, y = 18000, label = paste0("adjusted r squared: ", r2_1))+
  ggtitle("b) eBird species on >1% of checklists included")+
  theme(plot.margin = margin(10, 20, 20, 10))

species_1

species_all + species_1 + plot_layout(ncol=1)

ggsave("Figures/comparison_of_ebird_and_inat_total_obs.png", height=6.8, width=7.2, units="in")

# histogram of ebird species counts
options(scipen=10000)
ebird_hist <- response2 %>%
  ungroup() %>%
  dplyr::filter(ebird_percent_checklists >=1) %>%
  group_by(COMMON_NAME) %>%
  summarize(ebird_obs=sum(ebird_species_obs),
            inat_obs=sum(inat_species_obs)) %>%
  ggplot(., aes(x=ebird_obs))+
  geom_histogram(color="black", fill="green", bins=50)+
  xlab("eBird observations")+
  ylab("Number of species")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))

# histogram of inat species counts
inat_hist <- response2 %>%
  ungroup() %>%
  dplyr::filter(ebird_percent_checklists >=1) %>%
  group_by(COMMON_NAME) %>%
  summarize(ebird_obs=sum(ebird_species_obs),
            inat_obs=sum(inat_species_obs)) %>%
  ggplot(., aes(x=inat_obs))+
  geom_histogram(color="black", fill="green", bins=50)+
  xlab("iNaturalist observations")+
  ylab("Number of species")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))

ebird_hist + inat_hist + plot_layout(ncol=2)

ggsave("Figures/ebird_and_inat_hist_of_obs.png", height=5, width=7.8, units="in")

# plot the spatial relationship between
# number of checklists in each state
# and the total number of inaturalist observations
state_obs <- response %>%
  ungroup() %>%
  dplyr::select(STATE, ebird_number_checklists, inat_total_obs) %>%
  distinct() %>%
  ggplot(., aes(x=ebird_number_checklists, y=inat_total_obs))+
  geom_point(color="blue")+
  xlab("eBird checklists")+
  ylab("iNaturalist observations")+
  scale_x_log10(labels=comma)+
  scale_y_log10(labels=comma)+
  geom_smooth(method="lm", color="orange")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  ggtitle("a) observations per state (N=49)")

state_obs

ggsave("Figures/state_comparison_of_observations.png", height=5, width=6.2, units="in")

response %>%
  ungroup() %>%
  dplyr::select(STATE, ebird_number_checklists, inat_total_obs) %>%
  distinct() %>%
  lm(ebird_number_checklists ~ inat_total_obs, data=.) %>%
  summary()

species_obs <- response2 %>%
  ungroup() %>%
  dplyr::filter(ebird_percent_checklists >=1) %>%
  group_by(COMMON_NAME) %>%
  summarize(ebird_obs=sum(ebird_species_obs),
            inat_obs=sum(inat_species_obs)) %>%
  ggplot(., aes(x=ebird_obs, y=inat_obs))+
  geom_point(color="blue")+
  xlab("eBird observations")+
  ylab("iNaturalist observations")+
  scale_x_log10(labels=comma)+
  scale_y_log10(labels=comma)+
  geom_smooth(method="lm", color="orange")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  ggtitle("b) observations per species (N=507)")+
  theme(plot.margin = margin(10, 20, 20, 10))

species_obs

state_obs + species_obs + plot_layout(ncol=1)

ggsave("Figures/summary_of_data_figure.png", height=6.8, width=6.2, units="in")

# summary of final dataset
response3 <- response2 %>%
  ungroup() %>%
  dplyr::filter(ebird_percent_checklists >=1) %>%
  group_by(COMMON_NAME) %>%
  summarize(ebird_obs=sum(ebird_species_obs),
            inat_obs=sum(inat_species_obs))

# total number of species
length(unique(response3$COMMON_NAME)) 

# total number of eBird obs
sum(response3$ebird_obs)

# total number of iNat obs
sum(response3$inat_obs)

# plot the relationship between the percent of checklists a species is found on
# and the percent of total observations a species comprises
# summarized at the 'state' level
response %>%
  ggplot(., aes(x=ebird_percent_checklists, y=inat_percent_obs, group=STATE))+
  geom_smooth(method="lm", se=FALSE, color="gray30", size=0.8)+
  xlab("eBird checklist %")+
  ylab("iNaturalist observation %")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))

ggsave("Figures/percent_vs_percent_state_relationships.png", width=5.2, height=5.5, units="in")

#########################################################################
#########################################################################
#### Look at relationships between predictors and response variables ####
#########################################################################
#########################################################################

# join the response and predictor variables
data <- response2 %>%
  left_join(., predictors) %>%
  dplyr::filter(ebird_percent_checklists >=1)


# pick a state as an example (NY, since that's where I'm from)
# and work through a few things to get a feel for models to run
ny <- data %>%
  dplyr::filter(STATE == "New York")

# lets try extracting residuals for all species in NY (from eBird)
# but use a log +1 for iNat obs since there are lots of 0s, but this still should work the same
# as a reminder this is what the plot would look like
ggplot(ny, aes(x=ebird_species_obs, y=inat_species_obs))+
  geom_point(color="blue")+
  xlab("eBird observations")+
  ylab("iNaturalist observations")+
  scale_x_log10(labels=comma)+
  scale_y_log10(labels=comma)+
  geom_smooth(method="lm", color="orange")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  ggtitle("All species from NY")

# get resids from a model of this
resids <- lm(log(inat_species_obs) ~ log(ebird_species_obs), data=ny)$residuals

# add resids back in with data
ny <- ny %>%
  ungroup() %>%
  mutate(residuals=resids)

# now rerun the models above, with body size
# the relationship between residuals and body mass
# the prediction is that larger birds are more likely to be present in iNat
mod <- lm(residuals ~ log(adult_body_mass_g), data=ny)
summary(mod)

# but there is also the relationship between
# the percent of eBird lists a species is found on
# and the percent of total observations a species comprises in iNat
# for the NY example, this looks like:
ggplot(ny, aes(x=ebird_percent_checklists, y=inat_percent_obs))+
  geom_point(color="blue")+
  xlab("eBird observations")+
  ylab("iNaturalist observations")+
  scale_x_log10(labels=comma)+
  scale_y_log10(labels=comma)+
  geom_smooth(method="lm", color="orange")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  ggtitle("All species from NY")

# it is worth checking that the residuals from this relationship
# would strongly correlate with the residuals from the number of obs relationship
resid.1 <- lm(log(inat_species_obs) ~ log(ebird_species_obs), data=ny)$residuals
resid.2 <- lm(log(inat_percent_obs) ~ log(ebird_percent_checklists), data=ny)$residuals

resid_plot_df <- data.frame(observation_residuals=resid.1,
                            percent_residuals=resid.2)

ggplot(resid_plot_df, aes(x=observation_residuals, y=percent_residuals))+
  geom_point(color="blue")+
  xlab("Observation residuals")+
  ylab("Percent residuals")+
  geom_smooth(method="lm", color="orange")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  ggtitle("All species from NY")

# clearly, there is strong agreement between two theoretical ways to extract residuals
# but log-transforming percentages is likely a bit dodgy. But given the agreement,
# I proceed by using the total number of observations method

# note that we need to add a +1 because some states don't have all the
# inat species sampled that are sampled in eBird
# get residuals for each combination
get_resids_for_plotting <- function(state_name){
  
  dat <- data %>%
    dplyr::filter(STATE==state_name)
  
  resids <- lm(log10(inat_species_obs+1) ~ log10(ebird_species_obs), data=dat)$residuals
  
  dat.2 <- dat %>%
    ungroup() %>%
    mutate(residuals=resids)
  
  return(dat.2)
}

state_list_results <- lapply(unique(data$STATE), function(x){get_resids_for_plotting(x)})
state_resids_results <- bind_rows(state_list_results)

# investigate body size
#small_img <- readPNG("60901e63-aa9c-4d88-804c-0a7ed2afb123.512.png")
#small_g <- rasterGrob(small_img, interpolate=TRUE)
#lg_img <- readPNG("101ad9ab-dae8-442f-b102-8109b02b2a69.512.png")
#lg_g <- rasterGrob(lg_img, interpolate=TRUE)

body_vs_resids <- state_resids_results %>%
  ggplot(., aes(x=adult_body_mass_g, y=residuals))+
  geom_hline(yintercept=0, color="red")+
  scale_x_log10()+
  geom_smooth(data=state_resids_results, aes(x=adult_body_mass_g, y=residuals, group=STATE), method="lm", se=FALSE, color="gray40", size=0.2)+
  geom_smooth(method="lm", color="#FD8D3C", fill="#FDAE6B", 
              linetype="dashed", size=1.4)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  theme(axis.text=element_text(color="black"))+
  xlab("Body size (g)")+
  ylab("Relative iNaturalist observations")+
  ylim(-1, 1)
  #coord_cartesian(clip = 'off')+   # This keeps the labels from disappearing
  #theme(plot.margin = unit(c(1,1,6,1), "lines"))+
  #annotation_custom(grob=small_g, xmin=0.1, xmax=0.8, ymin=-2.5, ymax=1)+
  #annotation_custom(grob=lg_g, xmin=3.3, xmax=4.1, ymin=-3.6, ymax=1.9)

body_vs_resids

ggsave("Figures/body_size_residuals.png", height=4.6, width=6.2, units="in")

flock_vs_resids <- state_resids_results %>%
  ggplot(., aes(x=avg_flock_size, y=residuals))+
  geom_hline(yintercept=0, color="red")+
  scale_x_log10()+
  geom_smooth(data=state_resids_results, aes(x=avg_flock_size, y=residuals, group=STATE), method="lm", se=FALSE, color="gray40", size=0.2)+
  geom_smooth(method="lm", color="#FD8D3C", fill="#FDAE6B", 
              linetype="dashed", size=1.4)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  theme(axis.text=element_text(color="black"))+
  #ylim(-0.6, 0.6)+
  xlab("Flock size")+
  ylab("")+
  ylim(-1, 1)

flock_vs_resids

ggsave("Figures/flock_size_residuals.png", height=4.6, width=6.2, units="in")

color_vs_resids <- state_resids_results %>%
  ggplot(., aes(x=bird_color, y=residuals))+
  geom_hline(yintercept=0, color="red")+
  #scale_x_log10()+
  geom_smooth(data=state_resids_results, aes(x=bird_color, y=residuals, group=STATE), method="lm", se=FALSE, color="gray40", size=0.2)+
  geom_smooth(method="lm", color="#FD8D3C", fill="#FDAE6B", 
              linetype="dashed", size=1.4)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  theme(axis.text=element_text(color="black"))+
  #ylim(-1, 1)+
  xlab("Bird color")+
  ylab("Relative iNaturalist observations")+
  ylim(-1, 1)

color_vs_resids

ggsave("Figures/color_residuals.png", height=4.6, width=6.2, units="in")

iucn_vs_resids <- state_resids_results %>%
  dplyr::filter(complete.cases(IUCN_category)) %>%
  dplyr::filter(IUCN_category != "EN") %>%
  ggplot(., aes(x=IUCN_category, y=residuals))+
  #scale_x_log10()+
  #geom_smooth(method="lm", se=FALSE, color="gray40")+
  #geom_violin(position=position_dodge())+
  geom_boxplot(width=0.2, fill="gray80",outlier.color="transparent")+
  theme_bw()+
  theme(panel.grid=element_blank())+
  geom_line(data=state_resids_results %>%
               dplyr::filter(complete.cases(IUCN_category)) %>%
               dplyr::filter(IUCN_category != "EN") %>%
               group_by(STATE, IUCN_category) %>%
               summarize(mean=mean(residuals)), aes(x=IUCN_category, y=mean, group=STATE), color="gray40", size=0.2)+
  theme(axis.text=element_text(color="black"))+
  scale_x_discrete(limits=c("VU", "NT", "LC"),
                   breaks=c("VU", "NT", "LC"),
                   labels=c("VU", "NT", "LC"))+
  ylab("")+
  xlab("IUCN status")+
  ylim(-1,1)

iucn_vs_resids

ggsave("Figures/color_residuals.png", height=4.6, width=6.2, units="in")


body_vs_resids + flock_vs_resids + color_vs_resids + iucn_vs_resids + plot_layout(ncol=2)


ggsave("Figures/all_residuals_raw_plot.png", height=5.6, width=6.5, units="in")

# investigate body size
body_dat <- state_resids_results %>%
  dplyr::filter(complete.cases(adult_body_mass_g))

summary(lm(residuals ~ log10(adult_body_mass_g), data=body_dat))


#####################################################
#####################################################
# a function to complete the above for every state
# and summarize the results
# i.e., run models to test what (if anything) predicts the residuals
all_species_lm_models_function <- function(state_name){
    
    dat <- data %>%
      dplyr::filter(STATE==state_name)
    
    resids <- lm(log10(inat_species_obs+1) ~ log10(ebird_species_obs), data=dat)$residuals
    
    # run a simple mod for body size only
    dat.2 <- dat %>%
      ungroup() %>%
      mutate(residuals=resids) %>%
      dplyr::select(residuals, adult_body_mass_g, COMMON_NAME) %>%
      dplyr::filter(complete.cases(.))
    
    mod_1 <- lm(residuals ~  log10(adult_body_mass_g), data=dat.2)
    
    summary.1 <- tidy(mod_1) %>%
      mutate(STATE=state_name) %>%
      mutate(number_obs_in_model=nrow(dat.2)) %>%
      mutate(significance=ifelse(p.value<0.05, "Significant", "Not-significant")) %>%
      mutate(adj_r_squared=summary(mod_1)$adj.r.squared) %>%
      mutate(model="body_size_only")
    
    # run a simple mod for color (i.e., distance from brown) only
    dat.3 <- dat %>%
      ungroup() %>%
      mutate(residuals=resids) %>%
      dplyr::select(residuals, bird_color, COMMON_NAME) %>%
      dplyr::filter(complete.cases(.))
    
    mod_2 <- lm(residuals ~ bird_color, data=dat.3)
    
    summary.2 <- tidy(mod_2) %>%
      mutate(STATE=state_name) %>%
      mutate(number_obs_in_model=nrow(dat.3)) %>%
      mutate(significance=ifelse(p.value<0.05, "Significant", "Not-significant")) %>%
      mutate(adj_r_squared=summary(mod_2)$adj.r.squared) %>%
      mutate(model="color_only")
    
    # run a model for flock size
    dat.4 <- dat %>%
      ungroup() %>%
      mutate(residuals=resids) %>%
      dplyr::select(residuals, avg_flock_size, COMMON_NAME) %>%
      dplyr::filter(complete.cases(.))
    
    mod_3 <- lm(residuals ~ log10(avg_flock_size), data=dat.4)
    
    summary.3 <- tidy(mod_3) %>%
      mutate(STATE=state_name) %>%
      mutate(number_obs_in_model=nrow(dat.4)) %>%
      mutate(significance=ifelse(p.value<0.05, "Significant", "Not-significant")) %>%
      mutate(adj_r_squared=summary(mod_2)$adj.r.squared) %>%
      mutate(model="flock_size_only")
    
    # run a model for 'order' as a categorical variable dummy coded
    dat.5 <- dat %>%
      ungroup() %>%
      mutate(residuals=resids) %>%
      dplyr::select(residuals, order, COMMON_NAME) %>%
      dplyr::filter(complete.cases(.))
    
    dummy_code <- dat.5 %>%
      dplyr::select(order) %>%
      distinct() %>%
      mutate(order_dummy=1:nrow(.))
    
    dat.5 <- dat.5 %>%
      left_join(dummy_code)
    
    mod_4 <- lm(residuals ~ order_dummy, data=dat.5)
    
    summary.4 <- tidy(mod_4) %>%
      mutate(STATE=state_name) %>%
      mutate(number_obs_in_model=nrow(dat.5)) %>%
      mutate(significance=ifelse(p.value<0.05, "Significant", "Not-significant")) %>%
      mutate(adj_r_squared=summary(mod_2)$adj.r.squared) %>%
      mutate(model="taxonomic_order_only")
    
    # run a model for IUCN status only
    # run a model for 'order' as a categorical variable dummy coded
    dat.6 <- dat %>%
      ungroup() %>%
      mutate(residuals=resids) %>%
      dplyr::select(residuals, IUCN_category, COMMON_NAME) %>%
      dplyr::filter(complete.cases(.))
    
    dummy_code <- dat.6 %>%
      dplyr::select(IUCN_category) %>%
      distinct() %>%
      mutate(iucn_dummy=1:nrow(.))
    
    dat.6 <- dat.6 %>%
      left_join(dummy_code)
    
    mod_5 <- lm(residuals ~ iucn_dummy, data=dat.6)
    
    summary.5 <- tidy(mod_5) %>%
      mutate(STATE=state_name) %>%
      mutate(number_obs_in_model=nrow(dat.6)) %>%
      mutate(significance=ifelse(p.value<0.05, "Significant", "Not-significant")) %>%
      mutate(adj_r_squared=summary(mod_2)$adj.r.squared) %>%
      mutate(model="iucn_only")
    
    
    # run a mod for color (i.e., distance from brown) AND body size
    dat.all <- dat %>%
      ungroup() %>%
      mutate(residuals=resids) %>%
      dplyr::select(residuals, adult_body_mass_g, 
                    bird_color, order, IUCN_category,
                    avg_flock_size, COMMON_NAME) %>%
      dplyr::filter(complete.cases(.)) %>%
      ungroup()
    
    mod_all <- lm(residuals ~ log10(adult_body_mass_g)*bird_color + IUCN_category +
                    log10(avg_flock_size), data=dat.all)
    
    summary.all <- tidy(mod_all) %>%
      mutate(STATE=state_name) %>%
      mutate(number_obs_in_model=nrow(dat.4)) %>%
      mutate(significance=ifelse(p.value<0.05, "Significant", "Not-significant")) %>%
      mutate(adj_r_squared=summary(mod_3)$adj.r.squared) %>%
      mutate(model="all_variables")
    
    summary <- bind_rows(summary.1,
                         summary.2,
                         summary.3,
                         summary.4,
                         summary.5,
                         summary.all)
    
    return(summary)
  
}

state_list_results <- lapply(unique(data$STATE), function(x){all_species_lm_models_function(x)})
state_list_results <- bind_rows(state_list_results)

# plot some of the results
mu_body_size <- state_list_results %>%
  dplyr::filter(model=="body_size_only") %>%
  dplyr::filter(term == "log10(adult_body_mass_g)") %>%
  summarize(grp.mean=mean(estimate))

body_size <- state_list_results %>%
  dplyr::filter(model=="body_size_only") %>%
  dplyr::filter(term == "log10(adult_body_mass_g)") %>%
  ggplot(., aes(x=estimate))+
  geom_density(fill="gray80", alpha=0.5)+
  geom_histogram(fill='orange', color="black", bins=40)+
  geom_vline(data=mu_body_size, aes(xintercept=grp.mean),
             color="blue", linetype="dashed", size=1)+
  xlab("Parameter estimates")+
  ylab("Number of states")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  ggtitle("Body size")

body_size

ggsave("Figures/body_size_parameter_estimates.png", height=5, width=6.2, units="in")

mu_color <- state_list_results %>%
  dplyr::filter(model=="color_only") %>%
  dplyr::filter(term == "bird_color") %>%
  summarize(grp.mean=mean(estimate))

options(scipen=10000)

color <- state_list_results %>%
  dplyr::filter(model=="color_only") %>%
  dplyr::filter(term == "bird_color") %>%
  ggplot(., aes(x=estimate))+
  geom_density(fill="gray80", alpha=0.5)+
  geom_histogram(fill='orange', color="black", bins=50)+
  geom_vline(data=mu_color, aes(xintercept=grp.mean),
             color="blue", linetype="dashed", size=1)+
  xlab("Parameter estimates")+
  ylab("Number of states")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  ggtitle("Color (distance from brown)")+
  scale_x_continuous()

color

ggsave("Figures/color_parameter_estimates.png", height=5, width=6.2, units="in")

mu_flock_size <- state_list_results %>%
  dplyr::filter(model=="flock_size_only") %>%
  dplyr::filter(term=="log10(avg_flock_size)") %>%
  summarize(grp.mean=mean(estimate))

flock_size <- state_list_results %>%
  dplyr::filter(model=="flock_size_only") %>%
  dplyr::filter(term=="log10(avg_flock_size)") %>%
  ggplot(., aes(x=estimate))+
  geom_density(fill="gray80", alpha=0.5)+
  geom_histogram(fill='orange', color="black", bins=50)+
  geom_vline(data=mu_flock_size, aes(xintercept=grp.mean),
             color="blue", linetype="dashed", size=1)+
  xlab("Parameter estimates")+
  ylab("Number of states")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  ggtitle("Average flock size")+
  scale_x_continuous()

flock_size

ggsave("Figures/flock_size_parameter_estimates.png", height=5, width=6.2, units="in")

mu_iucn <- state_list_results %>%
  dplyr::filter(model=="iucn_only") %>%
  dplyr::filter(term=="iucn_dummy") %>%
  summarize(grp.mean=mean(estimate))

iucn <- state_list_results %>%
  dplyr::filter(model=="iucn_only") %>%
  dplyr::filter(term=="iucn_dummy") %>%
  ggplot(., aes(x=estimate))+
  geom_density(fill="gray80", alpha=0.5)+
  geom_histogram(fill='orange', color="black", bins=50)+
  geom_vline(data=mu_iucn, aes(xintercept=grp.mean),
             color="blue", linetype="dashed", size=1)+
  xlab("Parameter estimates")+
  ylab("Number of states")+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  ggtitle("IUCN status")+
  scale_x_continuous()

iucn

ggsave("Figures/iucn_parameter_estimates.png", height=5, width=6.2, units="in")

body_size + color + flock_size + iucn + ncol(2)

ggsave("Figures/state_level_estimates_all_four_predictors.png", height=5, width=6.2, units="in")

# Those are individual models for each state
# but what does a lmer model show
# where state is treated as a random effect
body_dat <- state_resids_results %>%
  dplyr::select(STATE, COMMON_NAME, adult_body_mass_g, residuals) %>%
  dplyr::filter(complete.cases(.))

body_big_mod <- lmer(residuals ~ log10(adult_body_mass_g) + (1|STATE), data=body_dat)
summary(body_big_mod)


color_dat <- state_resids_results %>%
  dplyr::select(STATE, COMMON_NAME, bird_color, residuals) %>%
  dplyr::filter(complete.cases(.))

color_big_mod <- lmer(residuals ~ bird_color + (1|STATE), data=color_dat)
summary(color_big_mod)

# difference between brightness and distance?
color_dat2 <- state_resids_results %>%
  dplyr::select(STATE, COMMON_NAME, max_distance, max_brightness, bird_color, residuals) %>%
  dplyr::filter(complete.cases(.))

color_mod_dist <- lmer(residuals ~ max_distance + (1|STATE), data=color_dat2)
summary(color_mod_dist)

color_mod_brightness <- lmer(residuals ~ max_brightness + (1|STATE), data=color_dat2)
summary(color_mod_brightness)

flock_dat <- state_resids_results %>%
  dplyr::select(STATE, COMMON_NAME, avg_flock_size, residuals) %>%
  dplyr::filter(complete.cases(.))

flock_big_mod <- lmer(residuals ~ log10(avg_flock_size) + (1|STATE), data=flock_dat)
summary(flock_big_mod)

iucn_dat <- state_resids_results %>%
  dplyr::select(STATE, COMMON_NAME, IUCN_category, residuals) %>%
  dplyr::filter(complete.cases(.)) %>%
  dplyr::filter(IUCN_category != "EN") %>%
  mutate(IUCN_ordinal=case_when(IUCN_category=="LC" ~ 3,
                                IUCN_category=="NT" ~ 2,
                                IUCN_category=="VU" ~ 1))

iucn_big_mod <- lmer(residuals ~ IUCN_ordinal + (1|STATE), data=iucn_dat)
summary(iucn_big_mod)

all_dat <- state_resids_results %>%
  dplyr::select(STATE, COMMON_NAME, avg_flock_size, bird_color, adult_body_mass_g, IUCN_category, residuals) %>%
  dplyr::filter(IUCN_category != "EN") %>%
  mutate(IUCN_ordinal=case_when(IUCN_category=="LC" ~ 3,
                                IUCN_category=="NT" ~ 2,
                                IUCN_category=="VU" ~ 1)) %>%
  dplyr::filter(complete.cases(.))

all_big_mod <- lmer(residuals ~ log10(avg_flock_size) + IUCN_ordinal + 
                      log10(adult_body_mass_g) + bird_color + (1|STATE), data=all_dat)
summary(all_big_mod)

plot_dat <- summary(all_big_mod)$coefficients %>%
  as.data.frame() %>%
  rownames_to_column(var="term") %>%
  bind_cols(confint(all_big_mod) %>% 
              as.data.frame() %>%
              slice(3:7)) %>%
  dplyr::filter(term != "(Intercept)") %>%
  mutate(term_clean=case_when(term=="log10(avg_flock_size)" ~ "Average flock size (log10)",
                              term=="bird_color" ~ "Bird color",
                              term=="IUCN_ordinal" ~ "IUCN status",
                              term=="log10(adult_body_mass_g)" ~ "Body size (log10)"))


ggplot(plot_dat, aes(x=term_clean, y=Estimate))+
  geom_point()+
  geom_errorbar(aes(ymin=`2.5 %`, ymax=`97.5 %`, width=0))+
  theme_bw()+
  theme(axis.text=element_text(color="black"))+
  xlab("")+
  ylab("Parameter estimate")+
  geom_hline(yintercept=0, color="red")+
  coord_flip()+
  ylim(-0.1, 0.1)+
  scale_x_discrete(labels=c("Bird color", "IUCN status", "Body size (log10)", "Average flock size (log10)"),
                   breaks=c("Bird color", "IUCN status", "Body size (log10)", "Average flock size (log10)"),
                   limits=c("Bird color", "IUCN status", "Body size (log10)", "Average flock size (log10)"))

ggsave("Figures/mixed_models_results.png", width=6.8, height=4.8, units="in")


# IS FLOCK SIZE and body size correlated?
all_dat %>%
  dplyr::select(COMMON_NAME, avg_flock_size, adult_body_mass_g) %>%
  distinct() %>%
  lm(log(avg_flock_size) ~ log(adult_body_mass_g), data=.) %>%
  summary()





# Make a table for the paper
# highlighting the number of observations in each model
table_1 <- state_list_results %>%
  dplyr::select(STATE, model, number_obs_in_model) %>%
  dplyr::distinct() %>%
  group_by(model) %>%
  summarize(mean_N=mean(number_obs_in_model),
            sd_N=sd(number_obs_in_model)) %>%
  dplyr::filter(model %in% c("color_only", "body_size_only", "flock_size_only", "iucn_only")) %>%
  mutate(mixed_effects_body=nrow(body_dat)) %>%
  mutate(mixed_effects_color=nrow(color_dat)) %>%
  mutate(mixed_effects_flock=nrow(flock_dat)) %>%
  mutate(mixed_effects_iucn=nrow(iucn_dat)) %>%
  mutate(mixed_effects_all=nrow(all_dat))

length(unique(body_dat$COMMON_NAME))
length(unique(color_dat$COMMON_NAME))
length(unique(flock_dat$COMMON_NAME))
length(unique(iucn_dat$COMMON_NAME))

write_csv(table_1, "Tables/table_1_scratch.csv")




#################################################################
######################### END ###################################


# plot map of effects in space
us_states <- map_data("state") %>%
  left_join(., state_list_results %>%
              dplyr::filter(term=="log10(avg_flock_size)") %>%
              dplyr::filter(model=="flock_size_only") %>%
              mutate(region=tolower(STATE)))

ggplot(data = us_states,
       mapping = aes(x = long, y = lat,
                     group = group, fill = estimate))+
  geom_polygon()+
  theme_bw()+
  scale_fill_viridis_c()+
  ggtitle("Flock size parameter estimate")


