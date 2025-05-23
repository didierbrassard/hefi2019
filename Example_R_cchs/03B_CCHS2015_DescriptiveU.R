#' ---
#' title: "Descriptive statistics based on repeated 24-h recall: HEFI-2019 scores using the NCI multivariate method - results"
#' author: Didier Brassard
#' date: "`r Sys.Date()`"
#' output: github_document
#' ---

# *********************************************************************** #
#                                                                         #
#                      CCHS 2015 - Nutrition (PUMF)                       #
#                                                                         #
#          Descriptive statistics based on repeated 24-h recall           #
#                                                                         #
#                        Author: Didier Brassard                          #
#                                                                         #
#                              Version 1.0                                #
#                               2025-05-23                                #
#                                                                         #
# NOTE: This code assumes that <03A_CCHS2015_DescriptiveU.R>              #
# was executed beforehand.                                                #
#                                                                         #
# *********************************************************************** #

#### General set-up: packages and location of files ####
# *********************************************************************** #
#                                                                         #
#             General set-up: packages and location of files              #
#                                                                         #
# *********************************************************************** #

#' Note 1: data pertaining to the 2015 Canadian Community Health Survey -
#' Nutrition (Public Use Microdata Files) are available upon request to
#' Statistics Canada online: https://www150.statcan.gc.ca/n1/en/catalogue/82M0024X2018001

# Automatic: create shortcuts for project directory tree

## Common directory
dir_example <- here::here("Example_R_cchs")

## For the NCI output
dir_nci <- file.path(dir_example, "data", "nci_mcmc")
dir_nci_original <- file.path(dir_nci, "original")
dir_nci_output <- file.path(dir_nci, "output")
dir_nci_bootstrap <- file.path(dir_nci,"bootstrap")
### create folder, if need be
if(dir.exists(dir_nci_output)==FALSE){
  dir.create(dir_nci_output, recursive = TRUE)
}

# Packages

## data management
library(data.table)
library(dplyr)
library(tidylog)
library(tidyfast)

## NCI multivariate method
library(ncimultivar)

## computing
library(tictoc)

## presentation
library(knitr)
library(ggplot2)
library(scales)

## HEFI-2019 scoring algorithm
if(!require("devtools")) install.packages("devtools")
if(!require("hefi2019")) devtools::install_github("didierbrassard/hefi2019"); library(hefi2019)

# suppress scientific notation
options(scipen = 9999)

#### Load/prepare simulation data, apply HEFI-2019 score, get distribution ####
# *********************************************************************** #
#                                                                         #
# Load/prepare simulation data, apply HEFI-2019 score, get distribution   #
#                                                                         #
# *********************************************************************** #

#' goal: generate all statistics of interest using simulation data
#'
#' note: if disk space is an issue, the present script can be added at the end of `03A_CCHS2015_DescriptiveU.R`.
#' Once **all** statistics of interest are calculated and saved, the simulation data `mcmc_sim*.rdata` can be
#' deleted. However, if additional statistics are needed, a new run of `ncimultivar::nci_multivar_distrib`
#' will be necessary to re-create all simulation data and output the additional statistics. Similarly,
#' if the model objects `mcmc_model*.rdata` have been deleted, a complete re-run including `ncimultivar::nci_multivar_mcmc`
#' will be required to generate any additional statistics that were not originally saved.
#' For these reasons, all models and simulation data are saved in this example at the expense of disk space.
#' A small price to pay for salvation...

# (manual) indicate number of replicate to run

replicfirst <- 0
repliclast  <- 5

#' note: to run only base/original estimates set both 'replicfirst' and 'repliclast' to `0`

# (auto) load vectors to rename variables
load(file = file.path(dir_nci_original, "predefined_vectors.rdata"))

# (auto) calculate number of replicate to derive list length
num.replicates <- ifelse(replicfirst==0, repliclast-replicfirst, repliclast-replicfirst+1)

# (auto) create vector for bootstrap data
distrib_diet_bootstrap <- vector(mode="list", length=num.replicates)

# (auto) log analysis time
tictoc::tic(glue::glue("NCI MCMC post-processing replicate {replicfirst} to {repliclast}"))

# (auto) lunch bootstrap loop
for (replicate.num in replicfirst:repliclast) {

  print(paste0("Post-NCI processing for iteration ", replicate.num))

  #####  Apply preliminary formatting #####
  # ********************************************** #
  #            Apply prelim. formatting            #
  # ********************************************** #

  # note: everything below is replicate-specific

  if(replicate.num==0){

    load(file.path(dir_nci_original,paste0("mcmc_sim",replicate.num,".rdata")))

  } else {

    load(file.path(dir_nci_bootstrap,paste0("mcmc_sim",replicate.num,".rdata")))

  }

  # convert to data.table to improve processing speed
  usintake_mc_t_out <- data.table::as.data.table(mcmc_sim); rm(mcmc_sim)

  # Generate vectors of variables: 'usual.intake', 'prob' and 'amount' variables
  vars_prob <- paste0("prob.",vars_diet_episodic)
  vars_amt <- paste0("amount.",vars_diet_episodic)
  to_drop <- c(vars_prob, vars_amt)
  vars_mc_name <- paste0("usual.intake.", vars_diet_all)

  # Drop variables specified in the vector
  data.table::set(usintake_mc_t_out, j = to_drop, value = NULL)

  # Rename variable names so they are consistent with input dataset + suffix '_u'
  data.table::setnames(usintake_mc_t_out,
                       old = c(vars_mc_name, "replicate"),
                       new = c(vars_diet_all,"replicate_mcmc"),
                       skip_absent =TRUE)

  #note: hardcoded 'replicate' name change to 'replicate_mcmc', avoid confusing with bootstrap 'replicate' id.

  # "Build back" sex/age group variables
  usintake_mc_t_out[, sex :=  tidyfast::dt_case_when (
    drig_f_9_13==0 & drig_f_14_18==0 ~ 1,
    drig_f_9_13==1 | drig_f_14_18==1 ~ 2
  )]

  usintake_mc_t_out[, sex_f:=
                      factor(sex,
                             levels =c(1,2),
                             labels=c("Boy", "Girl")
                      )]

  usintake_mc_t_out[,dri_group :=  tidyfast::dt_case_when (
    drig_f_14_18==1 ~ 7,
    drig_m_14_18==1 ~ 6,
    drig_f_9_13 ==1 ~ 5,
    TRUE ~ 4
  )]

  ## confirm recoding
  if(replicate.num==0) print(with(usintake_mc_t_out, table(sex_f, dri_group)))

  ##### Apply HEFI-2019 score #####
  # ********************************************** #
  #             Apply HEFI-2019 score              #
  # ********************************************** #

  # note: 'unsweetplantbevpro' was not reported in this sample, set to 0 for all respondents

  usintake_mc_t_out[,unsweetplantbevpro := 0]

  # Scoring algorithm
  usintake_mc_t_out<-
    hefi2019(indata           = usintake_mc_t_out,
           vegfruits          = "vf",
           wholegrfoods       = "wg",
           nonwholegrfoods    = "rg",
           profoodsanimal     = "pfab",
           profoodsplant      = "pfpb",
           otherfoods         = "otherfoods",
           waterhealthybev    = "water",
           unsweetmilk        = "milk",
           unsweetplantbevpro = "unsweetplantbevpro",
           otherbeverages     = "otherbevs" ,
           mufat              = "mufa" ,
           pufat              = "pufa" ,
           satfat             = "sfa" ,
           freesugars         = "freesugars",
           sodium             = "sodium",
           energy             = "energy")

  # create vector of HEFI-2019 score variables
  vars_diet_hefi <- grep("HEFI2019", names(usintake_mc_t_out),value=TRUE)
  if(replicate.num==0) print(vars_diet_hefi)

  ##### Get distribution with 'nci_multivar_summary' #####
  # ********************************************** #
  #  Get distribution with 'nci_multivar_summary'  #
  # ********************************************** #

  # indicate which percentile values to generate
  quantiles_to_output <- seq(1,99)/100

  # full sample
  distrib_all <-
    ncimultivar::nci_multivar_summary(
      input.data   = usintake_mc_t_out,
      variables    = c(vars_diet_all,vars_diet_hefi),
      weight       = paste0("BSW", replicate.num),
      do.means     = TRUE,
      do.quantiles = TRUE,
      quantiles    = quantiles_to_output)

  # by characteristic

  # (manual) define subgroup variable
  subgroup_var <- "sex_f"

  # (auto) output level and generate distribution statistics
  subgroup_var_level <- unique(usintake_mc_t_out[[subgroup_var]])

  # (auto) create list given levels
  distrib_subgroup <- vector(mode="list", length=length(subgroup_var_level))

  # (auto) generate summary statistics for each
  for (i in seq_along(subgroup_var_level)){

    distrib_subgroup[[i]] <-
      ncimultivar::nci_multivar_summary(
        input.data      = usintake_mc_t_out,
        row.subset      = usintake_mc_t_out[[subgroup_var]]==subgroup_var_level[i],
        population.name = subgroup_var_level[i],
        variables       = c(vars_diet_all,vars_diet_hefi),
        weight          = paste0("BSW", replicate.num),
        do.means        = TRUE,
        do.quantiles    = TRUE,
        quantiles       = quantiles_to_output)
  } # end of subgroup loop

  # make one data frame with all subgroups instead
  distrib_subgroup_df <- do.call(rbind, distrib_subgroup); rm(distrib_subgroup)

  # calculate difference between the TWO groups
  if(replicate.num==0) print(paste0("Distribution estimate difference calculated for ",subgroup_var,": ", subgroup_var_level[length(subgroup_var_level)]," vs. ", subgroup_var_level[1]))

  distrib_subgroup_df_diff <-
    ncimultivar::summary_difference(subset(distrib_subgroup_df, population==subgroup_var_level[length(subgroup_var_level)]),
                                    subset(distrib_subgroup_df, population==subgroup_var_level[1]))

  # append difference estimates
  distrib_subgroup_df <- rbind(distrib_subgroup_df, distrib_subgroup_df_diff)

  ##### Save distribution  #####
  # ********************************************** #
  #               Save distribution                #
  # ********************************************** #

  # append 'all' data frame with subgroup and their difference
  distrib_diet_r <- rbind(distrib_all, distrib_subgroup_df)

  # add replicate number for clarity
  distrib_diet_r$replicate <- replicate.num

  if(replicate.num == 0) {
    # base/original sample estimate
    distrib_diet_original <- distrib_diet_r

    save(distrib_diet_original, file = file.path(dir_nci_output, "distrib_diet_original.rdata"))

  } else {
    # bootstrap estimates
    distrib_diet_bootstrap[[replicate.num]] <- distrib_diet_r

    # save file once the final replicate has been estimated
    if (replicate.num==repliclast){
      first<- ifelse(replicfirst==0, 1, replicfirst)
      save(distrib_diet_bootstrap,
           file = file.path(dir_nci_bootstrap, paste0("_distrib_diet_r",first,"_to_",repliclast,".rdata"))
           )
      } # end of conditional on last bootstrap replicate
    } # end of conditional on bootstrap replicates

  rm(distrib_all, distrib_subgroup_df, distrib_diet_r, usintake_mc_t_out)

} # end of bootstrap replicate loop

tictoc::toc() # time out


#### Distribution overview ####
# *********************************************************************** #
#                                                                         #
#                         Distribution overview                           #
#                                                                         #
# *********************************************************************** #

#' note1: in this example, we expore distribution of original/base estimates only
#'
#' note2: see *NCI Method Daily Nutrient* using `browseVignettes("ncimultivar")` for variance estimation details

if(!(exists("distrib_diet_original"))) load(file = file.path(dir_nci_output, "distrib_diet_original.rdata"))

dim(distrib_diet_original); names(distrib_diet_original); table(distrib_diet_original$population)

##### Print table with percentile values  #####
# ********************************************** #
#       Print table with percentile values       #
# ********************************************** #

distrib_diet_original |>
  select(-replicate) |>
  filter(statistic %in% c("1%", "5%", "25%", "50%", "75%", "95%", "99%")) |>
  slice(grep("HEFI2019", variable)) |>
  group_by(population, variable) |>
  pivot_wider(
    names_from = "statistic",
    values_from = "value"
  ) |>
  knitr::kable(
    digits = 1,
    caption = "Percentile values of HEFI-2019 scores (total/80) among boys and girls aged 13-17 y (CCHS 2015 - Nutrition)"
  )

##### Make graph of distribution #####
# ********************************************** #
#           Make graph of distribution           #
# ********************************************** #

# Total HEFI-2019 score
fig_hefi_total <-
  distrib_diet_original |>
  slice(grep("HEFI2019", variable)) |>
  filter(statistic!="Mean" &
          population %in% c("All", "Boy", "Girl") &
           variable == "HEFI2019_TOTAL_SCORE") |>
  ggplot(aes(x=value, group=population), stat="identity") +
  geom_histogram(binwidth = 5, aes(y=after_stat(density)), colour="black",fill="white") +
  geom_density(kernel="gaussian",fill="black",alpha=0.1) +
  facet_wrap(~population, ncol=1) +
  labs(title="Distribution of total HEFI-2019 score",
       subtitle="Adolescents 13-17 y, CCHS 2015-Nutrition",
       y = "Density",
       x = "Total HEFI-2019 score (/80), points") +
  theme_bw()

print(fig_hefi_total)


# HEFI-2019 component scores
fig_hefi_comp <-
  # output desired statistics
  distrib_diet_original |>
  slice(grep("HEFI2019", variable)) |>
  filter(statistic!="Mean" &
           population %in% c("Boy", "Girl") &
           variable != "HEFI2019_TOTAL_SCORE") |>
  mutate(
    #ensure ordering kept
    variable = factor(variable, levels=variable, labels=variable)
  ) |>
  # generate figure using ggplot2
  ggplot(aes(x=value, group=population),stat="identity") +
  geom_density(aes(fill=population,colour=population), kernel="gaussian",alpha=0.3) +
  facet_wrap(~variable, scales="free",nrow=2) +
  # show full scale
  scale_x_continuous(breaks= scales::breaks_extended(n=6,only.loose=TRUE)) +
  theme_bw() +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_blank()) +
  labs(title="Distribution of HEFI-2019 component scores",
       subtitle="Adolescents 13-17 y, CCHS 2015-Nutrition",
       y = "Density",
       x = "Component score, points")

print(fig_hefi_comp)

#'
#### End of code 03B ####
# *********************************************************************** #
#                            End of code 03B                              #
# *********************************************************************** #

sessionInfo()
