Descriptive statistics based on repeated 24-h recall: HEFI-2019 scores
using the NCI multivariate method - results
================
Didier Brassard
2025-05-23

``` r
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
```

Note 1: data pertaining to the 2015 Canadian Community Health Survey -
Nutrition (Public Use Microdata Files) are available upon request to
Statistics Canada online:
<https://www150.statcan.gc.ca/n1/en/catalogue/82M0024X2018001>

``` r
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
```

goal: generate all statistics of interest using simulation data

note: if disk space is an issue, the present script can be added at the
end of `03A_CCHS2015_DescriptiveU.R`. Once **all** statistics of
interest are calculated and saved, the simulation data `mcmc_sim*.rdata`
can be deleted. However, if additional statistics are needed, a new run
of `ncimultivar::nci_multivar_distrib` will be necessary to re-create
all simulation data and output the additional statistics. Similarly, if
the model objects `mcmc_model*.rdata` have been deleted, a complete
re-run including `ncimultivar::nci_multivar_mcmc` will be required to
generate any additional statistics that were not originally saved. For
these reasons, all models and simulation data are saved in this example
at the expense of disk space. A small price to pay for salvation…

``` r
# (manual) indicate number of replicate to run

replicfirst <- 0
repliclast  <- 5
```

note: to run only base/original estimates set both ‘replicfirst’ and
‘repliclast’ to `0`

``` r
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
```

    ## [1] "Post-NCI processing for iteration 0"
    ##       dri_group
    ## sex_f       4      5      6      7
    ##   Boy  102500      0 390500      0
    ##   Girl      0  92500      0 427500

    ## Healthy Eating Food Index-2019 Scoring Algorithm R version 1.5

    ##  [1] "HEFI2019C1_VF"         "HEFI2019C2_WHOLEGR"    "HEFI2019C3_GRRATIO"    "HEFI2019C4_PROFOODS"  
    ##  [5] "HEFI2019C5_PLANTPRO"   "HEFI2019C6_BEVERAGES"  "HEFI2019C7_FATTYACID"  "HEFI2019C8_SFAT"      
    ##  [9] "HEFI2019C9_FREESUGARS" "HEFI2019C10_SODIUM"    "HEFI2019_TOTAL_SCORE" 
    ## [1] "Distribution estimate difference calculated for sex_f: Girl vs. Boy"
    ## [1] "Post-NCI processing for iteration 1"

    ## Healthy Eating Food Index-2019 Scoring Algorithm R version 1.5

    ## [1] "Post-NCI processing for iteration 2"

    ## Healthy Eating Food Index-2019 Scoring Algorithm R version 1.5

    ## [1] "Post-NCI processing for iteration 3"

    ## Healthy Eating Food Index-2019 Scoring Algorithm R version 1.5

    ## [1] "Post-NCI processing for iteration 4"

    ## Healthy Eating Food Index-2019 Scoring Algorithm R version 1.5

    ## [1] "Post-NCI processing for iteration 5"

    ## Healthy Eating Food Index-2019 Scoring Algorithm R version 1.5

``` r
tictoc::toc() # time out
```

    ## NCI MCMC post-processing replicate 0 to 5: 101.123 sec elapsed

``` r
#### Distribution overview ####
# *********************************************************************** #
#                                                                         #
#                         Distribution overview                           #
#                                                                         #
# *********************************************************************** #
```

note1: in this example, we expore distribution of original/base
estimates only

note2: see *NCI Method Daily Nutrient* using
`browseVignettes("ncimultivar")` for variance estimation details

``` r
if(!(exists("distrib_diet_original"))) load(file = file.path(dir_nci_output, "distrib_diet_original.rdata"))

dim(distrib_diet_original); names(distrib_diet_original); table(distrib_diet_original$population)
```

    ## [1] 10400     5

    ## [1] "population" "variable"   "statistic"  "value"      "replicate"

    ## 
    ##        All        Boy       Girl Girl - Boy 
    ##       2600       2600       2600       2600

``` r
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
```

    ## select: dropped one variable (replicate)
    ## filter: removed 9,672 rows (93%), 728 rows remaining
    ## slice: removed 420 rows (58%), 308 rows remaining
    ## group_by: 2 grouping variables (population, variable)
    ## pivot_wider: reorganized (statistic, value) into (1%, 5%, 25%, 50%, 75%, …) [was 308x4, now 44x9]

| population | variable              |   1% |   5% |  25% |  50% |  75% |  95% |  99% |
|:-----------|:----------------------|-----:|-----:|-----:|-----:|-----:|-----:|-----:|
| All        | HEFI2019C1_VF         |  3.4 |  4.4 |  6.1 |  7.4 |  8.8 | 11.0 | 12.6 |
| All        | HEFI2019C2_WHOLEGR    |  0.2 |  0.3 |  0.8 |  1.2 |  1.7 |  2.4 |  3.0 |
| All        | HEFI2019C3_GRRATIO    |  0.2 |  0.4 |  0.9 |  1.3 |  1.7 |  2.3 |  2.7 |
| All        | HEFI2019C4_PROFOODS   |  2.8 |  3.4 |  4.5 |  5.0 |  5.0 |  5.0 |  5.0 |
| All        | HEFI2019C5_PLANTPRO   |  0.1 |  0.2 |  0.6 |  1.1 |  1.6 |  2.6 |  3.4 |
| All        | HEFI2019C6_BEVERAGES  |  2.3 |  3.6 |  5.8 |  7.2 |  8.4 |  9.5 |  9.9 |
| All        | HEFI2019C7_FATTYACID  |  0.1 |  0.5 |  1.3 |  1.8 |  2.5 |  3.6 |  4.5 |
| All        | HEFI2019C8_SFAT       |  0.0 |  0.5 |  2.5 |  3.8 |  4.9 |  5.0 |  5.0 |
| All        | HEFI2019C9_FREESUGARS |  0.0 |  0.0 |  1.3 |  5.3 |  8.8 | 10.0 | 10.0 |
| All        | HEFI2019C10_SODIUM    |  0.0 |  1.7 |  3.9 |  5.3 |  6.5 |  8.0 |  9.0 |
| All        | HEFI2019_TOTAL_SCORE  | 20.4 | 24.9 | 32.5 | 38.9 | 44.8 | 51.3 | 54.8 |
| Boy        | HEFI2019C1_VF         |  3.2 |  4.2 |  5.8 |  7.0 |  8.2 | 10.2 | 11.6 |
| Boy        | HEFI2019C2_WHOLEGR    |  0.2 |  0.4 |  0.8 |  1.2 |  1.7 |  2.5 |  3.1 |
| Boy        | HEFI2019C3_GRRATIO    |  0.2 |  0.4 |  0.9 |  1.3 |  1.7 |  2.3 |  2.7 |
| Boy        | HEFI2019C4_PROFOODS   |  3.1 |  3.8 |  4.8 |  5.0 |  5.0 |  5.0 |  5.0 |
| Boy        | HEFI2019C5_PLANTPRO   |  0.1 |  0.2 |  0.6 |  1.0 |  1.6 |  2.6 |  3.4 |
| Boy        | HEFI2019C6_BEVERAGES  |  2.2 |  3.5 |  5.6 |  6.9 |  8.1 |  9.3 |  9.8 |
| Boy        | HEFI2019C7_FATTYACID  |  0.2 |  0.6 |  1.3 |  1.9 |  2.6 |  3.7 |  4.6 |
| Boy        | HEFI2019C8_SFAT       |  0.0 |  1.0 |  2.9 |  4.0 |  5.0 |  5.0 |  5.0 |
| Boy        | HEFI2019C9_FREESUGARS |  0.0 |  0.0 |  1.3 |  5.2 |  8.5 | 10.0 | 10.0 |
| Boy        | HEFI2019C10_SODIUM    |  0.0 |  1.7 |  3.8 |  5.2 |  6.4 |  7.9 |  8.8 |
| Boy        | HEFI2019_TOTAL_SCORE  | 20.5 | 24.9 | 32.3 | 38.4 | 44.3 | 50.7 | 54.1 |
| Girl       | HEFI2019C1_VF         |  3.6 |  4.7 |  6.5 |  7.8 |  9.3 | 11.5 | 13.2 |
| Girl       | HEFI2019C2_WHOLEGR    |  0.1 |  0.3 |  0.7 |  1.1 |  1.6 |  2.4 |  3.0 |
| Girl       | HEFI2019C3_GRRATIO    |  0.2 |  0.3 |  0.8 |  1.2 |  1.6 |  2.2 |  2.7 |
| Girl       | HEFI2019C4_PROFOODS   |  2.6 |  3.2 |  4.2 |  5.0 |  5.0 |  5.0 |  5.0 |
| Girl       | HEFI2019C5_PLANTPRO   |  0.1 |  0.2 |  0.6 |  1.1 |  1.7 |  2.7 |  3.5 |
| Girl       | HEFI2019C6_BEVERAGES  |  2.3 |  3.8 |  6.1 |  7.6 |  8.7 |  9.7 |  9.9 |
| Girl       | HEFI2019C7_FATTYACID  |  0.0 |  0.5 |  1.2 |  1.7 |  2.4 |  3.5 |  4.4 |
| Girl       | HEFI2019C8_SFAT       |  0.0 |  0.1 |  2.2 |  3.5 |  4.6 |  5.0 |  5.0 |
| Girl       | HEFI2019C9_FREESUGARS |  0.0 |  0.0 |  1.3 |  5.5 |  9.1 | 10.0 | 10.0 |
| Girl       | HEFI2019C10_SODIUM    |  0.0 |  1.8 |  4.0 |  5.4 |  6.6 |  8.2 |  9.1 |
| Girl       | HEFI2019_TOTAL_SCORE  | 20.3 | 24.9 | 32.8 | 39.3 | 45.4 | 51.9 | 55.3 |
| Girl - Boy | HEFI2019C1_VF         |  0.3 |  0.5 |  0.7 |  0.9 |  1.1 |  1.3 |  1.5 |
| Girl - Boy | HEFI2019C2_WHOLEGR    |  0.0 | -0.1 | -0.1 | -0.1 | -0.1 | -0.1 |  0.0 |
| Girl - Boy | HEFI2019C3_GRRATIO    | -0.1 | -0.1 | -0.1 | -0.1 | -0.1 | -0.1 |  0.0 |
| Girl - Boy | HEFI2019C4_PROFOODS   | -0.5 | -0.6 | -0.6 |  0.0 |  0.0 |  0.0 |  0.0 |
| Girl - Boy | HEFI2019C5_PLANTPRO   |  0.0 |  0.0 |  0.0 |  0.0 |  0.0 |  0.1 |  0.1 |
| Girl - Boy | HEFI2019C6_BEVERAGES  |  0.1 |  0.3 |  0.6 |  0.6 |  0.6 |  0.3 |  0.1 |
| Girl - Boy | HEFI2019C7_FATTYACID  | -0.1 | -0.1 | -0.2 | -0.2 | -0.2 | -0.2 | -0.2 |
| Girl - Boy | HEFI2019C8_SFAT       |  0.0 | -0.8 | -0.7 | -0.6 | -0.4 |  0.0 |  0.0 |
| Girl - Boy | HEFI2019C9_FREESUGARS |  0.0 |  0.0 |  0.0 |  0.3 |  0.6 |  0.0 |  0.0 |
| Girl - Boy | HEFI2019C10_SODIUM    |  0.0 |  0.1 |  0.2 |  0.2 |  0.3 |  0.3 |  0.3 |
| Girl - Boy | HEFI2019_TOTAL_SCORE  | -0.1 |  0.0 |  0.5 |  0.9 |  1.1 |  1.2 |  1.2 |

Percentile values of HEFI-2019 scores (total/80) among boys and girls
aged 13-17 y (CCHS 2015 - Nutrition)

``` r
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
```

    ## slice: removed 6,000 rows (58%), 4,400 rows remaining
    ## filter: removed 4,103 rows (93%), 297 rows remaining

``` r
print(fig_hefi_total)
```

![](/Example_R_cchs/Output/03B_CCHS2015_DescriptiveU_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
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
```

    ## slice: removed 6,000 rows (58%), 4,400 rows remaining
    ## filter: removed 2,420 rows (55%), 1,980 rows remaining
    ## mutate: converted 'variable' from character to factor (0 new NA)

``` r
print(fig_hefi_comp)
```

![](/Example_R_cchs/Output/03B_CCHS2015_DescriptiveU_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
#### End of code 03B ####
# *********************************************************************** #
#                            End of code 03B                              #
# *********************************************************************** #

sessionInfo()
```

    ## R version 4.4.3 (2025-02-28)
    ## Platform: x86_64-apple-darwin20
    ## Running under: macOS Sonoma 14.7.5
    ## 
    ## Matrix products: default
    ## BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.4-x86_64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## time zone: America/Toronto
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] scales_1.3.0       ggplot2_3.5.1      knitr_1.50         hefi2019_0.0.1.5   devtools_2.4.5     usethis_3.1.0     
    ##  [7] tictoc_1.2.1       ncimultivar_1.0.3  tidyfast_0.4.0     tidylog_1.1.0      dplyr_1.1.4        data.table_1.17.99
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] tidyselect_1.2.1  Exact_3.3         rootSolve_1.8.2.4 farver_2.1.2      fastmap_1.2.0     promises_1.3.2   
    ##  [7] digest_0.6.37     rpart_4.1.24      mime_0.13         lifecycle_1.0.4   cluster_2.1.8.1   ellipsis_0.3.2   
    ## [13] Didmisc_0.3.0     lmom_3.2          magrittr_2.0.3    compiler_4.4.3    rlang_1.1.6       Hmisc_5.2-3      
    ## [19] tools_4.4.3       yaml_2.3.10       utf8_1.2.5        labeling_0.4.3    htmlwidgets_1.6.4 pkgbuild_1.4.7   
    ## [25] here_1.0.1        rsconnect_1.3.4   pkgload_1.4.0     miniUI_0.1.1.1    expm_1.0-0        withr_3.0.2      
    ## [31] foreign_0.8-90    purrr_1.0.4       nnet_7.3-20       grid_4.4.3        urlchecker_1.0.1  profvis_0.4.0    
    ## [37] xtable_1.8-4      e1071_1.7-16      colorspace_2.1-1  MASS_7.3-65       cli_3.6.5         mvtnorm_1.3-3    
    ## [43] crayon_1.5.3      rmarkdown_2.29    generics_0.1.3    remotes_2.5.0     rstudioapi_0.17.1 httr_1.4.7       
    ## [49] tzdb_0.5.0        sessioninfo_1.2.3 readxl_1.4.5      gld_2.6.7         cachem_1.1.0      proxy_0.4-27     
    ## [55] stringr_1.5.1     cellranger_1.1.0  base64enc_0.1-3   vctrs_0.6.5       boot_1.3-31       Matrix_1.7-3     
    ## [61] cpp11_0.5.2       hms_1.1.3         Formula_1.2-5     htmlTable_2.4.3   clipr_0.8.0       tidyr_1.3.1      
    ## [67] glue_1.8.0        stringi_1.8.7     gtable_0.3.6      later_1.4.1       munsell_0.5.1     tibble_3.2.1     
    ## [73] pillar_1.10.2     clisymbols_1.2.0  htmltools_0.5.8.1 R6_2.6.1          rprojroot_2.0.4   evaluate_1.0.3   
    ## [79] shiny_1.10.0      lattice_0.22-7    haven_2.5.4       readr_2.1.5       backports_1.5.0   memoise_2.0.1    
    ## [85] httpuv_1.6.15     class_7.3-23      DescTools_0.99.60 Rcpp_1.0.14       gridExtra_2.3     checkmate_2.3.2  
    ## [91] xfun_0.52         fs_1.6.5          forcats_1.0.0     pkgconfig_2.0.3
