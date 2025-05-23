#' ---
#' title: "Descriptive statistics based on repeated 24-h recall: HEFI-2019 scores using the NCI multivariate method - model and simulations"
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
# NOTE: This code assumes that <01_CCHS2015_Data_preparation.R>           #
# was executed beforehand.                                                #
#                                                                         #
# *********************************************************************** #

#### General set-up: packages and location of files ####
# *********************************************************************** #
#                                                                         #
#             General set-up: packages and location of files              #
#                                                                         #
# *********************************************************************** #

#' Note 1: a hard coded path (<external_drive>) is used since files are on an external drive.
#' Usually we would want analysis to be self-contained, but survey files are a bit large.
#' Also, the data are set up exactly as they are once obtained from Statistics Canada
#'
#' Note 2: data pertaining to the 2015 Canadian Community Health Survey -
#' Nutrition (Public Use Microdata Files) are available upon request to
#' Statistics Canada online: https://www150.statcan.gc.ca/n1/en/catalogue/82M0024X2018001

# TO DO: Update CCHS 2015 - Nutrition file location
external_drive <- file.path("","Volumes","SANDISK DID")

## (auto) create shortcuts for raw CCHS 2015 - Nutrition file location
boot_dir <- file.path(external_drive,"CCHS_Nutrition_2015_PUMF","Bootstrap","Data_Donnee")

# (auto) create shortcuts for project directory tree

## Common directory
dir_example <- here::here("Example_R_cchs")
dir_processed <- file.path(dir_example,"data", "processed")
dir_temp <- file.path(dir_example,"data", "temp")

## For the NCI output
dir_nci <- file.path(dir_example, "data", "nci_mcmc")
dir_nci_original <- file.path(dir_nci, "original")
dir_nci_output <- file.path(dir_nci, "output")
dir_nci_bootstrap <- file.path(dir_nci,"bootstrap")
dir_nci_traceplot <- file.path(dir_nci,"traceplot")
  ### create folders, if need be
  if(dir.exists(dir_nci)==FALSE){
    dir.create(dir_nci, recursive = TRUE)
  }
  if(dir.exists(dir_nci_original)==FALSE){
    dir.create(dir_nci_original, recursive = TRUE)
  }
  if(dir.exists(dir_nci_output)==FALSE){
    dir.create(dir_nci_output, recursive = TRUE)
  }
  if(dir.exists(dir_nci_bootstrap)==FALSE){
    dir.create(dir_nci_bootstrap, recursive = TRUE)
  }
  if(dir.exists(dir_nci_traceplot)==FALSE){
    dir.create(dir_nci_traceplot, recursive = TRUE)
  }

# Packages

## data management
library(data.table)
library(dplyr)
library(tidylog)

## NCI multivariate method
library(ncimultivar)

## computing
library(tictoc)
library(parallel)
library(future)
library(doFuture)
library(furrr)
library(foreach)

# suppress scientific notation
options(scipen = 9999)

#### Prepare data suitable to apply the multivariate method ####
# *********************************************************************** #
#                                                                         #
#         Prepare data suitable to apply the multivariate method          #
#                                                                         #
# *********************************************************************** #

#' note1: for the NCI method, an input data frame in long (or tall) format
#' is required, where repeated 24-h dietary recall appear on separate rows.
#'
#' note2: for this example, we focus on children aged 13-17 inclusively.
#'
#' note3: we remove 24-h dietary recall with less than 250 kcal.
#' This cut-off is used since intakes <250 kcal cannot plausibly inform hypothetical
#' unobserved usual intakes (long-term average), but it is challenging to decide
#' which energy cut-off value is plausibly related to long-term intakes beyond that.

# ********************************************** #
#            Intake + sociodemo data             #
# ********************************************** #

# 1) load prepared CCHS data

## Summary nutrient intakes and sociodemographic characteristics
load(file = file.path(dir_processed, "CCHS2015_HS_NCI.rdata"))
dim(CCHS2015_HS_NCI); names(CCHS2015_HS_NCI)

## Dietary constituents required for the HEFI-2019
load(file = file.path(dir_processed, "CCHS2015_HEFI2019_PER24HR.rdata"))
dim(CCHS2015_HEFI2019_PER24HR); names(CCHS2015_HEFI2019_PER24HR)

# 2) Merge both data by respondent and recall identifiers
intake_and_sociodeom <-
  full_join(x  = CCHS2015_HS_NCI |>
              select("ADM_RNO", "WTS_P", "SUPPID",
                     "r24_weekend", "drig", "sex", "age"),
            y  = CCHS2015_HEFI2019_PER24HR,
            by = c("ADM_RNO", "SUPPID")) |>
  # focus on children with age [13, 17] with 250 kcal or more
  filter(age>12 & age<18 ) |>
  filter(energy>=250) |>
  # create dummy indicator for DRI age/sex group + 2nd recall
  mutate(
    r24_day2 = ifelse(SUPPID==2, 1, 0),
    drig_m_9_13  = ifelse(drig==4, 1, 0),
    drig_f_9_13  = ifelse(drig==5, 1, 0),
    drig_m_14_18 = ifelse(drig==6, 1, 0),
    drig_f_14_18 = ifelse(drig==7, 1, 0)
  ) |>
  # drop extra variables
  select(-c("WTS_P", "nonzero_energy"))

# note: A total of 10x 24-h dietary recalls had energy <250 kcal

## overview
dim(intake_and_sociodeom); names(intake_and_sociodeom)

intake_and_sociodeom |>
  select("SUPPID", "wg":"sodium") |>
  gtsummary::tbl_summary(
    by = "SUPPID",
    statistic = list(c("vf","wg", "rg", "pfab","pfpb","otherfoods",
                       "water", "otherbevs", "milk",
                       "energy", "sfa", "mufa", "pufa", "sodium") ~ "{mean} [{min} to {max}]"),
  ) |>
  gtsummary::modify_caption("Mean HEFI-2019 dietary constituents and range (CCHS 2015 - Nutrition), by 24-h dietary recall") |>
  gtsummary::as_kable()

#' note: unsweetened plant-based beverages with sufficient protein have not been reported (all values=0).

## confirm recoding
with(intake_and_sociodeom, table(drig, drig_m_9_13))
with(intake_and_sociodeom, table(drig, drig_f_9_13))
with(intake_and_sociodeom, table(drig, drig_m_14_18))
with(intake_and_sociodeom, table(drig, drig_f_14_18))

with(intake_and_sociodeom, table(SUPPID, r24_day2))

# ********************************************** #
#             Bootstrap weight data              #
# ********************************************** #

#' note: variance is estimated using (500) bootstrap
#' replicate weights in CCHS 2015 - Nutrition.

# 1) output list of (unique) respondent to output BSW data
respondent_list <-
  intake_and_sociodeom |>
  distinct(ADM_RNO)

dim(respondent_list); names(respondent_list)

# 2) Load and prepare bootstrap replicate weights

bsw <-
  data.table::fread(file.path(boot_dir,"b5.txt"),
                    stringsAsFactors = F, data.table= F, header=F,
                    col.names = c("ADM_RNO","WTS_P",paste0("BSW",seq(1,500))))

# save complete bsw data for further analysis
save(bsw, file = file.path(dir_temp, "bsw.rdata"))

# keep only (unique) respondents from 'preNCI'
bsw <-
  bsw |>
  right_join(respondent_list,
             by = "ADM_RNO") |>
  # rename sampling weights
  rename(BSW0=WTS_P)

#' note: all rows from 'preNCI_respondent_list' should be matched

#### Preliminary step: daily vs. episodic items, variable definition ####
# *********************************************************************** #
#                                                                         #
#    Preliminary step: daily vs. episodic items, variable definition      #
#                                                                         #
# *********************************************************************** #

# 1) Check proportion of 'zero' among dietary constituents based on first 24-h dietary recall 1
intake_day1 <-
  intake_and_sociodeom |>
  filter(SUPPID==1) |>
  select("wg":"sodium")

proportions <- numeric(length = ncol(intake_day1))

for (i in seq_along(intake_day1)) {
  # Calculate mean proportion of values equal to 0 in each column
  proportions[i] <- mean(intake_day1[[i]] == 0, na.rm = TRUE)
}
# add names for clarity
proportions <- setNames(proportions, names(intake_day1))

# Print Pr(X=0) for each dietary constituents, descending order:
sort(round(proportions*100,1), decreasing=TRUE)

#' note: pfpb, wg, milk and otherbevs would be episodically-consumed
#' dietary constituents based on a cut-off of 10% of zero.

# 2) Create vector of variables to be used within the NCI multivariate algorithm

## covariates
vars_z <- c("r24_weekend", "r24_day2", "drig_f_9_13", "drig_m_14_18", "drig_f_14_18")

#' note: the 'drig_m_9_13' dummy variable is omitted, so k-1 dummy are used in the model,
#' where k is the number of level based on the original DRI group variable for that sample (k=4).

## diet and nutrients
vars_diet_episodic <- c("pfpb", "wg", "milk", "otherbevs")
vars_diet_daily <- c("water", "pfab", "vf", "rg", "otherfoods", "freesugars", "energy", "sfa", "mufa", "pufa", "sodium")
vars_diet_all <- c(vars_diet_episodic, vars_diet_daily)

## misc and identifiers
participant_id <- "ADM_RNO"
recall_id <- "SUPPID"
wts_sample <- "BSW0"

# 3) Output 'preNCI' data for analysis
preNCI <-
  intake_and_sociodeom |>
  # merge with original sample weight (coded as BSW0) for preliminary NCI steps
  left_join(bsw |> select("ADM_RNO", "BSW0")) |>
  # tidy data to include only variables required for NCI
  select(all_of(participant_id), all_of(recall_id),
         all_of(vars_z), all_of(vars_diet_all), all_of(wts_sample))

# confirm lack of missing values:
colSums(is.na(preNCI[,c(vars_diet_all, vars_z, wts_sample)]))

if(sum(colSums(is.na(preNCI[,c(vars_diet_all, vars_z, wts_sample)])))==0) print("No missing values.") else message("Warning! Some values are missing!")

#' note: should show zero missing values for all variables

# Save preNCI data and bootstrap sample weights for reference and/or future analysis
save(preNCI,bsw,
     file = file.path(dir_nci_original, "preNCI_n_bsw.rdata"))

# Save predefined vectors for reference and/or analysis
save(vars_z, vars_diet_episodic, vars_diet_daily, vars_diet_all, participant_id, recall_id,
     file = file.path(dir_nci_original, "predefined_vectors.rdata"))

#### National Cancer Institute Multivariate Method ####
# *********************************************************************** #
#                                                                         #
#             National Cancer Institute Multivariate Method               #
#                                                                         #
# *********************************************************************** #

#'
##### 1) Winsorization #####
# ********************************************** #
#                 Winsorization                  #
# ********************************************** #

#' goal: identify extreme values and mitigate their impact using Winsorization
#'
#' note 1: in this example, only the UPPER values of intakes are winsorized
#'
#' note 2: to avoid repetitive code, every dietary constituent is looped through the winsorization algorithm

# For comparison purpose, find the maximum value for each variable BEFORE winsorization
max_values_before <- apply(preNCI[,c(vars_diet_all)], 2, max)

# Step 1: initiate list
winsorize_x <- vector(mode="list", length=length(vars_diet_all))

# Step 2: loop through all dietary constituents in 'vars_diet_all'
for (i in 1:length(vars_diet_all)){

  # output current variable name for easier reading
  current_x <- vars_diet_all[i]

  # 1) output a lower threshold to ignore winsorization of lower values
  p5_nonzero_x <-
    preNCI |>
    subset(subset = get(recall_id)==1 & get(current_x)>0,
           select = get(current_x)) |>
    stats::quantile(probs = 0.05, na.rm = TRUE)

  # note: 5th non zero percentile is arbitrary, does not matter as long as value is relatively low

  # 2) apply algorithm
  winsorize <-
    ncimultivar::boxcox_survey(
      input.data            = preNCI ,
      row.subset            = preNCI[[recall_id]]==1, # note: using first 24-h dietary recall to determine thresholds
      id                    = participant_id ,
      repeat.obs            = recall_id,
      variable              = current_x,
      weight                = wts_sample,
      do.winsorization      = TRUE,
      is.episodic           = current_x %in% vars_diet_episodic, # note: to assess non-zero values for episodic foods
      iqr.multiple          = 2 , # note: upper cut-off defined as: p75 + IQR * 2
      print.winsorization   = FALSE)

  # add generic names
  names(attributes(winsorize)$winsorization.report) <- c(participant_id, recall_id, "original.value", "winsorized.value")

  # 3) keep only the upper threshold, and ignore lower threshold
  winsorize_x[[i]] <-
    attributes(winsorize)$winsorization.report |>
    # remove rows that were 'low outlier' (i.e., winsorize only upper values)
    subset(original.value > p5_nonzero_x)

  rm(winsorize)

  # 4) Extract the upper threshold and winsorize accordingly
  if(length(winsorize_x[[i]]$winsorized.value)>0){
    message(glue::glue("Winsorization of x{i}, {current_x}: max. value {round(winsorize_x[[i]]$winsorized.value[1],2)}"))

    # if X>x, let X be x, otherwise, no change
    preNCI <-
      preNCI |>
      tidylog::mutate(
        !!sym(current_x) := pmin(get(current_x), winsorize_x[[i]]$winsorized.value[1])
      )

    # tidylog::mutate used here to have a summary of how many values were modified in the log
    # alternative base R code: preNCI[[current_x]] <- pmin(preNCI[[current_x]], winsorize_x[[i]]$winsorized.value[1])

  } else {
    message(glue::glue("Winsorization of x{i}, {current_x}: none."))

  }
  rm(current_x)
}

# Step 3) Assessment. Find the maximum value for each variable AFTER winsorization
max_values_after <- apply(preNCI[,c(vars_diet_all)], 2, max)

# Compare max before and after
print(round(max_values_before,1))
print(round(max_values_after,1))
print(round(max_values_before-max_values_after,1))

# Show report

## add variable names for clarity
names(winsorize_x) <- vars_diet_all

print(winsorize_x)

#'
##### 2) Box-Cox transformation #####
# ********************************************** #
#             Box-Cox transformation             #
# ********************************************** #

#' goal: run Box-Cox survey with covariates to get transformation parameter
#'
#' note: to avoid repetitive code, every dietary constituent is looped through Box-Cox survey

# Step 1: initiate list
tran_lambda <- vector(mode="list", length=length(vars_diet_all))

# Step 2: apply 'boxcox_survey'
for (i in 1:length(vars_diet_all)){

  # Box-Cox transformation
  boxcox <-
    ncimultivar::boxcox_survey(
      input.data            = preNCI,
      row.subset            = (preNCI[[recall_id]]==1), # note: use first 24-h recall for transformation
      variable              = vars_diet_all[i],
      covariates            = vars_z,
      weight                = wts_sample,
      is.episodic           = vars_diet_all[i] %in% vars_diet_episodic #assess non-zero values for episodic foods
    )

  message(glue::glue("Box-Cox lambda of x{i}, {vars_diet_all[i]}: {boxcox$tran_lambda}"))
  tran_lambda[[i]] <- boxcox$tran_lambda
  rm(boxcox)

}

# Create a dataset with transformation parameter to use in preprocessor macro

#' NCI note: the variable name must be in a column called `variable` and
#' the transformation parameter must be in a column called `tran_lambda`

boxcox_lambda_x <-
  data.frame(
    variable = vars_diet_all,
    tran_lambda = unlist(tran_lambda)
  )

# Show values
print(boxcox_lambda_x)

#'
##### 3) Minimum value #####
# ********************************************** #
#                 Minimum value                  #
# ********************************************** #

minimum_amount_x <-
  ncimultivar::calculate_minimum_amount(
    input.data         = preNCI,
    episodic.variables = vars_diet_episodic,
    daily.variables    = vars_diet_daily)

# Show minimum values
print(minimum_amount_x)

###### Save pre-processing data for reference ######
# ********************************************** #
#     Save pre-processing data for reference     #
# ********************************************** #

save(winsorize_x,boxcox_lambda_x, minimum_amount_x,
     file = file.path(dir_nci_original,"preprocess.rdata"))

#'
##### Start bootstrap loop #####
# ******************************************************** #
#                   Start bootstrap loop                   #
# ******************************************************** #

# (manual) indicate number of replicate to run

replicfirst <- 0
repliclast  <- 5

#' note 1: to run only base estimates (original data) set both 'replicfirst' and 'repliclast' to `0`
#'
#' note 2: suggest to start with original sample first to validate workflow, then,
#' a few replicates (say 2 to 5), and only then, all 500 replicates.
#'
#' note 3: in this example, all multivariate error models **and** full simulation are saved.
#' This provides maximal flexibility to generate statistics of interest based on simulated data.
#' However, considerable disk space is required for both the model and simulations. Saving all 500 bootstrap
#' replicate would require approximately 0.2 GB * 500 = 100 GB of disk space for this example.
#'
#' note 4: a parallel computing approach is demonstrated below to reduce analysis time. Alternatively,
#' a standard for-loop approach could be used using the code `for (replicate.num in replicfirst:repliclast){...}`

# (auto) ensure required data and vectors are available
if(!(exists("bsw")) | !(exists("preNCI"))) load(file.path(dir_nci_original,"preNCI_n_bsw.rdata"))
if(!(exists("vars_diet_daily")) | !(exists("vars_diet_episodic"))) load(file.path(dir_nci_original,"predefined_vectors.rdata"))
if(!(exists("boxcox_lambda_x")) | !(exists("minimum_amount_x"))) load(file.path(dir_nci_original,"preprocess.rdata"))

dim(preNCI); names(preNCI)
dim(bsw); names(bsw)[1:10]

# (auto) prepare parallel computing
future::plan(multisession, workers = parallel::detectCores()-2)
doFuture::registerDoFuture()
future::nbrOfWorkers()

# (auto) log time required to complete
tictoc::tic(glue::glue("NCI MCMC (model+sim.; {length(vars_diet_all)} components incl. {length(vars_diet_episodic)} episodic), replicate {replicfirst} to {repliclast}"))

# (auto) lunch parallel bootstrap loop, ensure all information available
foreach::foreach(replicate.num = replicfirst:repliclast,
                 .export = c("dir_nci_original", "dir_nci_bootstrap",
                             "participant_id", "recall_id",
                             "vars_diet_all", "vars_diet_daily", "vars_diet_episodic", "vars_z",
                             "preNCI", "bsw", "boxcox_lambda_x", "minimum_amount_x"),
                 .packages = c("dplyr", "tidylog", "ncimultivar")
) %dopar% {

  print(paste0("Starting Iteration ", replicate.num))

  #'
  ##### 4) Merge preNCI with BSW #####
  # ********************************************** #
  #            4) Merge preNCI with BSW            #
  # ********************************************** #

  preNCI_with_bsw <-
    preNCI |>
    inner_join(
      subset(bsw, select = c(get(participant_id), get(paste0("BSW", replicate.num))))
    )

  # note: all should be 'matched rows', or only rows from 'preNCI' if the full bsw data is used.

  #'
  ##### 5) Pre-processing: 'nci_multivar_preprocessor' #####
  # ********************************************** #
  #  Pre-processing: 'nci_multivar_preprocessor'   #
  # ********************************************** #

  pre_mcmc_data <-
    ncimultivar::nci_multivar_preprocessor(
      input.data         = preNCI_with_bsw,
      daily.variables    = vars_diet_daily,
      episodic.variables = vars_diet_episodic,
      boxcox.lambda.data  = boxcox_lambda_x,
      minimum.amount.data = minimum_amount_x)

  # overview
  if(replicate.num==0) print(pre_mcmc_data$backtransformation)


  ##### 6) MCMC error model: 'nci_multivar_mcmc' #####
  # ********************************************** #
  #     MCMC error model: 'nci_multivar_mcmc'      #
  # ********************************************** #

  mcmc_model <-
    ncimultivar::nci_multivar_mcmc(
      pre.mcmc.data          = pre_mcmc_data,
      id                     = participant_id ,
      repeat.obs             = recall_id,
      weight                 = paste0("BSW", replicate.num),
      daily.variables        = vars_diet_daily,
      episodic.variables     = vars_diet_episodic,
      default.covariate      = vars_z,
      num.mcmc.iterations    = 7000,
      num.burn               = 4000,
      num.thin               = 6, # note: (7000-3000)/6 = 500 iterations used to calculate posterior means
      mcmc.seed              = 7625+replicate.num)

  # Starting seed from https://www.random.org
  # 7625
  # Min: 1, Max: 10000
  # 2025-05-22 19:40:55 UTC

  # Save output data given base/original sample or bootstrap sample
  if(replicate.num == 0) {

    save(mcmc_model, file = file.path(dir_nci_original, paste0("mcmc_model",replicate.num,".rdata")))

    mcmc_base <- mcmc_model

  } else {

    save(mcmc_model, file = file.path(dir_nci_bootstrap, paste0("mcmc_model",replicate.num,".rdata")))
    mcmc_boot <- mcmc_model

    # clean temporary
    rm(mcmc_model)

  } # end of conditional on replicate number

  #' note: it would be more efficient to store all bootstrap models in a common list,
  #' but it is more flexible to save separate object in case of disk space issues, failed run, etc.

  #'
  ##### 7) Create population base for distrib #####
  # ********************************************** #
  #       Create population base for distrib       #
  # ********************************************** #

  # objective: create "weighted" data according to proportion of weekend/weekday, remove sequence effect
  dim(pre_mcmc_data$mcmc.input); names(pre_mcmc_data$mcmc.input)

  #' note: variable names are hardcoded for sequence=`r24_day2` and weekend=`r24_weekend`. Update in different setting.

  # output unique row for each respondent
  input_1row <-
    pre_mcmc_data$mcmc.input |>
    distinct(.data[[participant_id]],.keep_all=TRUE)

  num_subjects <- nrow(input_1row)

  # cancel effect of recall day 2
  input_1row$r24_day2 <- 0

  # create repeats of each participant for weekday and weekend consumption

  ## replicate each row of the data twice (for weekday/weekend)
  distrib_pop <- input_1row[rep(seq_len(num_subjects), each=2),]

  ## assign value of 0/1 for the weekend indicator
  distrib_pop$r24_weekend <- rep(c(0,1), num_subjects)

  ## assign weight to each row ('4' for weekend=0, '3' for weekend=1; i.e., Mon-Thur, Fri-Sun respectively)
  distrib_pop$weekend_weight <- rep(c(4,3), num_subjects)

  #'
  ##### 8) MCMC simulation: 'nci_multivar_distrib' #####
  # ********************************************** #
  #    MCMC simulation: 'nci_multivar_distrib'     #
  # ********************************************** #

  # Indicate input data according to base/original sample or bootstrap sample
  mcmc_data <- if (replicate.num == 0) mcmc_base else mcmc_boot

  # Simulate 'usual intake' data frame with 500 simulations for each respondent

  mcmc_sim <-
    ncimultivar::nci_multivar_distrib(
      multivar.mcmc.model  = mcmc_data,
      distrib.population   = distrib_pop,
      id                   = participant_id ,
      weight               = paste0("BSW", replicate.num),
      nuisance.weight      = "weekend_weight", # note: name defined in step 7 above
      additional.output    = vars_z[3:length(vars_z)], # note: hardcoded exclusion of first 2 variables which were nuisance (weekend, seq)
      num.simulated.u      = 500,
      distrib.seed         = 9666+replicate.num)

  # Starting seed from https://www.random.org
  # 9666
  # Min: 1, Max: 10000
  # 2025-05-22 19:41:29 UTC

  # Save simulation data given base/original sample or bootstrap sample
  if(replicate.num == 0) {

    save(mcmc_sim, file = file.path(dir_nci_original, paste0("mcmc_sim",replicate.num,".rdata")))

  } else {

    save(mcmc_sim, file = file.path(dir_nci_bootstrap, paste0("mcmc_sim",replicate.num,".rdata")))

  } # end of conditional on replicate number

  #' note: it would be more efficient to store all bootstrap simulations in a common list,
  #' but it is more flexible to save separate object in case of disk space issues, failed run, etc.

} # end of parallel bootstrap loop

tictoc::toc() # time out

#'
##### End bootstrap loop #####
# ******************************************************** #
#                                                          #
#                                                          #
#                    End bootstrap loop                    #
#                                                          #
#                                                          #
# ******************************************************** #

#' Multivar MCMC model assessment
#### Multivar MCMC model assessment ####
# *********************************************************************** #
#                                                                         #
#                     Multivar MCMC model assessment                      #
#                                                                         #
# *********************************************************************** #

#' note: see *Trace Plots for NCI Multivar MCMC* using `browseVignettes("ncimultivar")` for details.

# Helper function to generate and save PDF file of trace plot
make_trace_plot <- function(dir, multivar.mcmc.model, replicate.num){
  pdf(file = file.path(dir, paste0("traceplot_",replicate.num,".pdf")))
  ncimultivar::trace_plots( multivar.mcmc.model  = mcmc_model)
  dev.off()
  if(file.exists(file.path(dir, paste0("traceplot_",replicate.num,".pdf")))) {
    message("Trace plots created")
  } else {
    message("Error: trace plots were NOT created")
  }
}

# Load original sample model
load(file = file.path(dir_nci_original, "mcmc_model0.rdata"))

# Make trace plots for original/base model
make_trace_plot(
  dir                 = dir_nci_traceplot,
  multivar.mcmc.model = mcmc_model,
  replicate.num       = 0
)

#'
#### End of code 03A ####
# *********************************************************************** #
#                            End of code 03A                              #
# *********************************************************************** #

sessionInfo()
