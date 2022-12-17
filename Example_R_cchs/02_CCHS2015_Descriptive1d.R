#' ---
#' title: "Descriptive statistics based on a single 24-h recall: mean HEFI-2019 scores"
#' author: Didier Brassard
#' date: "`r Sys.Date()`"
#' output: github_document
#' ---

# *********************************************************************** #
#                                                                         #
#                      CCHS 2015 - Nutrition (PUMF)                       #
#                                                                         #
#          Descriptive statistics based on a single 24-h recall           #
#                                                                         #
#                        Author: Didier Brassard                          #
#                                                                         #
#                               Version 1                                 #
#                               2022-12-15                                #
#                                                                         #
# NOTE: This code assumes that <01_CCHS2015_Data_preparation.r            #
# was executed beforehand.                                                #
#                                                                         #
# *********************************************************************** #

# *********************************************************************** #
#                                                                         #
#                   General set-up: location of files                     #
#                                                                         #
# *********************************************************************** #

# Note 1: a hard coded path (<external_drive>) is used since files are on an external drive.
# Usually we would want analysis to be self-contained, but survey files are a bit large.
# Also, the data are set up exactly as they are once obtained from Statistics Canada

# Note 2: data pertaining to the 2015 Canadian Community Health Survey -
# Nutrition (Public Use Microdata Files) are available upon request to
# Statistics Canada online: https://www150.statcan.gc.ca/n1/en/catalogue/82M0024X2018001

# TO DO: Update CCHS 2015 - Nutrition file location
external_drive <- file.path("","Volumes","SANDISK DID")

  ## Automatic: create shortcuts for raw CCHS 2015 - Nutrition file location
  cchs_dir <- file.path(external_drive,"CCHS_Nutrition_2015_PUMF","Data_Donnee")
  boot_dir <- file.path(external_drive,"CCHS_Nutrition_2015_PUMF","Bootstrap","Data_Donnee")

# TO DO: define current directory (cd)

  ## For R script execution (full repository):
  cd <- file.path(".","Example_R_cchs")
  sas_dir <- file.path(".","Example_SAS_cchs")

  ## For Markdown execution (self-contained):
  cd <- '.'
  sas_dir <- cd

# Automatic: create shortcuts for project directory tree

  ## Common directory
  data_dir <- file.path(cd, "Fmtdata")
  temp_dir <- file.path(cd, "Temp")

# Packages
  library(data.table)
  library(haven)
  library(tidyverse)
  library(tidylog)
  library(gtsummary)
  library(gt)
  library(parallel)
  library(furrr)
  library(purrr)
  library(tictoc)

# HEFI-2019 scoring algorithm
  #if not installed: devtools::install_github("didierbrassard/hefi2019")
  library(hefi2019)

# suppress scientific notation
  options(scipen = 9999)


# *********************************************************************** #
#                                                                         #
#         Estimate mean scores using the population ratio method          #
#                      (First 24-h dietary recall)                        #
#                                                                         #
# *********************************************************************** #

  # note: the population ratio method is preferable to derive mean HEFI-2019 scores
  # when using data from a single 24-h dietary recall.
  # reference: Freedman et al. J Nutr (2008) https://pubmed.ncbi.nlm.nih.gov/18716176/

# Load processed data
  load(file.path(data_dir,"intake_per24hr.rdata"))
  load(file.path(data_dir,"hs_nci.rdata"))

# 1) Combine dietary intakes of the first recall with sociodemo data

  intake_and_sociodeom <-
    inner_join(intake_per24hr,hs_nci|>select(ADM_RNO,SUPPID,WTS_P,drig,sex,age,smoking)) |>
    # remove 24-h recall with 0 energy intake & first 24-hr only
    filter(energy>0 & SUPPID==1)

  #note: sample size of respondents 2y+ for first 24-h recall = 20,103

# 2) Calculate HEFI-2019 scores based on mean intakes

  # note: sampling weights are applied, but variance ignored for this example

  # 2.1) define vectors of HEFI-2019 dietary constituents
  hefi2019_vars <- names(intake_per24hr[,3:(ncol(intake_per24hr)-1)])
  hefi2019_vars

  # 2.2) Calculate mean intakes of HEFI-2019 dietary constituents, weighted using sampling weights
  popratio_scored <-
    intake_and_sociodeom |>
    summarise(
      across(all_of(hefi2019_vars), function(x) weighted.mean(x,WTS_P), .names ="{col}_MEAN")
      # note: suffix <_MEAN> added for labeling population-level values (vs. respondent-level)
    ) |>
  # 2.3) Apply the HEFI-2019 scoring algorithm
    hefi2019(#indata             = .,
             vegfruits          = vf_MEAN,
             wholegrfoods       = wg_MEAN,
             nonwholegrfoods    = rg_MEAN,
             profoodsanimal     = pfab_MEAN,
             profoodsplant      = pfpb_MEAN,
             otherfoods         = otherfoods_MEAN,
             waterhealthybev    = water_MEAN,
             unsweetmilk        = milk_MEAN,
             unsweetplantbevpro = plantbev_MEAN,
             otherbeverages     = otherbevs_MEAN ,
             mufat              = mufa_MEAN ,
             pufat              = pufa_MEAN ,
             satfat             = sfa_MEAN ,
             freesugars         = freesugars_MEAN,
             sodium             = sodium_MEAN,
             energy             = energy_MEAN
    ) |>
    mutate(drig=0)

  # 2.4) Repeat for DRI groups

  popratio_scored_drig <-
    intake_and_sociodeom |>
    group_by(drig) |>
    summarise(
      across(all_of(hefi2019_vars), function(x) weighted.mean(x,WTS_P), .names ="{col}_MEAN")
      # note: suffix <_MEAN> added for labeling population-level values (vs. respondent-level)
    ) |>
    # Apply the HEFI-2019 socring algorithm
    hefi2019(#indata             = .,
      vegfruits          = vf_MEAN,
      wholegrfoods       = wg_MEAN,
      nonwholegrfoods    = rg_MEAN,
      profoodsanimal     = pfab_MEAN,
      profoodsplant      = pfpb_MEAN,
      otherfoods         = otherfoods_MEAN,
      waterhealthybev    = water_MEAN,
      unsweetmilk        = milk_MEAN,
      unsweetplantbevpro = plantbev_MEAN,
      otherbeverages     = otherbevs_MEAN ,
      mufat              = mufa_MEAN ,
      pufat              = pufa_MEAN ,
      satfat             = sfa_MEAN ,
      freesugars         = freesugars_MEAN,
      sodium             = sodium_MEAN,
      energy             = energy_MEAN
    )

  #2.5) Combine the overall population estimates with subgroup estimates

  popratio_scored_all <-
      rbind(popratio_scored,popratio_scored_drig) |>
      select(drig,starts_with("HEFI"))

    # add labels to DRI groups
    popratio_scored_all$drig_f <-
      factor(popratio_scored_all$drig,
             levels = c(0,seq(2,15)),
             labels = c(
               'All, 2y or older',
               '2 to 3 y',
               '4 to 8 y',
               'Male, 9 to 13 y',
               'Female, 9 to 13 y',
               'Male, 14 to 18 y',
               'Female, 14 to 18 y',
               'Male, 19 to 30 y',
               'Female, 19 to 30 y',
               'Male, 31 to 50 y',
               'Female, 31 to 50 y',
               'Male, 51 to 70 y',
               'Female, 51 to 70 y',
               'Male, 71 y or older',
               'Female, 71 y or older'
             )
      )

# 3) Show results of mean score

  popratio_scored_all |>
    select(drig_f,starts_with("HEFI")) |>
    #pivot_longer(cols=starts_with("HEFI")) |>
    knitr::kable(digits=1,
                 caption = "Mean HEFI-2019 score among Canadians 2y+ (CCHS 2015 - Nutrition)")

# *********************************************************************** #
#                                                                         #
#                  Estimate mean scores and differences                   #
#                                                                         #
# *********************************************************************** #

  # note: we can calculate mean HEFI-2019 score difference using the same method
  # For this example, mean score difference among smokers vs. non-smokers

  # Load processed data
  load(file.path(data_dir,"intake_per24hr.rdata"))
  load(file.path(data_dir,"hs_nci.rdata"))

# 1) Combine dietary intakes of the first recall with sociodemo data

  intake_and_sociodeom <-
    inner_join(intake_per24hr,hs_nci|>select(ADM_RNO,SUPPID,WTS_P,drig,sex,age,smoking)) |>
    # remove 24-h recall with 0 energy intake & first 24-hr only & aged 19 y +
    filter(energy>0 & SUPPID==1 & age>=19) |>
    mutate(
      # recode smoking as yes or no
      smoking = ifelse(smoking %in% c(1,2),1,smoking)
    )

# 2) Calculate mean intakes of HEFI-2019 dietary constituents, weighted using sampling weights

  # note: sampling weights are applied, but variance ignored for this example

  # 2.1) define vectors of HEFI-2019 dietary constituents
  hefi2019_vars <- names(intake_per24hr[,3:(ncol(intake_per24hr)-1)])
  hefi2019_vars

  # 2.2) Calculate mean intakes of HEFI-2019 dietary constituents, weighted using sampling weights, by smoking
  popratio_scored_smk <-
    intake_and_sociodeom |>
    # remove 29 respondents with missing smoking status
    filter(is.na(smoking)==FALSE) |>
    group_by(smoking) |>
    summarise(
      across(all_of(hefi2019_vars), function(x) weighted.mean(x,WTS_P), .names ="{col}_MEAN")
      # note: suffix <_MEAN> added for labeling population-level values (vs. respondent-level)
    ) |>
    # Apply the HEFI-2019 scoring algorithm
    hefi2019(#indata             = .,
      vegfruits          = vf_MEAN,
      wholegrfoods       = wg_MEAN,
      nonwholegrfoods    = rg_MEAN,
      profoodsanimal     = pfab_MEAN,
      profoodsplant      = pfpb_MEAN,
      otherfoods         = otherfoods_MEAN,
      waterhealthybev    = water_MEAN,
      unsweetmilk        = milk_MEAN,
      unsweetplantbevpro = plantbev_MEAN,
      otherbeverages     = otherbevs_MEAN ,
      mufat              = mufa_MEAN ,
      pufat              = pufa_MEAN ,
      satfat             = sfa_MEAN ,
      freesugars         = freesugars_MEAN,
      sodium             = sodium_MEAN,
      energy             = energy_MEAN
    )

# 3) Calculate score differences according to smoking status

  popratio_scored_smk_diff <-
    popratio_scored_smk |>
    select(smoking,starts_with("HEFI")) |>
    pivot_longer(
      cols = starts_with("HEFI"),
      names_to = "HEFI2019_components"
    ) |>
    pivot_wider(
      names_from = "smoking",
      names_prefix = "SMK_"
    ) |>
    mutate(
      DIFF = SMK_1 - SMK_0
    )

# 4) show results
  popratio_scored_smk_diff |>
    knitr::kable(
      digits=1,
      col.names = c("Components", "Non-smoker","Smoker","Difference"),
      caption = "Mean HEFI-2019 scores estimated using the population ratio method, by smoking status (CCHS 2015 - Nutrition)"
    )

# *********************************************************************** #
#                                                                         #
#          Variance estimation for the population ratio method            #
#                                                                         #
# *********************************************************************** #

  # note: there are different approaches to estimate variance of population ratio
  # mean with survey data. Perhaps the simplest approach (albeit not the most efficient)
  # is to iteratively loop the analysis through all bootstrap replicate weights
  # (i.e., obtain new mean estimates for each bootstrap weight) and estimate
  # confidence intervals based on the sampling distribution.

# 1) Load and prepare bootstrap replicate weights

  bsw <-
    data.table::fread(file.path(external_drive,"CCHS_Nutrition_2015_PUMF","Bootstrap",
                                "Data_Donnee","b5.txt"),
                      stringsAsFactors = F, data.table= F, header=F,
                      col.names = c("ADM_RNO","WTS_P",paste0("BSW",seq(1,500)))) |>
    # keep only respondents in <intake_and_sociodeom>
    right_join(intake_and_sociodeom[,"ADM_RNO"]) |>
    # rename sampling weights
    rename(BSW0=WTS_P)

# 2) Generate function to calculate mean intakes of hefi-2019 dietary constituents and difference

  mean_n_diff <-
    function(bsw_number,indata,inbsw,bsw_suffix="BSW"){
      # indata = input data set
      # inbsw  = data set with bootstrap replicate weight
      # bsw_number = number from 0 to 500 where 0 = original estimate and 1, 2, 3 ... 500 = bsw

      # note: 'ADM_RNO', 'smoking' and  'hefi2019_vars' are hardcoded in this function

      # 1) Create weights variable
      weights <- paste0(bsw_suffix,bsw_number)

      # 2) calculate mean
      suppressMessages(
      estimate <-
        # combine weight value with indata
        dplyr::left_join(indata,inbsw |> select(ADM_RNO,!!weights)) |>
        # rename weights for use with weighted.mean
        dplyr::rename(CURRENT_BSW= !!weights ) |>
        # remove missing values
        dplyr::filter(is.na(smoking)==FALSE) |>
        # group by smoking status
        dplyr::group_by(smoking) |>
        # calculate weighted mean
        dplyr::summarise(
          across(.cols=all_of(hefi2019_vars),
                 function(x) weighted.mean(x,w=CURRENT_BSW),
                 .names ="{col}_MEAN" )
          # note: suffix <_MEAN> added for labeling population-level values (vs. respondent-level)
          ) |>
        # Apply the HEFI-2019 scoring algorithm
        hefi2019::hefi2019(#indata             = .,
          vegfruits          = vf_MEAN,
          wholegrfoods       = wg_MEAN,
          nonwholegrfoods    = rg_MEAN,
          profoodsanimal     = pfab_MEAN,
          profoodsplant      = pfpb_MEAN,
          otherfoods         = otherfoods_MEAN,
          waterhealthybev    = water_MEAN,
          unsweetmilk        = milk_MEAN,
          unsweetplantbevpro = plantbev_MEAN,
          otherbeverages     = otherbevs_MEAN ,
          mufat              = mufa_MEAN ,
          pufat              = pufa_MEAN ,
          satfat             = sfa_MEAN ,
          freesugars         = freesugars_MEAN,
          sodium             = sodium_MEAN,
          energy             = energy_MEAN)
      ) # end of suppress message

      # 3) Calculate difference
      estimate_diff <-
        estimate |>
        dplyr::select(smoking,starts_with("HEFI")) |>
        tidyr::pivot_longer(
          cols = starts_with("HEFI"),
          names_to = "HEFI2019_components"
        ) |>
        tidyr::pivot_wider(
          names_from = "smoking",
          names_prefix = "SMK_"
        ) |>
        dplyr::mutate(
          DIFF = SMK_1 - SMK_0,
          # add BSW id
          replicate = bsw_number
        )
      rm(estimate)
      rm(weights)
      return(estimate_diff)
    }

  # 2.1) Run for estimate and difference in original sample

  mean_n_diff(bsw_number = 0,
              indata = intake_and_sociodeom,
              inbsw  = bsw)

# 3) Run function over all BSW (<furrr::future_map_dfr> may be faster on some machines)

  # Sequential execution
  # hefi2019_smk <-
  #   purrr::map_dfr(seq(0,500), mean_n_diff,
  #                  indata = intake_and_sociodeom,
  #                  inbsw  = bsw)

  # 3.1) Parallel execution

  future::plan(multicore,workers=parallel::detectCores()-1)
  message("Number of parallel workers: ", nbrOfWorkers())

  tictoc::tic()
  hefi2019_smk <-
    furrr::future_map_dfr(seq(0,500), mean_n_diff,
                   indata = intake_and_sociodeom,
                   inbsw  = bsw)
  tictoc::toc()

  # 3.2) save for later use

  # note: useful to save output of bootstrap analysis as it can take some time to compete
  save(hefi2019_smk,
       file = file.path(temp_dir,"hefi2019_smk_bootstrap.rdata"))

# 4) Calculate bootstrap variance

  # note: different options possible, including normal approximation or percentile.
  # For simplicity, the normal approximation is shown below.

# 4.1) Separate 'original' sample estimates from 'bootstrap' estimates

  hefi2019_smk0 <- hefi2019_smk |> filter(replicate==0)
  hefi2019_smkbs <- hefi2019_smk |> filter(replicate>0)

# 4.2) Check bootstrap estimates normality

  hefi2019_smkbs |>
    # focus on difference for this example
    ggplot(aes(x=DIFF)) +
    geom_density(fill="gray",alpha=0.3) +
    labs(title="Bootstrap estimates distribution of HEFI-2019 score difference (CCHS 2015 - Nutrition)",
         subtitle = "Should be approximately normal to use normal approximation",
         y="Density",x="Difference, points") +
    facet_wrap(~HEFI2019_components,scales="free") +
    theme_bw() +
    theme( panel.grid.major.y = element_blank(),
           legend.title = element_blank(),
           legend.position = "top")

# 4.3) Calculate standard deviation of the sampling distribution (i.e., bootstrap Standard Error)

  hefi2019_smkbs_se <-
    hefi2019_smkbs |>
    group_by(HEFI2019_components) |>
    summarise(
      across(c("SMK_0","SMK_1","DIFF"), function(x) sd(x))
    )

# 4.4) Merge original sample estimates with bootstrap Standard Error, calculate 95CI

  # 4.4.1) Transpose base estimates (wide->long)
  hefi2019_smk0_long <-
    hefi2019_smk0 |>
    select(-replicate) |>
    pivot_longer(cols=c("SMK_0","SMK_1","DIFF"),
                 values_to="estimate")

  # 4.4.2) Transpose bootstraps Standard Error (wide->long)
  hefi2019_smkbs_se_long <-
    hefi2019_smkbs_se |>
    pivot_longer(cols=c("SMK_0","SMK_1","DIFF"),
                 values_to="se")

  # 4.4.3) Merge both data and calculate 95%CI using normal approximation
  hefi2019_smkf <-
    full_join(hefi2019_smk0_long,hefi2019_smkbs_se_long) |>
    mutate(
      # Calculate 95%CI
      nboot  = 500,
      alpha  = 0.05,
      tvalue = qt(1-(alpha/2),nboot-2),
      lcl    = estimate - (se * tvalue),
      ucl    = estimate + (se * tvalue)
    )

  rm(list=c("hefi2019_smk0","hefi2019_smkbs",
            "hefi2019_smkbs_se","hefi2019_smk0_long",
            "hefi2019_smkbs_se_long"))

# 5) Show results in a table
  hefi2019_smkf |>
    select(HEFI2019_components,name,estimate,lcl,ucl) |>
    knitr::kable(
      digits  = 1,
      caption = "Mean HEFI-2019 scores in Smokers (SMK_1) vs. Non-Smokers (SMK_0) and difference aged 19y+ (CCHS 2015 - Nutrition)"
    )

# Session info
sessionInfo()

# end of code 02
