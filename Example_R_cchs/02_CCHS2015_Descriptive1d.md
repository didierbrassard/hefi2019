Descriptive statistics based on a single 24-h recall: mean HEFI-2019
scores
================
Didier Brassard
2022-12-15

``` r
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
library(parallel)
library(haven)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.4.0      ✔ purrr   0.3.5 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.3      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::between()   masks data.table::between()
    ## ✖ dplyr::filter()    masks stats::filter()
    ## ✖ dplyr::first()     masks data.table::first()
    ## ✖ dplyr::lag()       masks stats::lag()
    ## ✖ dplyr::last()      masks data.table::last()
    ## ✖ purrr::transpose() masks data.table::transpose()

``` r
library(tidylog)
```

    ## 
    ## Attaching package: 'tidylog'
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     add_count, add_tally, anti_join, count, distinct, distinct_all,
    ##     distinct_at, distinct_if, filter, filter_all, filter_at, filter_if,
    ##     full_join, group_by, group_by_all, group_by_at, group_by_if,
    ##     inner_join, left_join, mutate, mutate_all, mutate_at, mutate_if,
    ##     relocate, rename, rename_all, rename_at, rename_if, rename_with,
    ##     right_join, sample_frac, sample_n, select, select_all, select_at,
    ##     select_if, semi_join, slice, slice_head, slice_max, slice_min,
    ##     slice_sample, slice_tail, summarise, summarise_all, summarise_at,
    ##     summarise_if, summarize, summarize_all, summarize_at, summarize_if,
    ##     tally, top_frac, top_n, transmute, transmute_all, transmute_at,
    ##     transmute_if, ungroup
    ## 
    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     drop_na, fill, gather, pivot_longer, pivot_wider, replace_na,
    ##     spread, uncount
    ## 
    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

``` r
library(gtsummary)
```

    ## 
    ## Attaching package: 'gtsummary'
    ## 
    ## The following objects are masked from 'package:tidylog':
    ## 
    ##     mutate, select

``` r
library(gt)
library(labelled)

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
```

    ## Joining, by = c("ADM_RNO", "SUPPID")
    ## inner_join: added 5 columns (WTS_P, drig, sex, age, smoking)
    ## > rows only in x ( 0)
    ## > rows only in y ( 0)
    ## > matched rows 27,544
    ## > ========
    ## > rows total 27,544
    ## filter: removed 7,441 rows (27%), 20,103 rows remaining

``` r
  #note: sample size of respondents 2y+ for first 24-h recall = 20,103

# 2) Calculate HEFI-2019 scores based on mean intakes

  # note: sampling weights are applied, but variance ignored for this example

  # 2.1) define vectors of HEFI-2019 dietary constituents
  hefi2019_vars <- names(intake_per24hr[,3:(ncol(intake_per24hr)-1)])
  hefi2019_vars
```

    ##  [1] "wg"         "rg"         "pfab"       "pfpb"       "otherfoods"
    ##  [6] "vf"         "water"      "otherbevs"  "milk"       "plantbev"  
    ## [11] "freesugars" "energy"     "sfa"        "mufa"       "pufa"      
    ## [16] "sodium"

``` r
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
```

    ## Healthy Eating Food Index-2019 Scoring Algorithm R version 1.2
    ## summarise: now one row and 16 columns, ungrouped

``` r
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
```

    ## Healthy Eating Food Index-2019 Scoring Algorithm R version 1.2
    ## group_by: one grouping variable (drig)
    ## summarise: now 14 rows and 17 columns, ungrouped

``` r
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
```

| drig_f                | HEFI2019C1_VF | HEFI2019C2_WHOLEGR | HEFI2019C3_GRRATIO | HEFI2019C4_PROFOODS | HEFI2019C5_PLANTPRO | HEFI2019C6_BEVERAGES | HEFI2019C7_FATTYACID | HEFI2019C8_SFAT | HEFI2019C9_FREESUGARS | HEFI2019C10_SODIUM | HEFI2019_TOTAL_SCORE |
|:----------------------|--------------:|-------------------:|-------------------:|--------------------:|--------------------:|---------------------:|---------------------:|----------------:|----------------------:|-------------------:|---------------------:|
| All, 2y or older      |           9.5 |                1.2 |                1.4 |                 5.0 |                 1.6 |                  7.7 |                  2.2 |             4.0 |                   7.9 |                5.0 |                 45.5 |
| 2 to 3 y              |          10.3 |                1.1 |                1.3 |                 5.0 |                 1.0 |                  8.1 |                  0.9 |             2.6 |                   7.5 |                6.6 |                 44.3 |
| 4 to 8 y              |           9.0 |                1.3 |                1.2 |                 5.0 |                 1.2 |                  7.3 |                  1.3 |             3.9 |                   4.5 |                5.6 |                 40.3 |
| Male, 9 to 13 y       |           7.5 |                1.4 |                1.3 |                 5.0 |                 0.9 |                  7.2 |                  1.5 |             3.6 |                   4.5 |                5.0 |                 37.9 |
| Female, 9 to 13 y     |           8.5 |                1.2 |                1.1 |                 4.7 |                 0.7 |                  7.1 |                  1.3 |             3.6 |                   4.2 |                5.5 |                 38.0 |
| Male, 14 to 18 y      |           7.3 |                1.3 |                1.3 |                 5.0 |                 1.3 |                  6.9 |                  2.0 |             3.9 |                   5.0 |                5.0 |                 39.1 |
| Female, 14 to 18 y    |           8.4 |                1.2 |                1.3 |                 4.9 |                 1.3 |                  7.7 |                  1.6 |             3.2 |                   4.6 |                5.3 |                 39.7 |
| Male, 19 to 30 y      |           7.0 |                1.0 |                1.2 |                 5.0 |                 1.5 |                  7.0 |                  2.3 |             3.8 |                   7.3 |                4.8 |                 40.9 |
| Female, 19 to 30 y    |           9.5 |                1.0 |                1.3 |                 4.9 |                 1.2 |                  7.9 |                  2.1 |             4.1 |                   7.2 |                5.1 |                 44.2 |
| Male, 31 to 50 y      |           8.7 |                1.1 |                1.3 |                 5.0 |                 1.8 |                  7.3 |                  2.4 |             4.2 |                   8.4 |                4.9 |                 45.3 |
| Female, 31 to 50 y    |          10.7 |                1.1 |                1.4 |                 5.0 |                 1.9 |                  8.3 |                  2.3 |             3.9 |                   9.2 |                4.8 |                 48.6 |
| Male, 51 to 70 y      |           9.5 |                1.2 |                1.5 |                 4.9 |                 1.7 |                  7.4 |                  2.6 |             4.3 |                   9.6 |                4.9 |                 47.6 |
| Female, 51 to 70 y    |          11.5 |                1.2 |                1.7 |                 4.8 |                 1.8 |                  8.4 |                  2.3 |             4.3 |                   8.9 |                5.5 |                 50.4 |
| Male, 71 y or older   |          10.6 |                1.2 |                1.5 |                 4.4 |                 1.4 |                  7.6 |                  2.1 |             3.9 |                   8.7 |                4.8 |                 46.3 |
| Female, 71 y or older |          11.2 |                1.5 |                1.7 |                 4.6 |                 1.4 |                  8.4 |                  1.9 |             3.9 |                   8.2 |                4.7 |                 47.5 |

Mean HEFI-2019 score among Canadians 2y+ (CCHS 2015 - Nutrition)

``` r
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
```

    ## Joining, by = c("ADM_RNO", "SUPPID")
    ## inner_join: added 5 columns (WTS_P, drig, sex, age, smoking)
    ## > rows only in x ( 0)
    ## > rows only in y ( 0)
    ## > matched rows 27,544
    ## > ========
    ## > rows total 27,544
    ## filter: removed 13,636 rows (50%), 13,908 rows remaining

``` r
# 2) Calculate mean intakes of HEFI-2019 dietary constituents, weighted using sampling weights

  # note: sampling weights are applied, but variance ignored for this example

  # 2.1) define vectors of HEFI-2019 dietary constituents
  hefi2019_vars <- names(intake_per24hr[,3:(ncol(intake_per24hr)-1)])
  hefi2019_vars
```

    ##  [1] "wg"         "rg"         "pfab"       "pfpb"       "otherfoods"
    ##  [6] "vf"         "water"      "otherbevs"  "milk"       "plantbev"  
    ## [11] "freesugars" "energy"     "sfa"        "mufa"       "pufa"      
    ## [16] "sodium"

``` r
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
```

    ## Healthy Eating Food Index-2019 Scoring Algorithm R version 1.2
    ## filter: removed 29 rows (<1%), 13,879 rows remaining
    ## group_by: one grouping variable (smoking)
    ## summarise: now 2 rows and 17 columns, ungrouped

``` r
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
```

    ## pivot_longer: reorganized (HEFI2019C1_VF, HEFI2019C2_WHOLEGR, HEFI2019C3_GRRATIO, HEFI2019C4_PROFOODS, HEFI2019C5_PLANTPRO, …) into (HEFI2019_components, value) [was 2x12, now 22x3]
    ## pivot_wider: reorganized (smoking, value) into (SMK_0, SMK_1) [was 22x3, now 11x3]

``` r
# 4) show results
  popratio_scored_smk_diff |>
    knitr::kable(
      digits=1,
      col.names = c("Components", "Non-smoker","Smoker","Difference"),
      caption = "Mean HEFI-2019 scores estimated using the population ratio method, by smoking status (CCHS 2015 - Nutrition)"
    )
```

| Components            | Non-smoker | Smoker | Difference |
|:----------------------|-----------:|-------:|-----------:|
| HEFI2019C1_VF         |       10.1 |    7.8 |       -2.3 |
| HEFI2019C2_WHOLEGR    |        1.2 |    0.8 |       -0.4 |
| HEFI2019C3_GRRATIO    |        1.5 |    1.1 |       -0.5 |
| HEFI2019C4_PROFOODS   |        5.0 |    4.9 |       -0.1 |
| HEFI2019C5_PLANTPRO   |        1.8 |    1.3 |       -0.4 |
| HEFI2019C6_BEVERAGES  |        7.9 |    7.2 |       -0.7 |
| HEFI2019C7_FATTYACID  |        2.4 |    2.2 |       -0.2 |
| HEFI2019C8_SFAT       |        4.2 |    3.8 |       -0.4 |
| HEFI2019C9_FREESUGARS |        9.0 |    7.1 |       -1.9 |
| HEFI2019C10_SODIUM    |        5.0 |    4.7 |       -0.3 |
| HEFI2019_TOTAL_SCORE  |       48.1 |   40.9 |       -7.2 |

Mean HEFI-2019 scores estimated using the population ratio method, by
smoking status (CCHS 2015 - Nutrition)

``` r
# *********************************************************************** #
#                                                                         #
#          Variance estimation for the population ratio method            #
#                                                                         #
# *********************************************************************** #

# ...

notrun <- function(){
# Load and prepare bootstrap replicate weights required for variance estimation
bsw <-
  data.table::fread(file.path(external_drive,"CCHS_Nutrition_2015_PUMF","Bootstrap",
                              "Data_Donnee","b5.txt"),
                    stringsAsFactors = F, data.table= F, header=F,
                    nThread = parallel::detectCores()-1, # note: a bit overkill, but why not
                    col.names = c("ADM_RNO","WTS_P",paste0("BSW",seq(1,500)))) |>
  # keep only respondents identified above
  dplyr::right_join(cchs_data[,"ADM_RNO"]) |>
  # remove sampling weights so that only 1 copy exists in <cchs_data>
  dplyr::select(-WTS_P)
}
```
