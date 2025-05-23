Descriptive statistics based on repeated 24-h recall: HEFI-2019 scores
using the NCI multivariate method - model and simulations
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
```

Note 1: a hard coded path (<external_drive>) is used since files are on
an external drive. Usually we would want analysis to be self-contained,
but survey files are a bit large. Also, the data are set up exactly as
they are once obtained from Statistics Canada

Note 2: data pertaining to the 2015 Canadian Community Health Survey -
Nutrition (Public Use Microdata Files) are available upon request to
Statistics Canada online:
<https://www150.statcan.gc.ca/n1/en/catalogue/82M0024X2018001>

``` r
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
```

    ## data.table 1.17.99 IN DEVELOPMENT built 2025-03-29 04:23:09 UTC; root using 4 threads (see ?getDTthreads).  Latest news: r-datatable.com
    ## **********
    ## This development version of data.table was built more than 4 weeks ago. Please update: data.table::update_dev_pkg()
    ## **********

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'
    ## 
    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidylog)
```

    ## 
    ## Attaching package: 'tidylog'
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     add_count, add_tally, anti_join, count, distinct, distinct_all, distinct_at, distinct_if, filter,
    ##     filter_all, filter_at, filter_if, full_join, group_by, group_by_all, group_by_at, group_by_if,
    ##     inner_join, left_join, mutate, mutate_all, mutate_at, mutate_if, relocate, rename, rename_all, rename_at,
    ##     rename_if, rename_with, right_join, sample_frac, sample_n, select, select_all, select_at, select_if,
    ##     semi_join, slice, slice_head, slice_max, slice_min, slice_sample, slice_tail, summarise, summarise_all,
    ##     summarise_at, summarise_if, summarize, summarize_all, summarize_at, summarize_if, tally, top_frac, top_n,
    ##     transmute, transmute_all, transmute_at, transmute_if, ungroup
    ## 
    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

``` r
## NCI multivariate method
library(ncimultivar)

## computing
library(tictoc)
```

    ## 
    ## Attaching package: 'tictoc'
    ## 
    ## The following object is masked from 'package:data.table':
    ## 
    ##     shift

``` r
library(parallel)
library(future)
library(doFuture)
```

    ## Loading required package: foreach

``` r
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
```

note1: for the NCI method, an input data frame in long (or tall) format
is required, where repeated 24-h dietary recall appear on separate rows.

note2: for this example, we focus on children aged 13-17 inclusively.

note3: we remove 24-h dietary recall with less than 250 kcal. This
cut-off is used since intakes \<250 kcal cannot plausibly inform
hypothetical unobserved usual intakes (long-term average), but it is
challenging to decide which energy cut-off value is plausibly related to
long-term intakes beyond that.

``` r
# ********************************************** #
#            Intake + sociodemo data             #
# ********************************************** #

# 1) load prepared CCHS data

## Summary nutrient intakes and sociodemographic characteristics
load(file = file.path(dir_processed, "CCHS2015_HS_NCI.rdata"))
dim(CCHS2015_HS_NCI); names(CCHS2015_HS_NCI)
```

    ## [1] 27544    19

    ##  [1] "ADM_RNO"     "WTS_P"       "SUPPID"      "r24_day"     "r24_month"   "r24_nfoods"  "province"    "age"        
    ##  [9] "sex"         "drig"        "education"   "smoking"     "r24_weekend" "foodwgt"     "energy"      "sodium"     
    ## [17] "sfa"         "mufa"        "pufa"

``` r
## Dietary constituents required for the HEFI-2019
load(file = file.path(dir_processed, "CCHS2015_HEFI2019_PER24HR.rdata"))
dim(CCHS2015_HEFI2019_PER24HR); names(CCHS2015_HEFI2019_PER24HR)
```

    ## [1] 27544    19

    ##  [1] "ADM_RNO"        "SUPPID"         "wg"             "rg"             "pfab"           "pfpb"           "otherfoods"    
    ##  [8] "vf"             "water"          "otherbevs"      "milk"           "plantbev"       "freesugars"     "energy"        
    ## [15] "sfa"            "mufa"           "pufa"           "sodium"         "nonzero_energy"

``` r
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
```

    ## select: dropped 12 variables (r24_day, r24_month, r24_nfoods, province, education, …)
    ## full_join: added 17 columns (wg, rg, pfab, pfpb, otherfoods, …)
    ##            > rows only in select(CCHS2015_HS_NCI,..       0
    ##            > rows only in CCHS2015_HEFI2019_PER24HR       0
    ##            > matched rows                            27,544
    ##            >                                        ========
    ##            > rows total                              27,544
    ## filter: removed 24,491 rows (89%), 3,053 rows remaining
    ## filter: removed 9 rows (<1%), 3,044 rows remaining
    ## mutate: new variable 'r24_day2' (double) with 2 unique values and 0% NA
    ##         new variable 'drig_m_9_13' (double) with 2 unique values and 0% NA
    ##         new variable 'drig_f_9_13' (double) with 2 unique values and 0% NA
    ##         new variable 'drig_m_14_18' (double) with 2 unique values and 0% NA
    ##         new variable 'drig_f_14_18' (double) with 2 unique values and 0% NA
    ## select: dropped 2 variables (WTS_P, nonzero_energy)

``` r
# note: A total of 10x 24-h dietary recalls had energy <250 kcal

## overview
dim(intake_and_sociodeom); names(intake_and_sociodeom)
```

    ## [1] 3044   27

    ##  [1] "ADM_RNO"      "SUPPID"       "r24_weekend"  "drig"         "sex"          "age"          "wg"          
    ##  [8] "rg"           "pfab"         "pfpb"         "otherfoods"   "vf"           "water"        "otherbevs"   
    ## [15] "milk"         "plantbev"     "freesugars"   "energy"       "sfa"          "mufa"         "pufa"        
    ## [22] "sodium"       "r24_day2"     "drig_m_9_13"  "drig_f_9_13"  "drig_m_14_18" "drig_f_14_18"

``` r
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
```

    ## select: dropped 10 variables (ADM_RNO, r24_weekend, drig, sex, age, …)

| **Characteristic** | **1** N = 2,025 | **2** N = 1,019 |
|:---|:--:|:--:|
| Whole grain foods, RA/d | 0.87 \[0.00 to 18.96\] | 0.75 \[0.00 to 11.63\] |
| Non-whole grain foods, RA/d | 2.62 \[0.00 to 25.97\] | 2.44 \[0.00 to 24.19\] |
| Protein foods, animal-based, RA/d (excludes bev.) | 2.36 \[0.00 to 27.39\] | 2.29 \[0.00 to 26.75\] |
| Protein foods, plant-based, RA/d (excludes bev.) | 0.46 \[0.00 to 38.49\] | 0.32 \[0.00 to 8.27\] |
| Other foods, RA/d | 4.5 \[0.0 to 51.7\] | 3.7 \[0.0 to 106.1\] |
| Vegetables and fruits, RA/d | 2.62 \[0.00 to 19.67\] | 2.46 \[0.00 to 14.77\] |
| Water and unsweetened beverages, g/d | 926 \[0 to 8,449\] | 820 \[0 to 5,609\] |
| Other (sweetened) beverages, g/d | 465 \[0 to 3,862\] | 359 \[0 to 1,994\] |
| Unsweetened milk, g/d | 221 \[0 to 3,682\] | 209 \[0 to 2,044\] |
| Unsweetened plant-based bev. with protein, g/d |  |  |
| 0 | 2,025 (100%) | 1,019 (100%) |
| Total free sugars, g/d | 66 (38, 109) | 52 (27, 89) |
| Total energy, kcal/d | 2,048 \[267 to 13,651\] | 1,822 \[301 to 5,758\] |
| Total saturated fats, g/d | 26 \[1 to 196\] | 24 \[0 to 176\] |
| Total monounsaturated fats, g/d | 27 \[0 to 233\] | 25 \[0 to 113\] |
| Total polyunsaturated fats, g/d | 15 \[0 to 166\] | 13 \[1 to 74\] |
| Total sodium, mg/d | 2,929 \[162 to 20,121\] | 2,701 \[139 to 10,944\] |

Mean HEFI-2019 dietary constituents and range (CCHS 2015 - Nutrition),
by 24-h dietary recall

note: unsweetened plant-based beverages with sufficient protein have not
been reported (all values=0).

``` r
## confirm recoding
with(intake_and_sociodeom, table(drig, drig_m_9_13))
```

    ##     drig_m_9_13
    ## drig    0    1
    ##    4    0  308
    ##    5  290    0
    ##    6 1157    0
    ##    7 1289    0

``` r
with(intake_and_sociodeom, table(drig, drig_f_9_13))
```

    ##     drig_f_9_13
    ## drig    0    1
    ##    4  308    0
    ##    5    0  290
    ##    6 1157    0
    ##    7 1289    0

``` r
with(intake_and_sociodeom, table(drig, drig_m_14_18))
```

    ##     drig_m_14_18
    ## drig    0    1
    ##    4  308    0
    ##    5  290    0
    ##    6    0 1157
    ##    7 1289    0

``` r
with(intake_and_sociodeom, table(drig, drig_f_14_18))
```

    ##     drig_f_14_18
    ## drig    0    1
    ##    4  308    0
    ##    5  290    0
    ##    6 1157    0
    ##    7    0 1289

``` r
with(intake_and_sociodeom, table(SUPPID, r24_day2))
```

    ##       r24_day2
    ## SUPPID    0    1
    ##      1 2025    0
    ##      2    0 1019

``` r
# ********************************************** #
#             Bootstrap weight data              #
# ********************************************** #
```

note: variance is estimated using (500) bootstrap replicate weights in
CCHS 2015 - Nutrition.

``` r
# 1) output list of (unique) respondent to output BSW data
respondent_list <-
  intake_and_sociodeom |>
  distinct(ADM_RNO)
```

    ## distinct: removed 1,018 rows (33%), 2,026 rows remaining

``` r
dim(respondent_list); names(respondent_list)
```

    ## [1] 2026    1

    ## [1] "ADM_RNO"

``` r
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
```

    ## right_join: added no columns
    ##             > rows only in bsw             (18,461)
    ##             > rows only in respondent_list       0
    ##             > matched rows                   2,026
    ##             >                              ========
    ##             > rows total                     2,026
    ## rename: renamed one variable (BSW0)

note: all rows from ‘preNCI_respondent_list’ should be matched

``` r
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
```

    ## filter: removed 1,019 rows (33%), 2,025 rows remaining
    ## select: dropped 11 variables (ADM_RNO, SUPPID, r24_weekend, drig, sex, …)

``` r
proportions <- numeric(length = ncol(intake_day1))

for (i in seq_along(intake_day1)) {
  # Calculate mean proportion of values equal to 0 in each column
  proportions[i] <- mean(intake_day1[[i]] == 0, na.rm = TRUE)
}
# add names for clarity
proportions <- setNames(proportions, names(intake_day1))

# Print Pr(X=0) for each dietary constituents, descending order:
sort(round(proportions*100,1), decreasing=TRUE)
```

    ##   plantbev       pfpb         wg       milk  otherbevs      water       pfab         vf         rg otherfoods freesugars 
    ##      100.0       75.2       48.0       36.0       24.0        7.2        5.9        5.7        4.9        4.6        0.4 
    ##     energy        sfa       mufa       pufa     sodium 
    ##        0.0        0.0        0.0        0.0        0.0

note: pfpb, wg, milk and otherbevs would be episodically-consumed
dietary constituents based on a cut-off of 10% of zero.

``` r
# 2) Create vector of variables to be used within the NCI multivariate algorithm

## covariates
vars_z <- c("r24_weekend", "r24_day2", "drig_f_9_13", "drig_m_14_18", "drig_f_14_18")
```

note: the ‘drig_m_9_13’ dummy variable is omitted, so k-1 dummy are used
in the model, where k is the number of level based on the original DRI
group variable for that sample (k=4).

``` r
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
```

    ## select: dropped 500 variables (BSW1, BSW2, BSW3, BSW4, BSW5, …)
    ## Joining with `by = join_by(ADM_RNO)`
    ## left_join: added one column (BSW0)
    ##            > rows only in intake_and_sociodeom           0
    ##            > rows only in select(bsw, "ADM_RNO", .. (    0)
    ##            > matched rows                            3,044
    ##            >                                        =======
    ##            > rows total                              3,044
    ## select: dropped 5 variables (drig, sex, age, plantbev, drig_m_9_13)

``` r
# confirm lack of missing values:
colSums(is.na(preNCI[,c(vars_diet_all, vars_z, wts_sample)]))
```

    ##         pfpb           wg         milk    otherbevs        water         pfab           vf           rg   otherfoods 
    ##            0            0            0            0            0            0            0            0            0 
    ##   freesugars       energy          sfa         mufa         pufa       sodium  r24_weekend     r24_day2  drig_f_9_13 
    ##            0            0            0            0            0            0            0            0            0 
    ## drig_m_14_18 drig_f_14_18         BSW0 
    ##            0            0            0

``` r
if(sum(colSums(is.na(preNCI[,c(vars_diet_all, vars_z, wts_sample)])))==0) print("No missing values.") else message("Warning! Some values are missing!")
```

    ## [1] "No missing values."

note: should show zero missing values for all variables

``` r
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
```

``` r
##### 1) Winsorization #####
# ********************************************** #
#                 Winsorization                  #
# ********************************************** #
```

goal: identify extreme values and mitigate their impact using
Winsorization

note 1: in this example, only the UPPER values of intakes are winsorized

note 2: to avoid repetitive code, every dietary constituent is looped
through the winsorization algorithm

``` r
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
```

    ## Winsorization of x1, pfpb: max. value 17.7

    ## mutate: changed one value (<1%) of 'pfpb' (0 new NAs)
    ## Winsorization of x2, wg: max. value 10.31
    ## 
    ## mutate: changed 4 values (<1%) of 'wg' (0 new NAs)
    ## Winsorization of x3, milk: max. value 1479.95
    ## 
    ## mutate: changed 15 values (<1%) of 'milk' (0 new NAs)
    ## Winsorization of x4, otherbevs: none.
    ## 
    ## Winsorization of x5, water: max. value 4741.13
    ## 
    ## mutate: changed 9 values (<1%) of 'water' (0 new NAs)
    ## Winsorization of x6, pfab: max. value 16.42
    ## 
    ## mutate: changed 2 values (<1%) of 'pfab' (0 new NAs)
    ## Winsorization of x7, vf: max. value 14.69
    ## 
    ## mutate: changed 6 values (<1%) of 'vf' (0 new NAs)
    ## Winsorization of x8, rg: max. value 14.87
    ## 
    ## mutate: changed 7 values (<1%) of 'rg' (0 new NAs)
    ## Winsorization of x9, otherfoods: max. value 30.61
    ## 
    ## mutate: changed 7 values (<1%) of 'otherfoods' (0 new NAs)
    ## Winsorization of x10, freesugars: max. value 409.55
    ## 
    ## mutate: changed 5 values (<1%) of 'freesugars' (0 new NAs)
    ## Winsorization of x11, energy: max. value 7554.86
    ## 
    ## mutate: changed one value (<1%) of 'energy' (0 new NAs)
    ## Winsorization of x12, sfa: max. value 135.1
    ## 
    ## mutate: changed 3 values (<1%) of 'sfa' (0 new NAs)
    ## Winsorization of x13, mufa: max. value 139.93
    ## 
    ## mutate: changed one value (<1%) of 'mufa' (0 new NAs)
    ## Winsorization of x14, pufa: max. value 82.6
    ## 
    ## mutate: changed 4 values (<1%) of 'pufa' (0 new NAs)
    ## Winsorization of x15, sodium: max. value 11083.25
    ## 
    ## mutate: changed 5 values (<1%) of 'sodium' (0 new NAs)

``` r
# Step 3) Assessment. Find the maximum value for each variable AFTER winsorization
max_values_after <- apply(preNCI[,c(vars_diet_all)], 2, max)

# Compare max before and after
print(round(max_values_before,1))
```

    ##       pfpb         wg       milk  otherbevs      water       pfab         vf         rg otherfoods freesugars     energy 
    ##       38.5       19.0     3681.7     3862.0     8449.3       27.4       19.7       26.0      106.1      490.1    13650.7 
    ##        sfa       mufa       pufa     sodium 
    ##      195.8      232.7      166.0    20121.3

``` r
print(round(max_values_after,1))
```

    ##       pfpb         wg       milk  otherbevs      water       pfab         vf         rg otherfoods freesugars     energy 
    ##       17.7       10.3     1479.9     3862.0     4741.1       16.4       14.7       14.9       30.6      409.6     7554.9 
    ##        sfa       mufa       pufa     sodium 
    ##      135.1      139.9       82.6    11083.3

``` r
print(round(max_values_before-max_values_after,1))
```

    ##       pfpb         wg       milk  otherbevs      water       pfab         vf         rg otherfoods freesugars     energy 
    ##       20.8        8.6     2201.7        0.0     3708.1       11.0        5.0       11.1       75.5       80.6     6095.8 
    ##        sfa       mufa       pufa     sodium 
    ##       60.7       92.7       83.4     9038.1

``` r
# Show report

## add variable names for clarity
names(winsorize_x) <- vars_diet_all

print(winsorize_x)
```

    ## $pfpb
    ##     ADM_RNO SUPPID original.value winsorized.value
    ## 855    5700      1       38.48693         17.69991
    ## 
    ## $wg
    ##      ADM_RNO SUPPID original.value winsorized.value
    ## 417     2811      2       11.62549         10.31391
    ## 705     4846      1       13.24570         10.31391
    ## 1780   11964      1       18.96000         10.31391
    ## 2581   17330      1       11.68525         10.31391
    ## 
    ## $milk
    ##      ADM_RNO SUPPID original.value winsorized.value
    ## 145     1065      1       1642.367         1479.946
    ## 293     2013      2       1660.487         1479.946
    ## 335     2241      2       1853.921         1479.946
    ## 1499   10253      2       2044.183         1479.946
    ## 1646   11199      1       1835.949         1479.946
    ## 1786   11987      1       3182.280         1479.946
    ## 2267   15288      1       2800.552         1479.946
    ## 2297   15504      1       1480.028         1479.946
    ## 2339   15855      2       1505.669         1479.946
    ## 2402   16219      1       1608.797         1479.946
    ## 2406   16264      1       2082.088         1479.946
    ## 2409   16309      1       1546.920         1479.946
    ## 2613   17521      1       3681.670         1479.946
    ## 2726   18285      2       1631.180         1479.946
    ## 2770   18588      1       1660.675         1479.946
    ## 
    ## $otherbevs
    ## [1] ADM_RNO          SUPPID           original.value   winsorized.value
    ## <0 rows> (or 0-length row.names)
    ## 
    ## $water
    ##      ADM_RNO SUPPID original.value winsorized.value
    ## 49       406      1       5399.109          4741.13
    ## 216     1492      2       5609.464          4741.13
    ## 759     5200      1       8449.255          4741.13
    ## 2225   15021      1       5304.960          4741.13
    ## 2226   15021      2       5350.000          4741.13
    ## 2421   16375      1       4865.897          4741.13
    ## 2547   17131      1       5001.846          4741.13
    ## 2620   17540      1       7471.880          4741.13
    ## 2859   19195      2       4942.976          4741.13
    ## 
    ## $pfab
    ##      ADM_RNO SUPPID original.value winsorized.value
    ## 1632   11093      2       26.75446           16.416
    ## 2406   16264      1       27.39395           16.416
    ## 
    ## $vf
    ##      ADM_RNO SUPPID original.value winsorized.value
    ## 1153    7743      1       19.67194         14.69163
    ## 1242    8406      1       17.23896         14.69163
    ## 1703   11548      1       17.00026         14.69163
    ## 1704   11548      2       14.76795         14.69163
    ## 2048   13800      1       18.72834         14.69163
    ## 2358   15964      1       18.64194         14.69163
    ## 
    ## $rg
    ##      ADM_RNO SUPPID original.value winsorized.value
    ## 364     2449      1       24.77116          14.8681
    ## 702     4821      1       15.79288          14.8681
    ## 751     5162      1       25.96743          14.8681
    ## 1842   12343      1       14.92221          14.8681
    ## 2108   14207      2       24.18657          14.8681
    ## 2406   16264      1       18.77153          14.8681
    ## 2451   16550      1       14.91389          14.8681
    ## 
    ## $otherfoods
    ##      ADM_RNO SUPPID original.value winsorized.value
    ## 317     2130      2      106.09107         30.60694
    ## 494     3314      2       32.11460         30.60694
    ## 1177    7912      1       34.92231         30.60694
    ## 1427    9692      1       51.69343         30.60694
    ## 2423   16376      1       30.94132         30.60694
    ## 2637   17694      1       36.91078         30.60694
    ## 2730   18304      1       32.96600         30.60694
    ## 
    ## $freesugars
    ##      ADM_RNO SUPPID original.value winsorized.value
    ## 1079    7151      1       434.9420         409.5531
    ## 1151    7738      1       490.1364         409.5531
    ## 1278    8592      1       454.6627         409.5531
    ## 2423   16376      1       450.1654         409.5531
    ## 2637   17694      1       428.2215         409.5531
    ## 
    ## $energy
    ##      ADM_RNO SUPPID original.value winsorized.value
    ## 2406   16264      1       13650.69         7554.856
    ## 
    ## $sfa
    ##      ADM_RNO SUPPID original.value winsorized.value
    ## 629     4293      1       154.4698         135.0987
    ## 1632   11093      2       175.6266         135.0987
    ## 2406   16264      1       195.8208         135.0987
    ## 
    ## $mufa
    ##      ADM_RNO SUPPID original.value winsorized.value
    ## 2406   16264      1       232.6652         139.9298
    ## 
    ## $pufa
    ##      ADM_RNO SUPPID original.value winsorized.value
    ## 1231    8327      1      166.00344         82.59753
    ## 1478   10146      1      101.68289         82.59753
    ## 2406   16264      1      163.42451         82.59753
    ## 2828   18992      1       89.76662         82.59753
    ## 
    ## $sodium
    ##      ADM_RNO SUPPID original.value winsorized.value
    ## 102      742      1       11456.05         11083.25
    ## 364     2449      1       11517.69         11083.25
    ## 2406   16264      1       20121.31         11083.25
    ## 2556   17191      1       11248.22         11083.25
    ## 2860   19196      1       12267.68         11083.25

``` r
##### 2) Box-Cox transformation #####
# ********************************************** #
#             Box-Cox transformation             #
# ********************************************** #
```

goal: run Box-Cox survey with covariates to get transformation parameter

note: to avoid repetitive code, every dietary constituent is looped
through Box-Cox survey

``` r
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
```

    ## Box-Cox lambda of x1, pfpb: 0.06

    ## Box-Cox lambda of x2, wg: 0.19

    ## Box-Cox lambda of x3, milk: 0.39

    ## Box-Cox lambda of x4, otherbevs: 0.07

    ## Box-Cox lambda of x5, water: 0.39

    ## Box-Cox lambda of x6, pfab: 0.22

    ## Box-Cox lambda of x7, vf: 0.41

    ## Box-Cox lambda of x8, rg: 0.33

    ## Box-Cox lambda of x9, otherfoods: 0.24

    ## Box-Cox lambda of x10, freesugars: 0.37

    ## Box-Cox lambda of x11, energy: 0.12

    ## Box-Cox lambda of x12, sfa: 0.18

    ## Box-Cox lambda of x13, mufa: 0.2

    ## Box-Cox lambda of x14, pufa: 0.12

    ## Box-Cox lambda of x15, sodium: 0.29

``` r
# Create a dataset with transformation parameter to use in preprocessor macro
```

NCI note: the variable name must be in a column called `variable` and
the transformation parameter must be in a column called `tran_lambda`

``` r
boxcox_lambda_x <-
  data.frame(
    variable = vars_diet_all,
    tran_lambda = unlist(tran_lambda)
  )

# Show values
print(boxcox_lambda_x)
```

    ##      variable tran_lambda
    ## 1        pfpb        0.06
    ## 2          wg        0.19
    ## 3        milk        0.39
    ## 4   otherbevs        0.07
    ## 5       water        0.39
    ## 6        pfab        0.22
    ## 7          vf        0.41
    ## 8          rg        0.33
    ## 9  otherfoods        0.24
    ## 10 freesugars        0.37
    ## 11     energy        0.12
    ## 12        sfa        0.18
    ## 13       mufa        0.20
    ## 14       pufa        0.12
    ## 15     sodium        0.29

``` r
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
```

    ##              variable     minamount
    ## pfpb             pfpb   0.005129795
    ## wg                 wg   0.004776275
    ## milk             milk   0.515000000
    ## otherbevs   otherbevs   0.695534500
    ## water           water   0.500845000
    ## pfab             pfab   0.004081045
    ## vf                 vf   0.001621135
    ## rg                 rg   0.005018683
    ## otherfoods otherfoods   0.022671545
    ## freesugars freesugars   0.007153146
    ## energy         energy 133.252370500
    ## sfa               sfa   0.128322000
    ## mufa             mufa   0.227410500
    ## pufa             pufa   0.320091500
    ## sodium         sodium  69.671218500

``` r
###### Save pre-processing data for reference ######
# ********************************************** #
#     Save pre-processing data for reference     #
# ********************************************** #

save(winsorize_x,boxcox_lambda_x, minimum_amount_x,
     file = file.path(dir_nci_original,"preprocess.rdata"))
```

``` r
##### Start bootstrap loop #####
# ******************************************************** #
#                   Start bootstrap loop                   #
# ******************************************************** #

# (manual) indicate number of replicate to run

replicfirst <- 0
repliclast  <- 5
```

note 1: to run only base estimates (original data) set both
‘replicfirst’ and ‘repliclast’ to `0`

note 2: suggest to start with original sample first to validate
workflow, then, a few replicates (say 2 to 5), and only then, all 500
replicates.

note 3: in this example, all multivariate error models **and** full
simulation are saved. This provides maximal flexibility to generate
statistics of interest based on simulated data. However, considerable
disk space is required for both the model and simulations. Saving all
500 bootstrap replicate would require approximately 0.2 GB \* 500 = 100
GB of disk space for this example.

note 4: a parallel computing approach is demonstrated below to reduce
analysis time. Alternatively, a standard for-loop approach could be used
using the code `for (replicate.num in replicfirst:repliclast){...}`

``` r
# (auto) ensure required data and vectors are available
if(!(exists("bsw")) | !(exists("preNCI"))) load(file.path(dir_nci_original,"preNCI_n_bsw.rdata"))
if(!(exists("vars_diet_daily")) | !(exists("vars_diet_episodic"))) load(file.path(dir_nci_original,"predefined_vectors.rdata"))
if(!(exists("boxcox_lambda_x")) | !(exists("minimum_amount_x"))) load(file.path(dir_nci_original,"preprocess.rdata"))

dim(preNCI); names(preNCI)
```

    ## [1] 3044   23

    ##  [1] "ADM_RNO"      "SUPPID"       "r24_weekend"  "r24_day2"     "drig_f_9_13"  "drig_m_14_18" "drig_f_14_18"
    ##  [8] "pfpb"         "wg"           "milk"         "otherbevs"    "water"        "pfab"         "vf"          
    ## [15] "rg"           "otherfoods"   "freesugars"   "energy"       "sfa"          "mufa"         "pufa"        
    ## [22] "sodium"       "BSW0"

``` r
dim(bsw); names(bsw)[1:10]
```

    ## [1] 2026  502

    ##  [1] "ADM_RNO" "BSW0"    "BSW1"    "BSW2"    "BSW3"    "BSW4"    "BSW5"    "BSW6"    "BSW7"    "BSW8"

``` r
# (auto) prepare parallel computing
future::plan(multisession, workers = parallel::detectCores()-2)
doFuture::registerDoFuture()
future::nbrOfWorkers()
```

    ## [1] 6

``` r
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
```

    ## [1] "Starting Iteration 0"
    ##      variable tran_lambda    minamount tran_center tran_scale biomarker
    ## 1      energy        0.12 1.332524e+02  12.1626520  0.7850041     FALSE
    ## 2  freesugars        0.37 7.153146e-03   9.6729096  2.7925495     FALSE
    ## 3        milk        0.39 5.150000e-01  20.0250198  5.7278405     FALSE
    ## 4        mufa        0.20 2.274105e-01   4.3577111  0.8235826     FALSE
    ## 5   otherbevs        0.07 6.955345e-01   7.6791139  0.8040011     FALSE
    ## 6  otherfoods        0.24 2.267155e-02   1.1758101  1.0443373     FALSE
    ## 7        pfab        0.22 4.081045e-03   0.4969870  0.9154161     FALSE
    ## 8        pfpb        0.06 5.129795e-03   0.1138360  0.7470796     FALSE
    ## 9        pufa        0.12 3.200915e-01   2.9126547  0.6343207     FALSE
    ## 10         rg        0.33 5.018683e-03   0.7452979  0.8749363     FALSE
    ## 11        sfa        0.18 1.283220e-01   4.0961616  0.7825729     FALSE
    ## 12     sodium        0.29 6.967122e+01  30.1993985  3.7263904     FALSE
    ## 13         vf        0.41 1.621135e-03   0.7876292  0.9495266     FALSE
    ## 14      water        0.39 5.008450e-01  29.5276147  9.8946673     FALSE
    ## 15         wg        0.19 4.776275e-03   0.2810858  0.5834197     FALSE

    ## Joining with `by = join_by(ADM_RNO, BSW0)`

    ## Warning: UNRELIABLE VALUE: One of the foreach() iterations ('doFuture-1') unexpectedly generated random numbers without
    ## declaring so. There is a risk that those random numbers are not statistically sound and the overall results might be
    ## invalid. To fix this, use '%dorng%' from the 'doRNG' package instead of '%dopar%'. This ensures that proper, parallel-safe
    ## random numbers are produced via the L'Ecuyer-CMRG method. To disable this check, set option 'doFuture.rng.onMisuse' to
    ## "ignore".

    ## [1] "Starting Iteration 1"

    ## Joining with `by = join_by(ADM_RNO)`

    ## Warning: UNRELIABLE VALUE: One of the foreach() iterations ('doFuture-2') unexpectedly generated random numbers without
    ## declaring so. There is a risk that those random numbers are not statistically sound and the overall results might be
    ## invalid. To fix this, use '%dorng%' from the 'doRNG' package instead of '%dopar%'. This ensures that proper, parallel-safe
    ## random numbers are produced via the L'Ecuyer-CMRG method. To disable this check, set option 'doFuture.rng.onMisuse' to
    ## "ignore".

    ## [1] "Starting Iteration 2"

    ## Joining with `by = join_by(ADM_RNO)`

    ## Warning: UNRELIABLE VALUE: One of the foreach() iterations ('doFuture-3') unexpectedly generated random numbers without
    ## declaring so. There is a risk that those random numbers are not statistically sound and the overall results might be
    ## invalid. To fix this, use '%dorng%' from the 'doRNG' package instead of '%dopar%'. This ensures that proper, parallel-safe
    ## random numbers are produced via the L'Ecuyer-CMRG method. To disable this check, set option 'doFuture.rng.onMisuse' to
    ## "ignore".

    ## [1] "Starting Iteration 3"

    ## Joining with `by = join_by(ADM_RNO)`

    ## Warning: UNRELIABLE VALUE: One of the foreach() iterations ('doFuture-4') unexpectedly generated random numbers without
    ## declaring so. There is a risk that those random numbers are not statistically sound and the overall results might be
    ## invalid. To fix this, use '%dorng%' from the 'doRNG' package instead of '%dopar%'. This ensures that proper, parallel-safe
    ## random numbers are produced via the L'Ecuyer-CMRG method. To disable this check, set option 'doFuture.rng.onMisuse' to
    ## "ignore".

    ## [1] "Starting Iteration 4"

    ## Joining with `by = join_by(ADM_RNO)`

    ## Warning: UNRELIABLE VALUE: One of the foreach() iterations ('doFuture-5') unexpectedly generated random numbers without
    ## declaring so. There is a risk that those random numbers are not statistically sound and the overall results might be
    ## invalid. To fix this, use '%dorng%' from the 'doRNG' package instead of '%dopar%'. This ensures that proper, parallel-safe
    ## random numbers are produced via the L'Ecuyer-CMRG method. To disable this check, set option 'doFuture.rng.onMisuse' to
    ## "ignore".

    ## [1] "Starting Iteration 5"

    ## Joining with `by = join_by(ADM_RNO)`

    ## Warning: UNRELIABLE VALUE: One of the foreach() iterations ('doFuture-6') unexpectedly generated random numbers without
    ## declaring so. There is a risk that those random numbers are not statistically sound and the overall results might be
    ## invalid. To fix this, use '%dorng%' from the 'doRNG' package instead of '%dopar%'. This ensures that proper, parallel-safe
    ## random numbers are produced via the L'Ecuyer-CMRG method. To disable this check, set option 'doFuture.rng.onMisuse' to
    ## "ignore".

    ## [[1]]
    ## NULL
    ## 
    ## [[2]]
    ## NULL
    ## 
    ## [[3]]
    ## NULL
    ## 
    ## [[4]]
    ## NULL
    ## 
    ## [[5]]
    ## NULL
    ## 
    ## [[6]]
    ## NULL

``` r
tictoc::toc() # time out
```

    ## NCI MCMC (model+sim.; 15 components incl. 4 episodic), replicate 0 to 5: 1456.362 sec elapsed

``` r
##### End bootstrap loop #####
# ******************************************************** #
#                                                          #
#                                                          #
#                    End bootstrap loop                    #
#                                                          #
#                                                          #
# ******************************************************** #
```

Multivar MCMC model assessment

``` r
#### Multivar MCMC model assessment ####
# *********************************************************************** #
#                     Multivar MCMC model assessment                      #
# *********************************************************************** #
```

note: see *Trace Plots for NCI Multivar MCMC* using
`browseVignettes("ncimultivar")` for details.

``` r
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
```

    ## Trace plots created

``` r
#### End of code 03A ####
# *********************************************************************** #
#                            End of code 03A                              #
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
    ## [1] parallel  stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] furrr_0.3.1        doFuture_1.0.2     foreach_1.5.2      future_1.34.0      tictoc_1.2.1       ncimultivar_1.0.3 
    ## [7] tidylog_1.1.0      dplyr_1.1.4        data.table_1.17.99
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] future.apply_1.11.3 compiler_4.4.3      tidyselect_1.2.1    gtsummary_2.1.0     tidyr_1.3.1        
    ##  [6] globals_0.16.3      yaml_2.3.10         fastmap_1.2.0       here_1.0.1          R6_2.6.1           
    ## [11] cards_0.5.1         generics_0.1.3      knitr_1.50          iterators_1.0.14    tibble_3.2.1       
    ## [16] rprojroot_2.0.4     pillar_1.10.2       rlang_1.1.6         xfun_0.52           cli_3.6.5          
    ## [21] withr_3.0.2         magrittr_2.0.3      digest_0.6.37       rstudioapi_0.17.1   clisymbols_1.2.0   
    ## [26] lifecycle_1.0.4     vctrs_0.6.5         evaluate_1.0.3      glue_1.8.0          listenv_0.9.1      
    ## [31] codetools_0.2-20    parallelly_1.43.0   rmarkdown_2.29      purrr_1.0.4         tools_4.4.3        
    ## [36] pkgconfig_2.0.3     htmltools_0.5.8.1
