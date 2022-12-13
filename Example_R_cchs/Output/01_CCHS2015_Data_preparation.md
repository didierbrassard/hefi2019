Prepare data suitable to apply the HEFI-2019 scoring algorithm (CCHS
2015 - Nutrition)
================
Didier Brassard
2022-12-13

``` r
# *********************************************************************** #
#                                                                         #
#                      CCHS 2015 - Nutrition (PUMF)                       #
#                                                                         #
#    Prepare a data suitable to apply the HEFI-2019 scoring algorithm     #
#                                                                         #
#                        Author: Didier Brassard                          #
#                                                                         #
#                               Version 1                                 #
#                               2022-11-07                                #
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
  library(haven)
  library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.4.0      ✔ purrr   0.3.5 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.3      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

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
  library(readxl)
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

# suppress scientific notation
options(scipen = 9999)


# *********************************************************************** #
#                                                                         #
#            Import and format HEFI-2019 classification data              #
#                                                                         #
# *********************************************************************** #

# 1) Import Excel sheet with hefi2019 classification
hefifoodcat <-
  readxl::read_xlsx(file.path(sas_dir,"HEFI2019_foodcat_2022-03.xlsx"),
                    sheet="CAT")
```

    ## Warning: Expecting logical in F3248 / R3248C6: got 'Formerly <profoodsanimal>'

``` r
# 2) prepare classification to merge with other cchs files
hefifoodcat <-
  hefifoodcat |>
  mutate(
    # format the RA_g variable as numerical
    grams_for_1RA = ifelse(RA_g == "NA",NA,as.numeric(RA_g)),
    # recode hefi2019 foot categories for easier handling
    hefi2019subgrp = case_when(
      HEFI2019Cat == "vegfruits" ~ 1,
      HEFI2019Cat == "wholegrfoods" ~ 2,
      HEFI2019Cat == "nonwholegrfoods" ~ 3,
      HEFI2019Cat == "profoodsanimal" ~ 4,
      HEFI2019Cat == "profoodsplant" ~ 5,
      HEFI2019Cat == "otherfoods" ~ 6,
      HEFI2019Cat == "waterhealthybev" ~ 7,
      HEFI2019Cat == "unsweetmilk" ~ 8,
      HEFI2019Cat == "unsweetplantbevpro" ~ 9,
      HEFI2019Cat == "otherbeverages" ~ 10,
      TRUE ~ 99 ),
    # further classify one food with missing value for RA but can be considered in beverages nonetheless
    hefi2019subgrp = ifelse(FoodRecipeCode == 5628,10,hefi2019subgrp)
  ) |>
  # drop useless variables
  select(FoodRecipeCode, FDC_DEN, grams_for_1RA, hefi2019subgrp) |>
  # rename for merging purpose with cchs files
  rename(FID_CDE = FoodRecipeCode, FDC_DEN_foodcat = FDC_DEN) |>
  arrange(FID_CDE)
```

    ## Warning in ifelse(RA_g == "NA", NA, as.numeric(RA_g)): NAs introduced by
    ## coercion

    ## rename: renamed 2 variables (FID_CDE, FDC_DEN_foodcat)

``` r
  # generate labels for hefi2019subgrp

  hefifoodcat$hefi2019subgrp_f <-
    factor(hefifoodcat$hefi2019subgrp,
           levels = c(seq(1,10,1),99),
           labels = c('1-Vegetables and fruits',
                     '2-Whole-grain foods',
                     '3-Non-whole grain foods',
                     '4-Protein foods, animal-based',
                     '5-Protein foods, plant-based',
                     '6-Other foods',
                     '7-Water and other healthy beverages',
                     '8-Milk, unsweetened',
                     '9-Plant-based beverages with proteins, unsweetened',
                     '10-Other beverages',
                     '99-Not considered (e.g., herbs, spices, fats, oils)')
           )

  # Overview of all classified foods
  table(hefifoodcat$hefi2019subgrp_f) / nrow(hefifoodcat) *100
```

    ## 
    ##                             1-Vegetables and fruits 
    ##                                         15.82386364 
    ##                                 2-Whole-grain foods 
    ##                                          5.12784091 
    ##                             3-Non-whole grain foods 
    ##                                          8.42329545 
    ##                       4-Protein foods, animal-based 
    ##                                         26.42045455 
    ##                        5-Protein foods, plant-based 
    ##                                          5.92329545 
    ##                                       6-Other foods 
    ##                                         23.49431818 
    ##                 7-Water and other healthy beverages 
    ##                                          0.65340909 
    ##                                 8-Milk, unsweetened 
    ##                                          0.52556818 
    ##  9-Plant-based beverages with proteins, unsweetened 
    ##                                          0.02840909 
    ##                                  10-Other beverages 
    ##                                          5.29829545 
    ## 99-Not considered (e.g., herbs, spices, fats, oils) 
    ##                                          8.28125000

``` r
# save data
save(hefifoodcat,
     file = file.path(data_dir,"hefifoodcat.rdata"))

# *********************************************************************** #
#                                                                         #
#                   Import and format free sugars data                    #
#                                                                         #
# *********************************************************************** #

  # note 1: data on free sugars is available at
  # https://www.mdpi.com/article/10.3390/nu13051471/s1 (Rana et al. Nutrients 2021)

  # note 2: note: assuming the file is at the <path> location,
  # in a folder named <Example_SAS_cchs>

# 1) Import Excel sheet with free sugars data
freesugars <-
  readxl::read_xlsx(file.path(sas_dir,"nutrients-1173784-supplementary.xlsx"),
                    .name_repair = "universal")
```

    ## Warning: Coercing text to numeric in B3647 / R3647C2: '5451'

    ## Warning: Coercing text to numeric in B3650 / R3650C2: '4778'

    ## Warning: Coercing text to numeric in B3651 / R3651C2: '5297'

    ## Warning: Coercing text to numeric in B4448 / R4448C2: '7573'

    ## Warning: Expecting numeric in B6141 / R6141C2: got '501762 '

    ## New names:
    ## • `Reported in CCHS-Nutrition 2015 0=No 1=yes` ->
    ##   `Reported.in.CCHS.Nutrition.2015..0.No.1.yes`
    ## • `Name English` -> `Name.English`
    ## • `Name French` -> `Name.French`
    ## • `Estimate free sugar amount (g/100g)` ->
    ##   `Estimate.free.sugar.amount..g.100g.`

``` r
# prepare free sugars data to merge with other cchs files
freesugars <-
  freesugars |>
  filter(is.na(Code)==FALSE) |>
  mutate(
    # ensure code is numerical
    FID_CDE = as.numeric(gsub('\\D+','', Code)),
    # change to free sugars per 1 (one) gram
    freesugars_per_g = Estimate.free.sugar.amount..g.100g. / 100
  ) |>
  rename(Name_English = Name.English) |>
  select(FID_CDE,freesugars_per_g,Name_English) |>
  arrange(FID_CDE)
```

    ## filter: removed 2 rows (<1%), 7,039 rows remaining
    ## rename: renamed one variable (Name_English)

``` r
# save data
save(freesugars,
     file = file.path(data_dir,"freesugars.rdata"))

# *********************************************************************** #
#                                                                         #
#    Import and format total nutrient intakes and sociodemo data [HS]     #
#                                                                         #
# *********************************************************************** #

# 1) Import HS_NCI data, save to data folder
hs_nci <-
  read_sas(data_file = file.path(cchs_dir,"hs_nci.sas7bdat")) |>
  # keep only respondents aged 2 years and older (i.e., target population of Canada`s food guide)
  filter(DHH_AGE>=2) |>
  mutate(
    # recode smoking status (variable to use later for demonstration purpose)
      smoking = case_when(
        SMK_202 == 3 ~ 0,
        SMK_202 == 2 ~ 1,
        SMK_202 == 1 ~ 2 ),
    # recode education
    education = ifelse(EDUG21==9,NA,EDUG21)
  ) |>
  select(
    # id
    ADM_RNO, WTS_P,
    # 24hr
    SUPPID, ADMDD, ADM_MOI, R24DCNT,
    # characteristics
    GEO_PRV, DHH_AGE, DHH_SEX, DHHDDRI, education, smoking, ADMFW,
    # dietary intakes
    FSDDWTG, FSDDEKC, FSDDSOD, FSDDFAT, FSDDFAS, FSDDFAM, FSDDFAP
  ) |>
  rename(
    r24_weekend=ADMFW, r24_day=ADMDD, r24_month=ADM_MOI, r24_nfoods=R24DCNT,
    province=GEO_PRV, sex=DHH_SEX, age=DHH_AGE, drig=DHHDDRI,
    foodwgt=FSDDWTG, energy=FSDDEKC, sodium=FSDDSOD,
    sfa=FSDDFAS, mufa=FSDDFAM, pufa=FSDDFAP
  ) |>
  arrange(ADM_RNO,SUPPID)
```

    ## filter: removed 551 rows (2%), 27,544 rows remaining
    ## rename: renamed 14 variables (r24_day, r24_month, r24_nfoods, province, age, …)

``` r
# overview
  table(hs_nci$SUPPID)
```

    ## 
    ##     1     2 
    ## 20115  7429

``` r
  with(
    hs_nci[hs_nci$energy>0,],
    table(SUPPID)
  )
```

    ## SUPPID
    ##     1     2 
    ## 20103  7426

``` r
# save data
save(hs_nci,
     file = file.path(data_dir,"hs_nci.rdata"))

# *********************************************************************** #
#                                                                         #
#       Import and format food or ingredient level data [FID,FRL]         #
#                                                                         #
# *********************************************************************** #

# 1) Import FID + FRL data, save to temp folder since these are large files

frl <-
  haven::read_sas(data_file = file.path(cchs_dir,"frl.sas7bdat"))
save(frl,
     file = file.path(temp_dir,"frl.rdata"))

fid <-
  haven::read_sas(data_file = file.path(cchs_dir,"fid.sas7bdat"))
save(fid,
     file = file.path(temp_dir,"fid.rdata"))

  ## Load data upon second run
  #load(file.path(temp_dir,"frl.rdata"))
  #load(file.path(temp_dir,"fid.rdata"))

# 2) Append both data together and save to temp folder as FIDFRL

fidfrl <-
  rbind(fid,frl) |>
  arrange(ADM_RNO,SUPPID,SEQID)

  ## remove large temporary files
  rm(fid)
  rm(frl)

# 3) Import CFG data (useful to keep only `true` unique foods)

cfg <-
  haven::read_sas(data_file = file.path(cchs_dir,"cfg.sas7bdat")) |>
  arrange(ADM_RNO,SUPPID,SEQID)

# 4) merge FIDFRL with CFG

nutrients_list <- c("FID_WTG", "FID_EKC", "FID_CAR", "FID_FI", "FID_SUG", "FID_FAT", "FID_FAS", "FID_FAM",
                    "FID_FAP", "FID_FAL", "FID_FAN", "FID_CHO", "FID_PRO", "FID_ALC", "FID_RAE", "FID_DMG",
                    "FID_C", "FID_THI", "FID_RIB", "FID_NIA", "FID_B6", "FID_B12", "FID_FON", "FID_FOA",
                    "FID_DFE", "FID_FOL", "FID_CAL", "FID_PHO", "FID_MAG", "FID_IRO", "FID_ZIN", "FID_SOD",
                    "FID_POT", "FID_CAF", "FID_MOI", "FID_DHA", "FID_DPA", "FID_EPA", "FID_ODA")

fidfrl_cfg <-
  right_join(fidfrl, cfg) |>
  # recode missing values for nutrients
  mutate(
    across(all_of(nutrients_list),function(x) ifelse(x>99999,NA,x))
  )
```

    ## Joining, by = c("VERDATE", "ADM_RNO", "SUPPID", "SEQID")
    ## right_join: added 3 columns (CFGSBGRP, PRTSZ123, PRTSIZE4)
    ## > rows only in x (446,750)
    ## > rows only in y 0
    ## > matched rows 705,715
    ## > =========
    ## > rows total 705,715

``` r
save(fidfrl_cfg,
     file = file.path(temp_dir,"fidfrl_cfg.rdata"))

# 5) remove temporary files to save memory
  rm(fidfrl)
  rm(cfg)

# *********************************************************************** #
#                                                                         #
#         Merge hefi2019 classification, free sugars and fidfrl           #
#                                                                         #
# *********************************************************************** #

# Load data upon second run
  # load(file.path(data_dir,"hefifoodcat.rdata"))
  # load(file.path(data_dir,"freesugars.rdata"))
  # load(file.path(temp_dir,"fidfrl_cfg.rdata"))

cchs24hr_detailed <-
  fidfrl_cfg |>
  # keep relevant variables only in <fidfrl_cfg>
  select(FID_CDE,ADM_RNO,SUPPID,FID_WTG) |>
  # add HEFI-2019 food category and data on free sugars
  left_join(hefifoodcat) |>
  left_join(freesugars) |>
  mutate(
    #Calculate the number of reference amounts consumed
    ra_qty = ifelse(is.na(grams_for_1RA),NA,FID_WTG / grams_for_1RA),
    #Calculate intake of free sugars
    freesugars = ifelse(is.na(freesugars_per_g),NA,FID_WTG * freesugars_per_g)
  )
```

    ## Joining, by = "FID_CDE"
    ## left_join: added 4 columns (FDC_DEN_foodcat, grams_for_1RA, hefi2019subgrp,
    ## hefi2019subgrp_f)
    ## > rows only in x 0
    ## > rows only in y ( 3,490)
    ## > matched rows 705,715
    ## > =========
    ## > rows total 705,715
    ## Joining, by = "FID_CDE"
    ## left_join: added 2 columns (freesugars_per_g, Name_English)
    ## > rows only in x 0
    ## > rows only in y ( 3,489)
    ## > matched rows 705,715
    ## > =========
    ## > rows total 705,715

``` r
#confirm reported intakes are classified
table(is.na(cchs24hr_detailed$hefi2019subgrp_f))
```

    ## 
    ##  FALSE 
    ## 705715

``` r
cchs24hr_detailed |>
  dplyr::group_by(hefi2019subgrp_f) |>
  dplyr::summarise(freq=n()) |>
  dplyr::mutate(percent=freq/sum(freq)) |>
  gt() |>
  gt::fmt_percent(percent,decimals=1) |>
  gt::fmt_number(freq,decimals = 0) |>
  gt::tab_header(title=paste0("Repartition of all foods and drinks reported in CCHS 2015 - Nutrition (total=",
                              scales::number(nrow(cchs24hr_detailed),big.mark=','),')')
                 )
```

<div id="vozqniioaj" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#vozqniioaj .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#vozqniioaj .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#vozqniioaj .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#vozqniioaj .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#vozqniioaj .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#vozqniioaj .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vozqniioaj .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#vozqniioaj .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#vozqniioaj .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#vozqniioaj .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#vozqniioaj .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#vozqniioaj .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#vozqniioaj .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#vozqniioaj .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#vozqniioaj .gt_from_md > :first-child {
  margin-top: 0;
}

#vozqniioaj .gt_from_md > :last-child {
  margin-bottom: 0;
}

#vozqniioaj .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#vozqniioaj .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#vozqniioaj .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#vozqniioaj .gt_row_group_first td {
  border-top-width: 2px;
}

#vozqniioaj .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#vozqniioaj .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#vozqniioaj .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#vozqniioaj .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vozqniioaj .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#vozqniioaj .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#vozqniioaj .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#vozqniioaj .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vozqniioaj .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#vozqniioaj .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#vozqniioaj .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#vozqniioaj .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#vozqniioaj .gt_left {
  text-align: left;
}

#vozqniioaj .gt_center {
  text-align: center;
}

#vozqniioaj .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#vozqniioaj .gt_font_normal {
  font-weight: normal;
}

#vozqniioaj .gt_font_bold {
  font-weight: bold;
}

#vozqniioaj .gt_font_italic {
  font-style: italic;
}

#vozqniioaj .gt_super {
  font-size: 65%;
}

#vozqniioaj .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#vozqniioaj .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#vozqniioaj .gt_indent_1 {
  text-indent: 5px;
}

#vozqniioaj .gt_indent_2 {
  text-indent: 10px;
}

#vozqniioaj .gt_indent_3 {
  text-indent: 15px;
}

#vozqniioaj .gt_indent_4 {
  text-indent: 20px;
}

#vozqniioaj .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  <thead class="gt_header">
    <tr>
      <td colspan="3" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Repartition of all foods and drinks reported in CCHS 2015 - Nutrition (total=705,715)</td>
    </tr>
    
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="hefi2019subgrp_f">hefi2019subgrp_f</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="freq">freq</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="percent">percent</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="hefi2019subgrp_f" class="gt_row gt_center">1-Vegetables and fruits</td>
<td headers="freq" class="gt_row gt_right">146,866</td>
<td headers="percent" class="gt_row gt_right">20.8%</td></tr>
    <tr><td headers="hefi2019subgrp_f" class="gt_row gt_center">2-Whole-grain foods</td>
<td headers="freq" class="gt_row gt_right">24,971</td>
<td headers="percent" class="gt_row gt_right">3.5%</td></tr>
    <tr><td headers="hefi2019subgrp_f" class="gt_row gt_center">3-Non-whole grain foods</td>
<td headers="freq" class="gt_row gt_right">63,625</td>
<td headers="percent" class="gt_row gt_right">9.0%</td></tr>
    <tr><td headers="hefi2019subgrp_f" class="gt_row gt_center">4-Protein foods, animal-based</td>
<td headers="freq" class="gt_row gt_right">84,255</td>
<td headers="percent" class="gt_row gt_right">11.9%</td></tr>
    <tr><td headers="hefi2019subgrp_f" class="gt_row gt_center">5-Protein foods, plant-based</td>
<td headers="freq" class="gt_row gt_right">13,848</td>
<td headers="percent" class="gt_row gt_right">2.0%</td></tr>
    <tr><td headers="hefi2019subgrp_f" class="gt_row gt_center">6-Other foods</td>
<td headers="freq" class="gt_row gt_right">98,867</td>
<td headers="percent" class="gt_row gt_right">14.0%</td></tr>
    <tr><td headers="hefi2019subgrp_f" class="gt_row gt_center">7-Water and other healthy beverages</td>
<td headers="freq" class="gt_row gt_right">88,721</td>
<td headers="percent" class="gt_row gt_right">12.6%</td></tr>
    <tr><td headers="hefi2019subgrp_f" class="gt_row gt_center">8-Milk, unsweetened</td>
<td headers="freq" class="gt_row gt_right">35,439</td>
<td headers="percent" class="gt_row gt_right">5.0%</td></tr>
    <tr><td headers="hefi2019subgrp_f" class="gt_row gt_center">9-Plant-based beverages with proteins, unsweetened</td>
<td headers="freq" class="gt_row gt_right">1</td>
<td headers="percent" class="gt_row gt_right">0.0%</td></tr>
    <tr><td headers="hefi2019subgrp_f" class="gt_row gt_center">10-Other beverages</td>
<td headers="freq" class="gt_row gt_right">35,904</td>
<td headers="percent" class="gt_row gt_right">5.1%</td></tr>
    <tr><td headers="hefi2019subgrp_f" class="gt_row gt_center">99-Not considered (e.g., herbs, spices, fats, oils)</td>
<td headers="freq" class="gt_row gt_right">113,218</td>
<td headers="percent" class="gt_row gt_right">16.0%</td></tr>
  </tbody>
  
  
</table>
</div>

``` r
# Save / clean temporary
save(cchs24hr_detailed,
     file = file.path(data_dir,"cchs24hr_detailed.rdata"))

rm(list=c("hefifoodcat","freesugars","fidfrl_cfg"))

# *********************************************************************** #
#                                                                         #
#           Calculate intake per respondent, recall, category             #
#                                                                         #
# *********************************************************************** #

# Load data upon second run
# load(file.path(data_dir,"cchs24hr_detailed.rdata"))

# 1) Calculate total food intake (RA) per respondent, recall and category

food_sum_t <-
  cchs24hr_detailed |>
  # 1.1) remove non-food categories
  filter(is.na(hefi2019subgrp)==FALSE) |>
  filter(!(hefi2019subgrp %in% c(7,8,9,10,99)) ) |>
  # 1.2) group data
  group_by(ADM_RNO, SUPPID, hefi2019subgrp) |>
  # 1.3) sum RA per participant, recall and hefi categories
  summarise(sum_ra_qty=sum(ra_qty)) |>
  # 1.4) transpose summed intakes (long->wide)
  pivot_wider(
    values_from = sum_ra_qty,
    names_from = hefi2019subgrp,
    names_prefix = "hefi_"
  ) |>
  # 1.5) format the wide (transposed) output
  mutate(
    across(starts_with("hefi_"),function(x) ifelse(is.na(x),0,x))
    # note: missing data indicates that a given food category was not reported on a given recall
  ) |>
  rename(
    vf = hefi_1,
    wg = hefi_2,
    rg = hefi_3,
    pfab = hefi_4,
    pfpb = hefi_5,
    otherfoods = hefi_6
  )
```

    ## filter: no rows removed
    ## filter: removed 273,283 rows (39%), 432,432 rows remaining
    ## group_by: 3 grouping variables (ADM_RNO, SUPPID, hefi2019subgrp)
    ## summarise: now 129,779 rows and 4 columns, 2 group variables remaining (ADM_RNO, SUPPID)
    ## pivot_wider: reorganized (hefi2019subgrp, sum_ra_qty) into (hefi_2, hefi_3, hefi_4, hefi_5, hefi_6, …) [was 129779x4, now 28056x8]
    ## rename: renamed 6 variables (wg, rg, pfab, pfpb, otherfoods, …)

``` r
  # 1.6)  Confirm that final output for foods is consistent with expectations

  head(food_sum_t)
```

    ## # A tibble: 6 × 8
    ## # Groups:   ADM_RNO, SUPPID [6]
    ##   ADM_RNO SUPPID    wg    rg  pfab  pfpb otherfoods    vf
    ##     <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>      <dbl> <dbl>
    ## 1       1      1 1.05   0.9   1.20 5.84       45.1  0    
    ## 2       2      1 0      1.24  4.45 0          12.3  2.21 
    ## 3       3      1 1.19   0     3.27 0           2.40 0.494
    ## 4       3      2 1.60   0     0    0.335       1.85 3.38 
    ## 5       4      1 0.477  1.45  1.08 0           3.27 2.11 
    ## 6       5      1 0      0.96  1.74 6.45       74.1  6.96

``` r
  table(food_sum_t$SUPPID)
```

    ## 
    ##     1     2 
    ## 20461  7595

``` r
  gtsummary::tbl_summary(
    food_sum_t[,-1],
    by = "SUPPID",
    statistic = list(c("vf","wg", "rg", "pfab","pfpb","otherfoods") ~ "{mean} [{min} to {max}]"),
  ) |>
    modify_caption("**Mean HEFI-2019 food categories and range (CCHS 2015 - Nutrition), by 24-h dietary recall**")
```

<div id="hqrfhdquok" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#hqrfhdquok .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#hqrfhdquok .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hqrfhdquok .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#hqrfhdquok .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#hqrfhdquok .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#hqrfhdquok .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hqrfhdquok .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hqrfhdquok .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#hqrfhdquok .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#hqrfhdquok .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#hqrfhdquok .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#hqrfhdquok .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#hqrfhdquok .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#hqrfhdquok .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#hqrfhdquok .gt_from_md > :first-child {
  margin-top: 0;
}

#hqrfhdquok .gt_from_md > :last-child {
  margin-bottom: 0;
}

#hqrfhdquok .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#hqrfhdquok .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#hqrfhdquok .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#hqrfhdquok .gt_row_group_first td {
  border-top-width: 2px;
}

#hqrfhdquok .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hqrfhdquok .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#hqrfhdquok .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#hqrfhdquok .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hqrfhdquok .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hqrfhdquok .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#hqrfhdquok .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#hqrfhdquok .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hqrfhdquok .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hqrfhdquok .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#hqrfhdquok .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hqrfhdquok .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#hqrfhdquok .gt_left {
  text-align: left;
}

#hqrfhdquok .gt_center {
  text-align: center;
}

#hqrfhdquok .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#hqrfhdquok .gt_font_normal {
  font-weight: normal;
}

#hqrfhdquok .gt_font_bold {
  font-weight: bold;
}

#hqrfhdquok .gt_font_italic {
  font-style: italic;
}

#hqrfhdquok .gt_super {
  font-size: 65%;
}

#hqrfhdquok .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#hqrfhdquok .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#hqrfhdquok .gt_indent_1 {
  text-indent: 5px;
}

#hqrfhdquok .gt_indent_2 {
  text-indent: 10px;
}

#hqrfhdquok .gt_indent_3 {
  text-indent: 15px;
}

#hqrfhdquok .gt_indent_4 {
  text-indent: 20px;
}

#hqrfhdquok .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  <caption><strong>Mean HEFI-2019 food categories and range (CCHS 2015 - Nutrition), by 24-h dietary recall</strong></caption>
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;1&lt;/strong&gt;, N = 20,461&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;"><strong>1</strong>, N = 20,461<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;2&lt;/strong&gt;, N = 7,595&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;"><strong>2</strong>, N = 7,595<sup class="gt_footnote_marks">1</sup></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">wg</td>
<td headers="stat_1" class="gt_row gt_center">0.83 [0.00 to 27.29]</td>
<td headers="stat_2" class="gt_row gt_center">0.79 [0.00 to 16.37]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">rg</td>
<td headers="stat_1" class="gt_row gt_center">2.08 [0.00 to 28.00]</td>
<td headers="stat_2" class="gt_row gt_center">1.97 [0.00 to 24.42]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">pfab</td>
<td headers="stat_1" class="gt_row gt_center">2.27 [0.00 to 27.39]</td>
<td headers="stat_2" class="gt_row gt_center">2.11 [0.00 to 26.75]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">pfpb</td>
<td headers="stat_1" class="gt_row gt_center">0.48 [0.00 to 38.49]</td>
<td headers="stat_2" class="gt_row gt_center">0.39 [0.00 to 17.80]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">otherfoods</td>
<td headers="stat_1" class="gt_row gt_center">4.3 [0.0 to 204.2]</td>
<td headers="stat_2" class="gt_row gt_center">3.8 [0.0 to 111.5]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">vf</td>
<td headers="stat_1" class="gt_row gt_center">3.09 [0.00 to 33.06]</td>
<td headers="stat_2" class="gt_row gt_center">2.89 [0.00 to 24.09]</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="3"><sup class="gt_footnote_marks">1</sup> Mean [Range]</td>
    </tr>
  </tfoot>
</table>
</div>

``` r
# 2) Calculate total beverage intake (grams) per respondent, recall and category

  bev_sum_t <-
    cchs24hr_detailed |>
    # 2.1) remove non-food categories
    filter(is.na(hefi2019subgrp)==FALSE) |>
    filter(hefi2019subgrp %in% c(7,8,9,10) ) |>
    # 2.2) group data
    group_by(ADM_RNO, SUPPID, hefi2019subgrp) |>
    # 2.3) sum FID_WTG per participant, recall and hefi categories
    summarise(sum_grams=sum(FID_WTG)) |>
    # 2.4) transpose summed intakes (long->wide)
    pivot_wider(
      values_from = sum_grams,
      names_from = hefi2019subgrp,
      names_prefix = "hefi_"
    ) |>
    # 2.5) format the wide (transposed) output
    mutate(
      across(starts_with("hefi_"),function(x) ifelse(is.na(x),0,x))
      # note: missing data indicates that a given food category was not reported on a given recall
    ) |>
    rename(
      water = hefi_7,
      milk = hefi_8,
      plantbev = hefi_9,
      otherbevs = hefi_10
    )
```

    ## filter: no rows removed
    ## filter: removed 545,650 rows (77%), 160,065 rows remaining
    ## group_by: 3 grouping variables (ADM_RNO, SUPPID, hefi2019subgrp)
    ## summarise: now 65,053 rows and 4 columns, 2 group variables remaining (ADM_RNO, SUPPID)
    ## pivot_wider: reorganized (hefi2019subgrp, sum_grams) into (hefi_7, hefi_10, hefi_8, hefi_9) [was 65053x4, now 28062x6]
    ## rename: renamed 4 variables (water, otherbevs, milk, plantbev)

``` r
  # 2.6)  Confirm that final output for foods is consistent with expectations

  head(bev_sum_t)
```

    ## # A tibble: 6 × 6
    ## # Groups:   ADM_RNO, SUPPID [6]
    ##   ADM_RNO SUPPID water otherbevs  milk plantbev
    ##     <dbl>  <dbl> <dbl>     <dbl> <dbl>    <dbl>
    ## 1       1      1 1917.        0    0          0
    ## 2       2      1    0      2125.   0          0
    ## 3       3      1  813.        0  122.         0
    ## 4       3      2 2384.        0  122.         0
    ## 5       4      1 1873.      685.  11.4        0
    ## 6       5      1 2226.      391.  61.0        0

``` r
  table(bev_sum_t$SUPPID)
```

    ## 
    ##     1     2 
    ## 20461  7601

``` r
  gtsummary::tbl_summary(
    bev_sum_t[,-1],
    by = "SUPPID",
    statistic = list(c("water","milk", "otherbevs") ~ "{mean} [{min} to {max}]")
  ) |>
    modify_caption("**Mean HEFI-2019 beverage categories and range (CCHS 2015 - Nutrition), by 24-h dietary recall**")
```

<div id="ehtuzzqchi" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#ehtuzzqchi .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#ehtuzzqchi .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ehtuzzqchi .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#ehtuzzqchi .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ehtuzzqchi .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ehtuzzqchi .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ehtuzzqchi .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ehtuzzqchi .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#ehtuzzqchi .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#ehtuzzqchi .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ehtuzzqchi .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ehtuzzqchi .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#ehtuzzqchi .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#ehtuzzqchi .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#ehtuzzqchi .gt_from_md > :first-child {
  margin-top: 0;
}

#ehtuzzqchi .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ehtuzzqchi .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#ehtuzzqchi .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#ehtuzzqchi .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#ehtuzzqchi .gt_row_group_first td {
  border-top-width: 2px;
}

#ehtuzzqchi .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ehtuzzqchi .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#ehtuzzqchi .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#ehtuzzqchi .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ehtuzzqchi .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ehtuzzqchi .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ehtuzzqchi .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ehtuzzqchi .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ehtuzzqchi .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ehtuzzqchi .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ehtuzzqchi .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ehtuzzqchi .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ehtuzzqchi .gt_left {
  text-align: left;
}

#ehtuzzqchi .gt_center {
  text-align: center;
}

#ehtuzzqchi .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ehtuzzqchi .gt_font_normal {
  font-weight: normal;
}

#ehtuzzqchi .gt_font_bold {
  font-weight: bold;
}

#ehtuzzqchi .gt_font_italic {
  font-style: italic;
}

#ehtuzzqchi .gt_super {
  font-size: 65%;
}

#ehtuzzqchi .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#ehtuzzqchi .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#ehtuzzqchi .gt_indent_1 {
  text-indent: 5px;
}

#ehtuzzqchi .gt_indent_2 {
  text-indent: 10px;
}

#ehtuzzqchi .gt_indent_3 {
  text-indent: 15px;
}

#ehtuzzqchi .gt_indent_4 {
  text-indent: 20px;
}

#ehtuzzqchi .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  <caption><strong>Mean HEFI-2019 beverage categories and range (CCHS 2015 - Nutrition), by 24-h dietary recall</strong></caption>
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;1&lt;/strong&gt;, N = 20,461&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;"><strong>1</strong>, N = 20,461<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;2&lt;/strong&gt;, N = 7,601&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;"><strong>2</strong>, N = 7,601<sup class="gt_footnote_marks">1</sup></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">water</td>
<td headers="stat_1" class="gt_row gt_center">1,157 [0 to 9,570]</td>
<td headers="stat_2" class="gt_row gt_center">985 [0 to 9,300]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">otherbevs</td>
<td headers="stat_1" class="gt_row gt_center">406 [0 to 9,672]</td>
<td headers="stat_2" class="gt_row gt_center">340 [0 to 8,054]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">milk</td>
<td headers="stat_1" class="gt_row gt_center">177 [0 to 3,682]</td>
<td headers="stat_2" class="gt_row gt_center">184 [0 to 2,413]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">plantbev</td>
<td headers="stat_1" class="gt_row gt_center"></td>
<td headers="stat_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    0</td>
<td headers="stat_1" class="gt_row gt_center">20,460 (100%)</td>
<td headers="stat_2" class="gt_row gt_center">7,601 (100%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    63.595881</td>
<td headers="stat_1" class="gt_row gt_center">1 (&lt;0.1%)</td>
<td headers="stat_2" class="gt_row gt_center">0 (0%)</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="3"><sup class="gt_footnote_marks">1</sup> Mean [Range]; n (%)</td>
    </tr>
  </tfoot>
</table>
</div>

``` r
# 3) Calculate total free sugars (grams) per respondent, recall and category

  freesug_sum <-
    cchs24hr_detailed |>
    # 3.1) group data
    group_by(ADM_RNO, SUPPID) |>
    # 3.2) sum FID_WTG per participant, recall and hefi categories
    summarise(freesugars=sum(freesugars)) |>
    # 3.3) format output
    mutate(
      freesugars = ifelse(is.na(freesugars),0,freesugars)
      # note: NAs are from foods without free sugars per g values, setting to 0 (some under estimation)
    )
```

    ## group_by: 2 grouping variables (ADM_RNO, SUPPID)
    ## summarise: now 28,091 rows and 3 columns, one group variable remaining (ADM_RNO)

``` r
  # 3.4) Confirm that final output for foods is consistent with expectations

  head(freesug_sum)
```

    ## # A tibble: 6 × 3
    ## # Groups:   ADM_RNO [5]
    ##   ADM_RNO SUPPID freesugars
    ##     <dbl>  <dbl>      <dbl>
    ## 1       1      1      109. 
    ## 2       2      1       70.9
    ## 3       3      1       11.3
    ## 4       3      2       12.1
    ## 5       4      1       30.2
    ## 6       5      1      356.

``` r
  table(freesug_sum$SUPPID)
```

    ## 
    ##     1     2 
    ## 20483  7608

``` r
  gtsummary::tbl_summary(
    freesug_sum[,-1],
    by = "SUPPID",
    statistic = "freesugars" ~ "{mean} [{min} to {max}]") |>
    modify_caption("**Mean free sugars intake and range (CCHS 2015 - Nutrition), by 24-h dietary recall**")
```

<div id="cppxixqoye" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#cppxixqoye .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#cppxixqoye .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#cppxixqoye .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#cppxixqoye .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#cppxixqoye .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#cppxixqoye .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#cppxixqoye .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#cppxixqoye .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#cppxixqoye .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#cppxixqoye .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#cppxixqoye .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#cppxixqoye .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#cppxixqoye .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#cppxixqoye .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#cppxixqoye .gt_from_md > :first-child {
  margin-top: 0;
}

#cppxixqoye .gt_from_md > :last-child {
  margin-bottom: 0;
}

#cppxixqoye .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#cppxixqoye .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#cppxixqoye .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#cppxixqoye .gt_row_group_first td {
  border-top-width: 2px;
}

#cppxixqoye .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#cppxixqoye .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#cppxixqoye .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#cppxixqoye .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#cppxixqoye .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#cppxixqoye .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#cppxixqoye .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#cppxixqoye .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#cppxixqoye .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#cppxixqoye .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#cppxixqoye .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#cppxixqoye .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#cppxixqoye .gt_left {
  text-align: left;
}

#cppxixqoye .gt_center {
  text-align: center;
}

#cppxixqoye .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#cppxixqoye .gt_font_normal {
  font-weight: normal;
}

#cppxixqoye .gt_font_bold {
  font-weight: bold;
}

#cppxixqoye .gt_font_italic {
  font-style: italic;
}

#cppxixqoye .gt_super {
  font-size: 65%;
}

#cppxixqoye .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#cppxixqoye .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#cppxixqoye .gt_indent_1 {
  text-indent: 5px;
}

#cppxixqoye .gt_indent_2 {
  text-indent: 10px;
}

#cppxixqoye .gt_indent_3 {
  text-indent: 15px;
}

#cppxixqoye .gt_indent_4 {
  text-indent: 20px;
}

#cppxixqoye .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  <caption><strong>Mean free sugars intake and range (CCHS 2015 - Nutrition), by 24-h dietary recall</strong></caption>
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;1&lt;/strong&gt;, N = 20,483&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;"><strong>1</strong>, N = 20,483<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;2&lt;/strong&gt;, N = 7,608&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;"><strong>2</strong>, N = 7,608<sup class="gt_footnote_marks">1</sup></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">freesugars</td>
<td headers="stat_1" class="gt_row gt_center">59 [0 to 891]</td>
<td headers="stat_2" class="gt_row gt_center">53 [0 to 633]</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="3"><sup class="gt_footnote_marks">1</sup> Mean [Range]</td>
    </tr>
  </tfoot>
</table>
</div>

``` r
  rm(cchs24hr_detailed)

# *********************************************************************** #
#                                                                         #
#            Merge total nutrient, food and beverage intakes              #
#                                                                         #
# *********************************************************************** #

# note: goal is to have a single data with 1 row per participant per recall,
#  where columns are the dietary constituents of the hefi-2019

# Load data upon second run
  #load(file.path(data_dir,"hs_nci.rdata"))

# Merge all data into one by subject and recall number

  intake_per24hr <-
    full_join(food_sum_t,bev_sum_t) |>
    full_join(freesug_sum) |>
    right_join(hs_nci|>select(ADM_RNO,SUPPID,energy, sfa, mufa, pufa, sodium)) |>
    mutate(
      # any missing value for a dietary constituent is due to not consuming a given category
      across(everything(),function(x) ifelse(is.na(x),0,x)),
      # flag null reported energy intake
      nonzero_energy = ifelse(energy>0,1,0)
    ) |>
    ungroup()
```

    ## Joining, by = c("ADM_RNO", "SUPPID")
    ## full_join: added 4 columns (water, otherbevs, milk, plantbev)
    ## > rows only in x 29
    ## > rows only in y 35
    ## > matched rows 28,027
    ## > ========
    ## > rows total 28,091
    ## Joining, by = c("ADM_RNO", "SUPPID")
    ## full_join: added one column (freesugars)
    ## > rows only in x 0
    ## > rows only in y 0
    ## > matched rows 28,091
    ## > ========
    ## > rows total 28,091
    ## Joining, by = c("ADM_RNO", "SUPPID")
    ## right_join: added 5 columns (energy, sfa, mufa, pufa, sodium)
    ## > rows only in x ( 551)
    ## > rows only in y 4
    ## > matched rows 27,540
    ## > ========
    ## > rows total 27,544
    ## ungroup: no grouping variables

``` r
  #note: the 551 rows removed correspond to respondents aged
  # 1 to <2 y (CFG applies to 2 y or older)

# Add labels to dietary constituents

  var_label(intake_per24hr) <-
  list(
  energy = "Total energy, kcal/d",
  sfa    = "Total saturated fats, g/d",
  mufa   = "Total monounsaturated fats, g/d",
  pufa   = "Total polyunsaturated fats, g/d",
  sodium = "Total sodium, mg/d",
  vf   = "Vegetables and fruits, RA/d",
  wg   = "Whole grain foods, RA/d",
  rg   = "Refined grain foods, RA/d",
  pfab = "Protein foods, animal-based, RA/d (excludes bev.)",
  pfpb = "Protein foods, plant-based, RA/d (excludes bev.)",
  otherfoods = "Other foods, RA/d",
  water     = "Water and unsweetened beverages, g/d",
  milk      = "Unsweetened milk, g/d",
  plantbev  = "Unsweetened plant-based bev. with protein, g/d",
  otherbevs = "Other (sweetened) beverages, g/d" ,
  freesugars = "Total free sugars, g/d")

# Save
  save(intake_per24hr,
       file = file.path(data_dir,"intake_per24hr.rdata"))

  rm(list=c("food_sum_t","bev_sum_t","freesug_sum","hs_nci"))

# *********************************************************************** #
#                                                                         #
#                   Overview of `raw` dietary intakes                     #
#                                                                         #
# *********************************************************************** #

# Load data upon second run
  #load(file.path(data_dir,"intake_per24hr.rdata"))

# 1) Define dietary constituents of the HEFI-2019

  hefi2019_vars <- names(intake_per24hr[,3:(ncol(intake_per24hr)-1)])

# 2) Descriptive statistics

  intake_per24hr |>
    dplyr::select(-c(ADM_RNO,nonzero_energy)) |>
    dplyr::mutate(
      plantbev= as.numeric(plantbev)
    ) |>
   tbl_summary(
      by="SUPPID",
      statistic = all_of(hefi2019_vars[-10]) ~ "{mean} [{min} to {max}]"
    ) |>
    modify_caption("**Mean HEFI-2019 dietary constituents and range (CCHS 2015 - Nutrition), by 24-h dietary recall**")
```

<div id="hlgccuqkzv" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#hlgccuqkzv .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#hlgccuqkzv .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hlgccuqkzv .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#hlgccuqkzv .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#hlgccuqkzv .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#hlgccuqkzv .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hlgccuqkzv .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hlgccuqkzv .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#hlgccuqkzv .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#hlgccuqkzv .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#hlgccuqkzv .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#hlgccuqkzv .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#hlgccuqkzv .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#hlgccuqkzv .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#hlgccuqkzv .gt_from_md > :first-child {
  margin-top: 0;
}

#hlgccuqkzv .gt_from_md > :last-child {
  margin-bottom: 0;
}

#hlgccuqkzv .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#hlgccuqkzv .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#hlgccuqkzv .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#hlgccuqkzv .gt_row_group_first td {
  border-top-width: 2px;
}

#hlgccuqkzv .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hlgccuqkzv .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#hlgccuqkzv .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#hlgccuqkzv .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hlgccuqkzv .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hlgccuqkzv .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#hlgccuqkzv .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#hlgccuqkzv .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hlgccuqkzv .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hlgccuqkzv .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#hlgccuqkzv .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hlgccuqkzv .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#hlgccuqkzv .gt_left {
  text-align: left;
}

#hlgccuqkzv .gt_center {
  text-align: center;
}

#hlgccuqkzv .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#hlgccuqkzv .gt_font_normal {
  font-weight: normal;
}

#hlgccuqkzv .gt_font_bold {
  font-weight: bold;
}

#hlgccuqkzv .gt_font_italic {
  font-style: italic;
}

#hlgccuqkzv .gt_super {
  font-size: 65%;
}

#hlgccuqkzv .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#hlgccuqkzv .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#hlgccuqkzv .gt_indent_1 {
  text-indent: 5px;
}

#hlgccuqkzv .gt_indent_2 {
  text-indent: 10px;
}

#hlgccuqkzv .gt_indent_3 {
  text-indent: 15px;
}

#hlgccuqkzv .gt_indent_4 {
  text-indent: 20px;
}

#hlgccuqkzv .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  <caption><strong>Mean HEFI-2019 dietary constituents and range (CCHS 2015 - Nutrition), by 24-h dietary recall</strong></caption>
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;1&lt;/strong&gt;, N = 20,115&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;"><strong>1</strong>, N = 20,115<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;2&lt;/strong&gt;, N = 7,429&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;"><strong>2</strong>, N = 7,429<sup class="gt_footnote_marks">1</sup></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Whole grain foods, RA/d</td>
<td headers="stat_1" class="gt_row gt_center">0.83 [0.00 to 27.29]</td>
<td headers="stat_2" class="gt_row gt_center">0.79 [0.00 to 16.37]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Refined grain foods, RA/d</td>
<td headers="stat_1" class="gt_row gt_center">2.10 [0.00 to 28.00]</td>
<td headers="stat_2" class="gt_row gt_center">2.00 [0.00 to 24.42]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Protein foods, animal-based, RA/d (excludes bev.)</td>
<td headers="stat_1" class="gt_row gt_center">2.28 [0.00 to 27.39]</td>
<td headers="stat_2" class="gt_row gt_center">2.12 [0.00 to 26.75]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Protein foods, plant-based, RA/d (excludes bev.)</td>
<td headers="stat_1" class="gt_row gt_center">0.48 [0.00 to 38.49]</td>
<td headers="stat_2" class="gt_row gt_center">0.39 [0.00 to 17.80]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Other foods, RA/d</td>
<td headers="stat_1" class="gt_row gt_center">4.3 [0.0 to 204.2]</td>
<td headers="stat_2" class="gt_row gt_center">3.9 [0.0 to 111.5]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Vegetables and fruits, RA/d</td>
<td headers="stat_1" class="gt_row gt_center">3.11 [0.00 to 33.06]</td>
<td headers="stat_2" class="gt_row gt_center">2.90 [0.00 to 24.09]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Water and unsweetened beverages, g/d</td>
<td headers="stat_1" class="gt_row gt_center">1,171 [0 to 9,570]</td>
<td headers="stat_2" class="gt_row gt_center">1,001 [0 to 9,300]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Other (sweetened) beverages, g/d</td>
<td headers="stat_1" class="gt_row gt_center">410 [0 to 9,672]</td>
<td headers="stat_2" class="gt_row gt_center">346 [0 to 8,054]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Unsweetened milk, g/d</td>
<td headers="stat_1" class="gt_row gt_center">173 [0 to 3,682]</td>
<td headers="stat_2" class="gt_row gt_center">178 [0 to 2,413]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">plantbev</td>
<td headers="stat_1" class="gt_row gt_center"></td>
<td headers="stat_2" class="gt_row gt_center"></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    0</td>
<td headers="stat_1" class="gt_row gt_center">20,114 (100%)</td>
<td headers="stat_2" class="gt_row gt_center">7,429 (100%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    63.595881</td>
<td headers="stat_1" class="gt_row gt_center">1 (&lt;0.1%)</td>
<td headers="stat_2" class="gt_row gt_center">0 (0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Total free sugars, g/d</td>
<td headers="stat_1" class="gt_row gt_center">59 [0 to 891]</td>
<td headers="stat_2" class="gt_row gt_center">54 [0 to 633]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Total energy, kcal/d</td>
<td headers="stat_1" class="gt_row gt_center">1,850 [0 to 13,651]</td>
<td headers="stat_2" class="gt_row gt_center">1,726 [0 to 6,978]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Total saturated fats, g/d</td>
<td headers="stat_1" class="gt_row gt_center">23 [0 to 241]</td>
<td headers="stat_2" class="gt_row gt_center">22 [0 to 176]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Total monounsaturated fats, g/d</td>
<td headers="stat_1" class="gt_row gt_center">25 [0 to 260]</td>
<td headers="stat_2" class="gt_row gt_center">23 [0 to 173]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Total polyunsaturated fats, g/d</td>
<td headers="stat_1" class="gt_row gt_center">14 [0 to 170]</td>
<td headers="stat_2" class="gt_row gt_center">13 [0 to 130]</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Total sodium, mg/d</td>
<td headers="stat_1" class="gt_row gt_center">2,667 [0 to 20,121]</td>
<td headers="stat_2" class="gt_row gt_center">2,511 [1 to 14,106]</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="3"><sup class="gt_footnote_marks">1</sup> Mean [Range]; n (%)</td>
    </tr>
  </tfoot>
</table>
</div>

``` r
  intake_per24hr_nonzero <-
    intake_per24hr |>
    dplyr::select(-c(ADM_RNO,nonzero_energy)) |>
    dplyr::mutate(across(all_of(hefi2019_vars),function(x) ifelse(x<0.001,1,0)))

  var_label(intake_per24hr_nonzero) <-
    list(
      energy = "Total energy, kcal/d",
      sfa    = "Total saturated fats, g/d",
      mufa   = "Total monounsaturated fats, g/d",
      pufa   = "Total polyunsaturated fats, g/d",
      sodium = "Total sodium, mg/d",
      vf   = "Vegetables and fruits, RA/d",
      wg   = "Whole grain foods, RA/d",
      rg   = "Refined grain foods, RA/d",
      pfab = "Protein foods, animal-based, RA/d (excludes bev.)",
      pfpb = "Protein foods, plant-based, RA/d (excludes bev.)",
      otherfoods = "Other foods, RA/d",
      water     = "Water and unsweetened beverages, g/d",
      milk      = "Unsweetened milk, g/d",
      plantbev  = "Unsweetened plant-based bev. with protein, g/d",
      otherbevs = "Other (sweetened) beverages, g/d" ,
      freesugars = "Total free sugars, g/d")

  intake_per24hr_nonzero |>
    gtsummary::tbl_summary(
      by="SUPPID"
    ) |>
    modify_caption("**Proportion of zero for HEFI-2019 dietary constituents (CCHS 2015 - Nutrition), by 24-h dietary recall**")
```

<div id="yugtmzuaqm" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#yugtmzuaqm .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#yugtmzuaqm .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#yugtmzuaqm .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#yugtmzuaqm .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#yugtmzuaqm .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#yugtmzuaqm .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#yugtmzuaqm .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#yugtmzuaqm .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#yugtmzuaqm .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#yugtmzuaqm .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#yugtmzuaqm .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#yugtmzuaqm .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#yugtmzuaqm .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#yugtmzuaqm .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#yugtmzuaqm .gt_from_md > :first-child {
  margin-top: 0;
}

#yugtmzuaqm .gt_from_md > :last-child {
  margin-bottom: 0;
}

#yugtmzuaqm .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#yugtmzuaqm .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#yugtmzuaqm .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#yugtmzuaqm .gt_row_group_first td {
  border-top-width: 2px;
}

#yugtmzuaqm .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#yugtmzuaqm .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#yugtmzuaqm .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#yugtmzuaqm .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#yugtmzuaqm .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#yugtmzuaqm .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#yugtmzuaqm .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#yugtmzuaqm .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#yugtmzuaqm .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#yugtmzuaqm .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#yugtmzuaqm .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#yugtmzuaqm .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#yugtmzuaqm .gt_left {
  text-align: left;
}

#yugtmzuaqm .gt_center {
  text-align: center;
}

#yugtmzuaqm .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#yugtmzuaqm .gt_font_normal {
  font-weight: normal;
}

#yugtmzuaqm .gt_font_bold {
  font-weight: bold;
}

#yugtmzuaqm .gt_font_italic {
  font-style: italic;
}

#yugtmzuaqm .gt_super {
  font-size: 65%;
}

#yugtmzuaqm .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#yugtmzuaqm .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#yugtmzuaqm .gt_indent_1 {
  text-indent: 5px;
}

#yugtmzuaqm .gt_indent_2 {
  text-indent: 10px;
}

#yugtmzuaqm .gt_indent_3 {
  text-indent: 15px;
}

#yugtmzuaqm .gt_indent_4 {
  text-indent: 20px;
}

#yugtmzuaqm .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  <caption><strong>Proportion of zero for HEFI-2019 dietary constituents (CCHS 2015 - Nutrition), by 24-h dietary recall</strong></caption>
  
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;1&lt;/strong&gt;, N = 20,115&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;"><strong>1</strong>, N = 20,115<sup class="gt_footnote_marks">1</sup></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;2&lt;/strong&gt;, N = 7,429&lt;sup class=&quot;gt_footnote_marks&quot;&gt;1&lt;/sup&gt;"><strong>2</strong>, N = 7,429<sup class="gt_footnote_marks">1</sup></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Whole grain foods, RA/d</td>
<td headers="stat_1" class="gt_row gt_center">8,765 (44%)</td>
<td headers="stat_2" class="gt_row gt_center">3,307 (45%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Refined grain foods, RA/d</td>
<td headers="stat_1" class="gt_row gt_center">1,907 (9.5%)</td>
<td headers="stat_2" class="gt_row gt_center">714 (9.6%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Protein foods, animal-based, RA/d (excludes bev.)</td>
<td headers="stat_1" class="gt_row gt_center">1,048 (5.2%)</td>
<td headers="stat_2" class="gt_row gt_center">423 (5.7%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Protein foods, plant-based, RA/d (excludes bev.)</td>
<td headers="stat_1" class="gt_row gt_center">13,486 (67%)</td>
<td headers="stat_2" class="gt_row gt_center">5,223 (70%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Other foods, RA/d</td>
<td headers="stat_1" class="gt_row gt_center">1,349 (6.7%)</td>
<td headers="stat_2" class="gt_row gt_center">595 (8.0%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Vegetables and fruits, RA/d</td>
<td headers="stat_1" class="gt_row gt_center">902 (4.5%)</td>
<td headers="stat_2" class="gt_row gt_center">347 (4.7%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Water and unsweetened beverages, g/d</td>
<td headers="stat_1" class="gt_row gt_center">819 (4.1%)</td>
<td headers="stat_2" class="gt_row gt_center">360 (4.8%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Other (sweetened) beverages, g/d</td>
<td headers="stat_1" class="gt_row gt_center">5,892 (29%)</td>
<td headers="stat_2" class="gt_row gt_center">2,465 (33%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Unsweetened milk, g/d</td>
<td headers="stat_1" class="gt_row gt_center">6,809 (34%)</td>
<td headers="stat_2" class="gt_row gt_center">2,440 (33%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Unsweetened plant-based bev. with protein, g/d</td>
<td headers="stat_1" class="gt_row gt_center">20,114 (100%)</td>
<td headers="stat_2" class="gt_row gt_center">7,429 (100%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Total free sugars, g/d</td>
<td headers="stat_1" class="gt_row gt_center">303 (1.5%)</td>
<td headers="stat_2" class="gt_row gt_center">129 (1.7%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Total energy, kcal/d</td>
<td headers="stat_1" class="gt_row gt_center">12 (&lt;0.1%)</td>
<td headers="stat_2" class="gt_row gt_center">3 (&lt;0.1%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Total saturated fats, g/d</td>
<td headers="stat_1" class="gt_row gt_center">13 (&lt;0.1%)</td>
<td headers="stat_2" class="gt_row gt_center">6 (&lt;0.1%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Total monounsaturated fats, g/d</td>
<td headers="stat_1" class="gt_row gt_center">14 (&lt;0.1%)</td>
<td headers="stat_2" class="gt_row gt_center">8 (0.1%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Total polyunsaturated fats, g/d</td>
<td headers="stat_1" class="gt_row gt_center">14 (&lt;0.1%)</td>
<td headers="stat_2" class="gt_row gt_center">8 (0.1%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Total sodium, mg/d</td>
<td headers="stat_1" class="gt_row gt_center">4 (&lt;0.1%)</td>
<td headers="stat_2" class="gt_row gt_center">0 (0%)</td></tr>
  </tbody>
  
  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="3"><sup class="gt_footnote_marks">1</sup> n (%)</td>
    </tr>
  </tfoot>
</table>
</div>

``` r
# clean temporary
  rm(intake_per24hr_nonzero)

# end of code 01
```
