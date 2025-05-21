#' ---
#' title: "Prepare data suitable to apply the HEFI-2019 scoring algorithm (CCHS 2015 - Nutrition)"
#' author: Didier Brassard
#' date: "`r Sys.Date()`"
#' output: github_document
#' ---

# *********************************************************************** #
#                                                                         #
#                      CCHS 2015 - Nutrition (PUMF)                       #
#                                                                         #
#    Prepare a data suitable to apply the HEFI-2019 scoring algorithm     #
#                                                                         #
#                        Author: Didier Brassard                          #
#                                                                         #
#                              Version 1.2                                #
#                               2025-05-21                                #
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

  ## Automatic: create shortcuts for raw CCHS 2015 - Nutrition file location
    cchs_dir <- file.path(external_drive,"CCHS_Nutrition_2015_PUMF","Data_Donnee")
    boot_dir <- file.path(external_drive,"CCHS_Nutrition_2015_PUMF","Bootstrap","Data_Donnee")

# Automatic: create shortcuts for project directory tree

  ## Common directory
  dir_example <- here::here("Example_R_cchs")
  dir_raw <- file.path(dir_example,"data", "raw")
  dir_processed <- file.path(dir_example,"data", "processed")
  dir_metadata <- file.path(dir_example, "data", "metadata")
  dir_temp <- file.path(dir_example,"data", "temp")

  ## create directory if need be
  if(dir.exists(dir_raw)==FALSE){
    dir.create(dir_raw, recursive = TRUE)
  }
  if(dir.exists(dir_processed)==FALSE){
    dir.create(dir_processed, recursive = TRUE)
  }
  if(dir.exists(dir_metadata)==FALSE){
    dir.create(dir_metadata, recursive = TRUE)
  }
  if(dir.exists(dir_temp)==FALSE){
    dir.create(dir_temp, recursive = TRUE)
  }

# Packages
library(haven)
library(dplyr)
library(tidylog)
library(gtsummary)
library(labelled)
library(summarytools)
library(tictoc)

# suppress scientific notation
options(scipen = 9999)

#### Download and prepare Health Canada data for HEFI-2019 ####
# *********************************************************************** #
#                                                                         #
#         Download and prepare Health Canada data for HEFI-2019           #
#                                                                         #
# *********************************************************************** #

#' Reference:
#' Health Canada (2024). The Healthy Eating Food Index 2019.
#' Open Government Data Portal. Available at https://open.canada.ca/data/en/dataset/29892c85-2ff5-484c-873c-f494ffba6e1b

# 1) Download/Read CSV file with data on hefi2019 classification and free sugars per food items

if(file.exists(file.path(dir_raw,"HealthCanada_HEFI2019_data.csv"))==FALSE){
  ## indicate URL to access CSV file
  opendata_hefi2019_url <- c("https://open.canada.ca/data/en/datastore/dump/b044b16f-f371-4631-87fc-0568d0a502d1?bom=True")
  ## download file
  download.file(opendata_hefi2019_url, destfile =file.path(dir_raw,"HealthCanada_HEFI2019_data.csv"))
  ## load downloaded file from dir_raw
  HealthCanada_HEFI2019_data <- read.csv(file = file.path(dir_raw, "HealthCanada_HEFI2019_data.csv"))
} else {
  ## the file exists, no need to download
  message("dir_raw/HealthCanada_HEFI2019_data.csv found. Date modified:  ", file.info(file.path(dir_raw,"HealthCanada_HEFI2019_data.csv"))$mtime)
}

# read CSV file
HealthCanada_HEFI2019_raw <- read.csv(file = file.path(dir_raw, "HealthCanada_HEFI2019_data.csv"))

# overview
dim(HealthCanada_HEFI2019_raw); names(HealthCanada_HEFI2019_raw)

# 2) prepare classification to merge with other cchs files
HealthCanada_HEFI2019_clean <-
  HealthCanada_HEFI2019_raw |>
  mutate(
    # ensure the RA_g variable is numerical
    grams_for_1ra = ifelse(RA_g == "NA",NA,as.numeric(RA_g)),
    # recode hefi2019 foot categories for easier handling
    hefi2019_category =
      dplyr::case_when(
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
        .default = 99 ),
    ## further classify one food with missing value for RA but can be considered in beverages nonetheless
    hefi2019_category = ifelse(FID_CDE == 5628,10,hefi2019_category),
    ## add labels as factor variable
    hefi2019_category_f =
      factor(hefi2019_category,
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
                        '99-Not considered (e.g., herbs, spices, fats, oils)'))
  ) |>
  # keep only essential variables
  select("FID_CDE", "FDC_DEN", "hefi2019_category", "hefi2019_category_f", "grams_for_1ra", "Free_sugars_g") |>
  # rename for merging purpose with cchs files
  rename(freesugars_per_1g = Free_sugars_g, food_description = FDC_DEN) |>
  arrange(FID_CDE)

# Overview of all classified foods
round(table(HealthCanada_HEFI2019_clean$hefi2019_category_f) / nrow(HealthCanada_HEFI2019_clean) *100,1)

# 3) Save data for further analysis

# add labels
labelled::var_label(HealthCanada_HEFI2019_clean) <-
  list(
    FID_CDE             = "Food or Recipe code",
    food_description    = "Food or recipe description",
    hefi2019_category   = "Healthy Eating Food Index (HEFI)-2019 Category",
    hefi2019_category_f = "Healthy Eating Food Index (HEFI)-2019 Category",
    grams_for_1ra       = "Amount of food for 1 reference amount, grams",
    freesugars_per_1g   = "Amount of free sugars per 1g of food, grams")

# copy for shorter names
HealthCanada_HEFI2019 <- HealthCanada_HEFI2019_clean

# save .rdata and CSV
save(HealthCanada_HEFI2019, file = file.path(dir_processed,"HealthCanada_HEFI2019.rdata"))
write.csv(HealthCanada_HEFI2019, file = file.path(dir_processed, "HealthCanada_HEFI2019.csv"))

# create data dictionnary for sharing purpose
summary_result <- summarytools::dfSummary(HealthCanada_HEFI2019)
summarytools::stview(summary_result,
                     file = file.path(dir_metadata, "HealthCanada_HEFI2019.html"),
                     report.title = "Data Frame Summary: HealthCanada_HEFI2019")

#### Load and prepare total nutrient intakes and sociodemo data [HS]  ####
# *********************************************************************** #
#                                                                         #
#    Load and prepare total nutrient intakes and sociodemo data [HS]      #
#                                                                         #
# *********************************************************************** #

# 1) Import HS_NCI data, save to data folder
CCHS2015_HS_NCI <-
  haven::read_sas(data_file = file.path(cchs_dir,"hs_nci.sas7bdat")) |>
  # keep only respondents aged 2 years and older (i.e., target population of Canada`s food guide)
  filter(DHH_AGE>=2) |>
  mutate(
    # recode smoking status (variable to use later for demonstration purpose)
      smoking = case_when(
        SMK_202 == 3 ~ 0,
        SMK_202 == 2 ~ 1,
        SMK_202 == 1 ~ 2 ),
    # recode education
    education = ifelse(EDUG21==9,NA,EDUG21),
    # recode weekend
    r24_weekend = ifelse(ADMFW==1, 1, 0)
  ) |>
  select(
    # id
    "ADM_RNO", "WTS_P",
    # 24hr
    "SUPPID", "ADMDD", "ADM_MOI", "R24DCNT",
    # characteristics
    "GEO_PRV", "DHH_AGE", "DHH_SEX", "DHHDDRI", "education", "smoking", "r24_weekend",
    # dietary intakes
    "FSDDWTG", "FSDDEKC", "FSDDSOD", "FSDDFAS", "FSDDFAM", "FSDDFAP"
  ) |>
  rename(
    r24_day=ADMDD, r24_month=ADM_MOI, r24_nfoods=R24DCNT,
    province=GEO_PRV, sex=DHH_SEX, age=DHH_AGE, drig=DHHDDRI,
    foodwgt=FSDDWTG, energy=FSDDEKC, sodium=FSDDSOD,
    sfa=FSDDFAS, mufa=FSDDFAM, pufa=FSDDFAP
  ) |>
  arrange(ADM_RNO,SUPPID)

# overview
dim(CCHS2015_HS_NCI); names(CCHS2015_HS_NCI); table(CCHS2015_HS_NCI$SUPPID)

with(
  CCHS2015_HS_NCI[CCHS2015_HS_NCI$energy>0,],
  table(SUPPID)
)

# 2) Save data for further analysis

# add labels
labelled::var_label(CCHS2015_HS_NCI) <-
  list(
    ADM_RNO     = "Respondent id",
    WTS_P       = "Sample weight - PUMF",
    SUPPID      = "24-hour dietary recall identifier",
    r24_day     = "Reference day - 24-hour dietary recall",
    r24_month   = "Month of interview",
    r24_nfoods  = "Number of food items reported during dietary recall",
    province    = "Province of residence of respondent",
    age         = "Age, years",
    sex         = "Sex",
    drig        = "Age/sex groupings - Dietary Reference Intakes (DRIs)",
    education   = "Highest level of education (resp.) from roster",
    smoking     = "Type of smoker",
    r24_weekend = "Weekend reference day - 24-hour dietary recall",
    foodwgt     = "Amount of food - g",
    energy      = "Energy intake from food sources, kcal",
    sodium      = "Sodium intake from food sources, mg",
    sfa         = "Total saturated fatty acid intake from food, g",
    mufa        = "Total monounsaturated fatty acid intake from food, g",
    pufa        = "Total polyunsaturated fatty acid intake from food , g")


# save .rdata and CSV
save(CCHS2015_HS_NCI, file = file.path(dir_processed,"CCHS2015_HS_NCI.rdata"))
write.csv(CCHS2015_HS_NCI, file = file.path(dir_processed, "CCHS2015_HS_NCI.csv"))

# create data dictionnary for sharing purpose
summary_result <- summarytools::dfSummary(CCHS2015_HS_NCI)
summarytools::stview(summary_result,
                     file = file.path(dir_metadata, "CCHS2015_HS_NCI.html"),
                     report.title = "Data Frame Summary: CCHS2015_HS_NCI")

#### Load and prepare food or ingredient level data [FID,FRL] ####
# *********************************************************************** #
#                                                                         #
#        Load and prepare food or ingredient level data [FID,FRL]         #
#                                                                         #
# *********************************************************************** #

# 1) Import FID + FRL + CFG data, save to temp folder since these are larger datafiles

if(file.exists(file.path(dir_temp,"frl.rdata"))==FALSE){
  tictoc::tic("Read and save FID data")
  frl <- haven::read_sas(data_file = file.path(cchs_dir,"frl.sas7bdat"))
  save(frl,file = file.path(dir_temp,"frl.rdata"))
  tictoc::toc()
} else {
  ## the file exists, no need to import
  message("dir_temp/frl.rdata found. Date modified:  ", file.info(file.path(dir_temp,"frl.rdata"))$mtime)
  load(file.path(dir_temp,"frl.rdata"))
}

if(file.exists(file.path(dir_temp,"fid.rdata"))==FALSE){
  tictoc::tic("Read and save FID data")
  fid <- haven::read_sas(data_file = file.path(cchs_dir,"fid.sas7bdat"))
  save(fid,file = file.path(dir_temp,"fid.rdata"))
  tictoc::toc()
} else {
  ## the file exists, no need to import
  message("dir_temp/fid.rdata found. Date modified:  ", file.info(file.path(dir_temp,"fid.rdata"))$mtime)
  load(file.path(dir_temp,"fid.rdata"))
}

# Import CFG data to keep only `true` unique foods (i.e., recipes rolled down or up by Health Canada)
if(file.exists(file.path(dir_temp,"cfg.rdata"))==FALSE){
  tictoc::tic("Read and save cfg data")
  cfg <-
    haven::read_sas(data_file = file.path(cchs_dir,"cfg.sas7bdat")) |>
    arrange(ADM_RNO,SUPPID,SEQID)
  save(cfg,file = file.path(dir_temp,"cfg.rdata"))
  tictoc::toc()
} else {
  ## the file exists, no need to import
  message("dir_temp/cfg.rdata found. Date modified:  ", file.info(file.path(dir_temp,"cfg.rdata"))$mtime)
  load(file.path(dir_temp,"cfg.rdata"))
}

# 2) Append both data together
fidfrl <-
  rbind(fid,frl) |>
  select(-"VERDATE") |>
  arrange(ADM_RNO,SUPPID,SEQID)

  ## free memory from temporary data
  rm(fid); rm(frl)

# 3) merge FIDFRL with CFG

#' Details shown in './CCHS_Nutrition_2015_PUMF/Documentation/CFG/CFG_PSEUDO_CODE.pdf'

nutrients_list <- c("FID_WTG", "FID_EKC", "FID_CAR", "FID_FI", "FID_SUG", "FID_FAT", "FID_FAS", "FID_FAM",
                    "FID_FAP", "FID_FAL", "FID_FAN", "FID_CHO", "FID_PRO", "FID_ALC", "FID_RAE", "FID_DMG",
                    "FID_C", "FID_THI", "FID_RIB", "FID_NIA", "FID_B6", "FID_B12", "FID_FON", "FID_FOA",
                    "FID_DFE", "FID_FOL", "FID_CAL", "FID_PHO", "FID_MAG", "FID_IRO", "FID_ZIN", "FID_SOD",
                    "FID_POT", "FID_CAF", "FID_MOI", "FID_DHA", "FID_DPA", "FID_EPA", "FID_ODA")

fidfrl_cfg <-
  # right_join: keep only records in 'cfg'
  right_join(x = fidfrl, y = cfg ,
             by = c("ADM_RNO", "SUPPID", "SEQID")) |>
  mutate(
    # recode missing values for nutrients
    across(.cols = all_of(nutrients_list),
           .fns = function(x) ifelse(x>99999,NA,x))
  )

# save for further analysis
save(fidfrl_cfg, file = file.path(dir_temp,"fidfrl_cfg.rdata"))

## free memory from temporary data
rm(fidfrl, cfg)

#### Merge HEFI2019 classification + free sugars with fidfrl_cfg ####
# *********************************************************************** #
#                                                                         #
#      Merge HEFI2019 classification + free sugars with fidfrl_cfg        #
#                                                                         #
# *********************************************************************** #

# 1) Load data if need be
  if(!(exists("HealthCanada_HEFI2019"))) load(file.path(dir_processed,"HealthCanada_HEFI2019.rdata"))
  if(!(exists("fidfrl_cfg"))) load(file.path(dir_temp,"fidfrl_cfg.rdata"))

CCHS2015_FIDFRLCFG_EXPANDED <-
  fidfrl_cfg |>
  # keep only relevant variables from 'fidfrl_cfg'
  select("FID_CDE","ADM_RNO","SUPPID","FID_WTG") |>
  # merge HEFI-2019 food category and data on free sugars, but keep only FID_CDE reported in CCHS ('left_join')
  left_join(HealthCanada_HEFI2019) |>
  mutate(
    # calculate the number of reference amounts consumed
    ra_qty = ifelse(is.na(grams_for_1ra),NA,FID_WTG / grams_for_1ra),
    # calculate intake of free sugars
    freesugars = ifelse(is.na(freesugars_per_1g), NA , FID_WTG * freesugars_per_1g)
  )

# note: 'rows only in HealthCanada_HEFI2019' correspond to food items (FID_CDE) that were not used in CCHS 2015

# 2) confirm all reported intakes are classified
table(CCHS2015_FIDFRLCFG_EXPANDED$hefi2019_category_f, useNA = "always")

CCHS2015_FIDFRLCFG_EXPANDED |>
  group_by(hefi2019_category_f) |>
  summarise(freq=n()) |>
  mutate(percent=freq/sum(freq),
         freq=scales::number(freq,big.mark=","),
         percent=scales::percent(percent,accuracy=0.1)) |>
  knitr::kable(
    caption = paste0("Repartition of all foods and drinks reported in CCHS 2015 - Nutrition (total=",
                     scales::number(nrow(CCHS2015_FIDFRLCFG_EXPANDED),big.mark=','),')')
  )

# 3) Save for further analysis

# add a few missing labels
labelled::var_label(CCHS2015_FIDFRLCFG_EXPANDED) <-
  list(
    FID_CDE             = "Food or recipe code",
    ADM_RNO             = "Respondent id",
    SUPPID              = "24-hour dietary recall identifier",
    FID_WTG             = "Food amount in grams",
    ra_qty              = "Number of reference amount",
    freesugars          = "Free sugars, g")

# save
save(CCHS2015_FIDFRLCFG_EXPANDED, file = file.path(dir_processed,"CCHS2015_FIDFRLCFG_EXPANDED.rdata"))
write.csv(CCHS2015_FIDFRLCFG_EXPANDED, file = file.path(dir_processed, "CCHS2015_FIDFRLCFG_EXPANDED.csv"))

# create data dictionnary for sharing purpose
summary_result <- summarytools::dfSummary(CCHS2015_FIDFRLCFG_EXPANDED)
summarytools::stview(summary_result,
                     file = file.path(dir_metadata, "CCHS2015_FIDFRLCFG_EXPANDED.html"),
                     report.title = "Data Frame Summary: CCHS2015_FIDFRLCFG_EXPANDED")


rm(HealthCanada_HEFI2019, fidfrl_cfg)

#### Calculate intake per respondent, recall, HEFI-2019 category ####
# *********************************************************************** #
#                                                                         #
#      Calculate intake per respondent, recall, HEFI-2019 category        #
#                                                                         #
# *********************************************************************** #

# Load data if need be
if(!(exists("CCHS2015_FIDFRLCFG_EXPANDED"))) load(file.path(dir_processed,"CCHS2015_FIDFRLCFG_EXPANDED.rdata"))

# 1) Calculate total food intake (RA) per respondent, recall and category
food_sum_t <-
  CCHS2015_FIDFRLCFG_EXPANDED |>
  # 1.1) remove missing and non-food categories
  filter(is.na(hefi2019_category)==FALSE) |>
  filter(!(hefi2019_category %in% c(7,8,9,10,99)) ) |>
  # 1.2) group data
  group_by(ADM_RNO, SUPPID, hefi2019_category) |>
  # 1.3) sum RA per participant, recall and HEFI-2019 categories
  summarise(sum_ra_qty=sum(ra_qty,na.rm = TRUE)) |>
  # note: na.rm=TRUE required to obtain sum when respondents reported foods with missing RAs
  # 1.4) transpose summed intakes (long->wide)
  pivot_wider(
    values_from = sum_ra_qty,
    names_from = hefi2019_category,
    names_prefix = "hefi_"
  ) |>
  # 1.5) format the wide (transposed) output
  mutate(
    across(.cols = starts_with("hefi_"),
           .fns  = function(x) ifelse(is.na(x),0,x))
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

  # 1.6)  Confirm that final output for foods is consistent with expectations

  dim(food_sum_t); head(food_sum_t); table(food_sum_t$SUPPID)

  gtsummary::tbl_summary(
    food_sum_t[,-1],
    by = "SUPPID",
    statistic = list(c("vf","wg", "rg", "pfab","pfpb","otherfoods") ~ "{mean} [{min} to {max}]"),
  ) |>
    gtsummary::modify_caption("Mean HEFI-2019 food categories and range (CCHS 2015 - Nutrition), by 24-h dietary recall") |>
    gtsummary::as_kable()


# 2) Calculate total beverage intake (grams) per respondent, recall and category
  bev_sum_t <-
    CCHS2015_FIDFRLCFG_EXPANDED |>
    # 2.1) remove missing and non-food categories
    filter(is.na(hefi2019_category)==FALSE) |>
    filter(hefi2019_category %in% c(7,8,9,10) ) |>
    # 2.2) group data
    group_by(ADM_RNO, SUPPID, hefi2019_category) |>
    # 2.3) sum FID_WTG per participant, recall and hefi categories
    summarise(sum_grams=sum(FID_WTG,na.rm = TRUE)) |>
    # note: na.rm=TRUE required to obtain sum when respondents reported foods with missing FID_WTG
    # 2.4) transpose summed intakes (long->wide)
    pivot_wider(
      values_from = sum_grams,
      names_from = hefi2019_category,
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

  # 2.6)  Confirm that final output for foods is consistent with expectations
  dim(bev_sum_t); head(bev_sum_t); table(bev_sum_t$SUPPID)

  gtsummary::tbl_summary(
    bev_sum_t[,-1],
    by = "SUPPID",
    statistic = list(c("water","milk", "otherbevs") ~ "{mean} [{min} to {max}]")
  ) |>
    gtsummary::modify_caption("Mean HEFI-2019 beverage categories and range (CCHS 2015 - Nutrition), by 24-h dietary recall") |>
    gtsummary::as_kable()

# 3) Calculate total free sugars (grams) per respondent, recall and category
  freesug_sum_t <-
    CCHS2015_FIDFRLCFG_EXPANDED |>
    # 3.1) group data
    group_by(ADM_RNO, SUPPID) |>
    # 3.2) sum freesugars per participant, recall and hefi categories
    summarise(freesugars=sum(freesugars,na.rm=TRUE)) |>
    # note: na.rm=TRUE required to obtain sum when respondents reported foods with missing free sugars value
    # 3.3) format output
    mutate(
      freesugars = ifelse(is.na(freesugars), 0, freesugars)
      # note: NAs are from foods without free sugars per g values, setting to 0 (some under estimation)
    )

  # 3.4) Confirm that final output for foods is consistent with expectations
  dim(freesug_sum_t); head(freesug_sum_t); table(freesug_sum_t$SUPPID)

  gtsummary::tbl_summary(
    freesug_sum_t[,-1],
    by = "SUPPID",
    statistic = "freesugars" ~ "{mean} [{min} to {max}]") |>
    gtsummary::modify_caption("Mean free sugars intake and range (CCHS 2015 - Nutrition), by 24-h dietary recall") |>
    gtsummary::as_kable()

  rm(CCHS2015_FIDFRLCFG_EXPANDED)

#### Merge total nutrient, food and beverage intakes ####
# *********************************************************************** #
#                                                                         #
#            Merge total nutrient, food and beverage intakes              #
#                                                                         #
# *********************************************************************** #

#' note: goal is to have a single data with 1 row per participant per recall,
#' where columns are the dietary constituents of the HEFI-2019

# 1) Load data if need be
  if(!(exists("CCHS2015_HS_NCI"))) load(file.path(dir_processed,"CCHS2015_HS_NCI.rdata"))

# 2) Merge all data into one by respondent and recall identifier

  # 2.1) output names of calculated intake variables to apply common formatting
  calculated_intakes_var <-
    c(names(food_sum_t[-c(1:2)]),
      names(bev_sum_t[-c(1:2)]),
      names(freesug_sum_t[-c(1:2)]))

  calculated_intakes_var

  # 2.2)
  CCHS2015_HEFI2019_PER24HR <-
    full_join(food_sum_t,bev_sum_t,
              by = c("ADM_RNO", "SUPPID")) |>
    full_join(freesug_sum_t,
              by = c("ADM_RNO", "SUPPID")) |>
    right_join(CCHS2015_HS_NCI|>
                 select("ADM_RNO","SUPPID","energy", "sfa", "mufa", "pufa", "sodium"),
               by = c("ADM_RNO", "SUPPID")) |>
    mutate(
      # any missing value for a 'calculated_intakes_var' is due to not consuming a given category by the respondent
      across(.cols = all_of(calculated_intakes_var),
             .fns  = function(x) ifelse(is.na(x),0,x)),
      # flag null reported energy intake
      nonzero_energy = ifelse(energy>0,1,0)
    ) |>
    ungroup()

  #' note: the 551 rows only in 'x' (removed) correspond to respondents aged
  #' 1 to <2 y (CFG applies to 2 y or older) to whom CFG recommendations do not apply

# 3) Save for further analysis

  # Add labels to dietary constituents
  labelled::var_label(CCHS2015_HEFI2019_PER24HR) <-
    list(
      energy = "Total energy, kcal/d",
      sfa    = "Total saturated fats, g/d",
      mufa   = "Total monounsaturated fats, g/d",
      pufa   = "Total polyunsaturated fats, g/d",
      sodium = "Total sodium, mg/d",
      vf   = "Vegetables and fruits, RA/d",
      wg   = "Whole grain foods, RA/d",
      rg   = "Non-whole grain foods, RA/d",
      pfab = "Protein foods, animal-based, RA/d (excludes bev.)",
      pfpb = "Protein foods, plant-based, RA/d (excludes bev.)",
      otherfoods = "Other foods, RA/d",
      water     = "Water and unsweetened beverages, g/d",
      milk      = "Unsweetened milk, g/d",
      plantbev  = "Unsweetened plant-based bev. with protein, g/d",
      otherbevs = "Other (sweetened) beverages, g/d" ,
      freesugars = "Total free sugars, g/d",
      nonzero_energy = "Flag for 24-hour dietary recall with total energy intake > 0")

# save
save(CCHS2015_HEFI2019_PER24HR, file = file.path(dir_processed,"CCHS2015_HEFI2019_PER24HR.rdata"))
write.csv(CCHS2015_HEFI2019_PER24HR, file = file.path(dir_processed, "CCHS2015_HEFI2019_PER24HR.csv"))

# create data dictionnary for sharing purpose
summary_result <- summarytools::dfSummary(CCHS2015_HEFI2019_PER24HR)
summarytools::stview(summary_result,
                     file = file.path(dir_metadata, "CCHS2015_HEFI2019_PER24HR.html"),
                     report.title = "Data Frame Summary: CCHS2015_HEFI2019_PER24HR")

#### Overview of `raw` dietary intakes ####
# *********************************************************************** #
#                                                                         #
#                   Overview of `raw` dietary intakes                     #
#                                                                         #
# *********************************************************************** #

# Load data if need be
if(!(exists("CCHS2015_HEFI2019_PER24HR"))) load(file.path(dir_processed,"CCHS2015_HEFI2019_PER24HR.rdata"))

# 1) Define dietary constituents of the HEFI-2019

  hefi2019_vars <- names(CCHS2015_HEFI2019_PER24HR[,3:(ncol(CCHS2015_HEFI2019_PER24HR)-1)])

# 2) Descriptive statistics

  # mean, min to max
  CCHS2015_HEFI2019_PER24HR |>
    dplyr::select(-c("ADM_RNO","nonzero_energy")) |>
    dplyr::mutate(
      plantbev= as.numeric(plantbev)
    ) |>
   gtsummary::tbl_summary(
      by="SUPPID",
      statistic = all_of(hefi2019_vars[-10]) ~ "{mean} [{min} to {max}]"
    ) |>
    gtsummary::modify_caption("Mean HEFI-2019 dietary constituents and range (CCHS 2015 - Nutrition), by 24-h dietary recall")  |>
    gtsummary::as_kable()

  # proportion of zero
  intake_per24hr_nonzero <-
    CCHS2015_HEFI2019_PER24HR |>
    dplyr::select(-c("ADM_RNO","nonzero_energy")) |>
    dplyr::mutate(
      across(.cols = all_of(hefi2019_vars),
             .fns = function(x) ifelse(x<0.001,1,0)))

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
    gtsummary::modify_caption("Proportion of zero for HEFI-2019 dietary constituents (CCHS 2015 - Nutrition), by 24-h dietary recall")  |>
    gtsummary::as_kable()

#'
#### End of code 01 ####
# *********************************************************************** #
#                             End of code 01                              #
# *********************************************************************** #

sessionInfo()
