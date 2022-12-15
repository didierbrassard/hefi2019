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
  library(tidylog)
  library(readxl)
  library(gtsummary)
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

# 2) prepare free sugars data to merge with other cchs files
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

# overview
  table(hs_nci$SUPPID)

  with(
    hs_nci[hs_nci$energy>0,],
    table(SUPPID)
  )

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

#confirm reported intakes are classified
table(is.na(cchs24hr_detailed$hefi2019subgrp_f))

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
  summarise(sum_ra_qty=sum(ra_qty,na.rm = TRUE)) |>
    # note: na.rm=TRUE required to obtain sum when respondents reported foods with missing RAs
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

  # 1.6)  Confirm that final output for foods is consistent with expectations

  head(food_sum_t)

  table(food_sum_t$SUPPID)

  gtsummary::tbl_summary(
    food_sum_t[,-1],
    by = "SUPPID",
    statistic = list(c("vf","wg", "rg", "pfab","pfpb","otherfoods") ~ "{mean} [{min} to {max}]"),
  ) |>
    modify_caption("Mean HEFI-2019 food categories and range (CCHS 2015 - Nutrition), by 24-h dietary recall") |>
    as_kable()


# 2) Calculate total beverage intake (grams) per respondent, recall and category

  bev_sum_t <-
    cchs24hr_detailed |>
    # 2.1) remove non-food categories
    filter(is.na(hefi2019subgrp)==FALSE) |>
    filter(hefi2019subgrp %in% c(7,8,9,10) ) |>
    # 2.2) group data
    group_by(ADM_RNO, SUPPID, hefi2019subgrp) |>
    # 2.3) sum FID_WTG per participant, recall and hefi categories
    summarise(sum_grams=sum(FID_WTG,na.rm = TRUE)) |>
      # note: na.rm=TRUE required to obtain sum when respondents reported foods with missing FID_WTG
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

  # 2.6)  Confirm that final output for foods is consistent with expectations

  head(bev_sum_t)

  table(bev_sum_t$SUPPID)

  gtsummary::tbl_summary(
    bev_sum_t[,-1],
    by = "SUPPID",
    statistic = list(c("water","milk", "otherbevs") ~ "{mean} [{min} to {max}]")
  ) |>
    modify_caption("Mean HEFI-2019 beverage categories and range (CCHS 2015 - Nutrition), by 24-h dietary recall") |>
    as_kable()

# 3) Calculate total free sugars (grams) per respondent, recall and category

  freesug_sum <-
    cchs24hr_detailed |>
    # 3.1) group data
    group_by(ADM_RNO, SUPPID) |>
    # 3.2) sum freesugars per participant, recall and hefi categories
    summarise(freesugars=sum(freesugars,na.rm=TRUE)) |>
      # note: na.rm=TRUE required to obtain sum when respondents reported foods with missing free sugars value
    # 3.3) format output
    mutate(
      freesugars = ifelse(is.na(freesugars),0,freesugars)
      # note: NAs are from foods without free sugars per g values, setting to 0 (some under estimation)
    )

  # 3.4) Confirm that final output for foods is consistent with expectations

  head(freesug_sum)

  table(freesug_sum$SUPPID)

  gtsummary::tbl_summary(
    freesug_sum[,-1],
    by = "SUPPID",
    statistic = "freesugars" ~ "{mean} [{min} to {max}]") |>
    modify_caption("Mean free sugars intake and range (CCHS 2015 - Nutrition), by 24-h dietary recall") |>
    as_kable()

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
    modify_caption("Mean HEFI-2019 dietary constituents and range (CCHS 2015 - Nutrition), by 24-h dietary recall")  |>
    as_kable()

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
    modify_caption("Proportion of zero for HEFI-2019 dietary constituents (CCHS 2015 - Nutrition), by 24-h dietary recall")  |>
    as_kable()

# clean temporary
  rm(intake_per24hr_nonzero)

# Session info
  sessionInfo()

# end of code 01
