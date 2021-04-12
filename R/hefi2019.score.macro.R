#' Healthy Eating Food Index-2019 Scoring Algorithm
#'
#' This function scores dietary constituents provided in the input dataset
#' according to the Healthy Eating Food Index (HEFI)-2019 scoring algorithm
#' (Brassard et al., 2021). The original variables are
#' kept in the output data. New variables include density of intakes
#' (i.e., ratios of dietary constituents), the total HEFI-2019 and its
#' components subscores.
#'
#' @param indata Input dataset with dietary constituents
#' @param vegwfruit RAs from vegetables and (whole) fruits
#' @param wholegrfoods RAs from whole-grain foods
#' @param nonwholegrfoods RAs from non-whole grain foods
#' @param profoodsanimal RAs from animal-based protein foods
#' @param profoodsplant RAs from plant-based protein foods
#' @param otherfoods RAs from all others foods (i.e., not considered in the variables above)
#' @param mufat Grams of fat from monounsaturated fats
#' @param pufat Grams of fat from polyunsaturated fats
#' @param satfat Grams of fat from saturated fats
#' @param sugars Grams of free sugars
#' @param kcal Total energy intake, kcal
#' @param sodium Milligrams of sodium
#' @param water_and_other_healthy Grams of water and other healthy beverages (see definition in Lamarche et al. 2021)
#' @param unsweetmilk Grams of unsweetened milk (all % M.F.)
#' @param unsweetplantbevpro Grams of unsweetened plant-based beverages
#' @param otherbev Grams of all other beverages (artificially- or sugar-sweetened beverages, juices, sweetened milk or plant-based beverages. See definition in Lamarche et al. 2021)
#' @return Input dataset (\code{indata}) with additional variables including density of intakes (i.e., ratios of dietary constituents), total score and component subscores
#' @export

hefi2019 <-
  function(indata,
           vegwfruit,
           wholegrfoods,
           nonwholegrfoods,
           profoodsanimal,
           profoodsplant,
           otherfoods,
           water_and_other_healthy,
           unsweetmilk,
           unsweetplantbevpro,
           otherbev,
           mufat,
           pufat,
           satfat,
           sugars,
           kcal,
           sodium) {

    # general message
    message("Healthy Eating Food Index-2019 Scoring Algorithm R version 1.1")

    # create quosure
    vegwfruit <- dplyr::enquo(vegwfruit)
    wholegrfoods <- dplyr::enquo(wholegrfoods)
    nonwholegrfoods <- dplyr::enquo(nonwholegrfoods)
    profoodsanimal <- dplyr::enquo(profoodsanimal)
    profoodsplant <- dplyr::enquo(profoodsplant)
    otherfoods <- dplyr::enquo(otherfoods)
    mufat <- dplyr::enquo(mufat)
    pufat <- dplyr::enquo(pufat)
    satfat <- dplyr::enquo(satfat)
    sugars <- dplyr::enquo(sugars)
    kcal <- dplyr::enquo(kcal)
    sodium <- dplyr::enquo(sodium)
    water_and_other_healthy <- dplyr::enquo(water_and_other_healthy)
    unsweetmilk <- dplyr::enquo(unsweetmilk)
    unsweetplantbevpro <- dplyr::enquo(unsweetplantbevpro)
    otherbev <- dplyr::enquo(otherbev)

    # assign scores based on input data
    return(
      indata %>%
        dplyr::mutate(
          .,
          # calculate reference amounts from unsweetened milk and unsweetened plant-based beverages protein foods,
          # assuming average of 258g per RA
          unsweetmilk_RA = (!!unsweetmilk / 258),
          unsweetplantbevpro_RA = (!!unsweetplantbevpro / 258),

          # calculate total reference amounts from foods and protein beverages
          totfoodsRA = (!!vegwfruit + !!wholegrfoods + !!nonwholegrfoods + !!profoodsanimal + !!profoodsplant + !!otherfoods +
            unsweetmilk_RA + unsweetplantbevpro_RA),

          #********************************************
          #* Component 1 - Vegetables and fruit       *
          #********************************************

          # ratio
          RATIO_VF = ifelse(totfoodsRA > 0, (!!vegwfruit / totfoodsRA), NA),

          # score
          HEFI2019C1_VF = ifelse(totfoodsRA > 0, (20 * (RATIO_VF / 0.50)), 0),
          HEFI2019C1_VF = ifelse(HEFI2019C1_VF > 20, 20, HEFI2019C1_VF),

          #********************************************
          #* Component 2 - Whole grain foods          *
          #********************************************

          # ratio
          RATIO_WGTOT = ifelse(totfoodsRA > 0, (!!wholegrfoods / totfoodsRA), NA),

          # score
          HEFI2019C2_WHOLEGR = ifelse(totfoodsRA > 0, (5 * (RATIO_WGTOT / 0.25)), 0),
          HEFI2019C2_WHOLEGR = ifelse(HEFI2019C2_WHOLEGR > 5, 5, HEFI2019C2_WHOLEGR),

          #********************************************
          #* Component 3 - Grain foods ratio          *
          #********************************************

          # total
          totgrain = (!!wholegrfoods + !!nonwholegrfoods),

          # ratio
          RATIO_WGGR = ifelse(totgrain > 0, (!!wholegrfoods / totgrain), NA),

          # score
          HEFI2019C3_GRRATIO = ifelse(totgrain > 0, (5 * (RATIO_WGGR)), 0),
          HEFI2019C3_GRRATIO = ifelse(HEFI2019C3_GRRATIO > 5, 5, HEFI2019C3_GRRATIO),

          #********************************************
          #* Component 4 - Total protein foods        *
          #********************************************

          # total
          totpro = (!!profoodsanimal + !!profoodsplant + unsweetmilk_RA + unsweetplantbevpro_RA),

          # ratio
          RATIO_PRO = ifelse(totpro > 0, (totpro / totfoodsRA), NA),

          # score
          HEFI2019C4_PROFOODS = ifelse(totpro > 0, (5 * (RATIO_PRO / 0.25)), 0),
          HEFI2019C4_PROFOODS = ifelse(HEFI2019C4_PROFOODS > 5, 5, HEFI2019C4_PROFOODS),

          #********************************************
          #* Component 5 - Plant-based protein foods  *
          #********************************************

          # ratio
          RATIO_PLANT = ifelse(totpro > 0, (!!profoodsplant / totpro), NA),

          # score
          HEFI2019C5_PLANTPRO = ifelse(totpro > 0, (5 * (RATIO_PLANT / 0.50000001)), 0),
          HEFI2019C5_PLANTPRO = ifelse(HEFI2019C5_PLANTPRO > 5, 5, HEFI2019C5_PLANTPRO),

          #********************************************
          #* Component 6 - Beverages                  *
          #********************************************

          # total
          totbev = (!!water_and_other_healthy + !!unsweetmilk + !!unsweetplantbevpro + !!otherbev),

          # ratio
          RATIO_BEV = ifelse(totbev > 0, ((!!water_and_other_healthy + !!unsweetmilk + !!unsweetplantbevpro) / totbev), NA),

          # score
          HEFI2019C6_BEVERAGES = ifelse(totbev > 0, (10 * (RATIO_BEV)), 0),
          HEFI2019C6_BEVERAGES = ifelse(HEFI2019C6_BEVERAGES > 10, NA, HEFI2019C6_BEVERAGES), # > 10 indicates that more bev were consumed than total bev (impossible)

          #********************************************
          #* Component 7 - Fatty acids ratio          *
          #********************************************

          # input limits
          FATmin = 1.1,
          FATmax = 2.6,

          # sum
          unsatfat = (!!mufat + !!pufat),

          # ratio
          RATIO_UNSFAT = ifelse(!!satfat > 0, ((!!mufat + !!pufat) / !!satfat), NA),

          # score
          HEFI2019C7_FATTYACID = ifelse(!!satfat == 0,
            ifelse(unsatfat > 0, 5, 0),
            5 * ((RATIO_UNSFAT - FATmin) / (FATmax - FATmin))
          ),
          HEFI2019C7_FATTYACID = ifelse(is.na(RATIO_UNSFAT), HEFI2019C7_FATTYACID,
            ifelse((RATIO_UNSFAT >= FATmax), 5,
              ifelse((RATIO_UNSFAT <= FATmin), 0, HEFI2019C7_FATTYACID)
            )
          ),

          #********************************************
          #* Component 8 - Saturated fats             *
          #********************************************

          # input limits
          SFAmin = 10,
          SFAmax = 15,

          # ratio
          SFA_PERC = ifelse((!!kcal > 0), 100 * (!!satfat * 9 / !!kcal), NA),

          # score
          HEFI2019C8_SFAT = ifelse((!!kcal == 0), 0,
            5 - (5 * (SFA_PERC - SFAmin) / (SFAmax - SFAmin))
          ),
          HEFI2019C8_SFAT = ifelse((SFA_PERC < SFAmin), 5, HEFI2019C8_SFAT),
          HEFI2019C8_SFAT = ifelse((SFA_PERC >= SFAmax), 0, HEFI2019C8_SFAT),

          #********************************************
          #* Component 9 - (Free) sugars              *
          #********************************************

          # input limits
          SUGmin = 10,
          SUGmax = 20,

          # ratio
          SUG_PERC = ifelse((!!kcal > 0), 100 * (!!sugars * 4 / !!kcal), NA),

          # score
          HEFI2019C9_SUGARS = ifelse((!!kcal == 0), 0,
            10 - (10 * (SUG_PERC - SUGmin) / (SUGmax - SUGmin))
          ),
          HEFI2019C9_SUGARS = ifelse((SUG_PERC < SUGmin), 10, HEFI2019C9_SUGARS),
          HEFI2019C9_SUGARS = ifelse((SUG_PERC >= SUGmax), 0, HEFI2019C9_SUGARS),

          #********************************************
          #* Component 10 - Sodium                   *
          #********************************************

          # input limits
          SODmin = 0.9,
          SODmax = 2.0,

          # ratio
          SODDEN = ifelse(!!kcal > 0, (!!sodium / !!kcal), NA),

          # score
          HEFI2019C10_SODIUM = ifelse((!!kcal == 0), 0,
            10 - (10 * (SODDEN - SODmin) / (SODmax - SODmin))
          ),
          HEFI2019C10_SODIUM = ifelse((SODDEN < SODmin), 10, HEFI2019C10_SODIUM),
          HEFI2019C10_SODIUM = ifelse((SODDEN >= SODmax), 0, HEFI2019C10_SODIUM),

          #**************************************************************
          #* Total score: the sum of the HEFI2019 component subscores   *
          #**************************************************************

          HEFI2019_TOTAL_SCORE = (HEFI2019C1_VF + HEFI2019C2_WHOLEGR + HEFI2019C3_GRRATIO + HEFI2019C4_PROFOODS +
            HEFI2019C5_PLANTPRO + HEFI2019C6_BEVERAGES + HEFI2019C7_FATTYACID + HEFI2019C8_SFAT +
            HEFI2019C9_SUGARS + HEFI2019C10_SODIUM)
        ) %>% # end of mutate
        dplyr::select(., -c(
          "unsweetmilk_RA", "unsweetplantbevpro_RA", "totfoodsRA", "totgrain", "totpro",
          "unsatfat", "FATmin", "FATmax", "totbev", "SFAmin", "SFAmax", "SUGmin", "SUGmax", "SODmin", "SODmax"
        )) # end of select, temporary variables are cleared
    ) # end of return
  }
