#' Healthy Eating Food Index-2019 Scoring Algorithm
#'
#' This function scores dietary constituents provided in the input data set
#' according to the Healthy Eating Food Index (HEFI)-2019 scoring algorithm
#' See Brassard et al., Appl Physiol Nutr Metab 2022 for details and
#' information on classification of foods.
#' The original variables are kept in the output data. The new variables include
#' density of intakes (i.e., ratios of dietary constituents), the total
#' HEFI-2019 score and component scores.
#' Of note, when no foods, beverages or energy are reported, ratios are
#' not calculated and a score of 0 is assigned to the corresponding
#' components. Missing data for any dietary constituents will results in
#' missing components score(s) and total score.
#'
#' Suggested layout for the input data set:
#' the function should ideally be applied to a data set where rows correspond
#' to individuals and dietary constituents are columns.
#'
#'  Caution:  variable names "unsweetmilk_RA", "unsweetplantbevpro_RA",
#'  "totfoodsRA", "totgrain", "totpro", "totbev", "unsatfat",
#'  "RATIO_VF", "RATIO_WGTOT", "RATIO_WGGR", "RATIO_PRO",
#'  "RATIO_PLANT", "RATIO_FA", "RATIO_BEV", "SFA_PERC", "SUG_PERC",
#'  "SODDEN", "FATmin", "FATmax", "SFAmin", "SFAmax", "SUGmin",
#'  "SUGmax", "SODmin", "SODmax" are reserved for this function.
#'
#' @param indata Input data set with dietary constituents
#' @param vegfruits Reference amounts (RA) from vegetables and fruits (excludes fruit juices)
#' @param wholegrfoods RA from whole-grain foods (includes whole wheat)
#' @param nonwholegrfoods RA from non-whole grain foods
#' @param profoodsanimal RA from animal-based protein foods (excludes milk)
#' @param profoodsplant RA from plant-based protein foods (excludes plant-based beverages)
#' @param otherfoods RA from all other foods not considered in the categories above (vegetables and fruits, grain foods, protein foods), excludes beverages
#' @param waterhealthybev Grams of water and other healthy beverages not in unsweetened milk or unsweetened plant-based beverages with sufficient protein
#' @param unsweetmilk Grams of unsweetened milk (includes all % M.F.)
#' @param unsweetplantbevpro Grams of unsweetened plant-based beverages with sufficient protein
#' @param otherbeverages Grams of all other beverages (artificially- or sugar-sweetened beverages, juices, sweetened milk or plant-based beverages, etc.)
#' @param mufat Grams of fat from monounsaturated fats
#' @param pufat Grams of fat from polyunsaturated fats
#' @param satfat Grams of fat from saturated fats
#' @param freesugars Grams of free sugars
#' @param sodium Milligrams of sodium
#' @param energy Total energy intake, kcal
#'
#' @references Brassard et al. Appl Physiol Nutr Metab. 2022. Development of the Healthy Eating Food Index (HEFI)-2019 measuring adherence to Canada's Food Guide 2019 recommendations on healthy food choices
#'
#' @import dplyr
#'
#' @return Input data set (\code{indata}) with additional variables including density of intakes (i.e., ratios of dietary constituents), total and component scores
#'
#' @export
#'

hefi2019 <-
  function(indata,
           vegfruits,
           wholegrfoods,
           nonwholegrfoods,
           profoodsanimal,
           profoodsplant,
           otherfoods,
           waterhealthybev,
           unsweetmilk,
           unsweetplantbevpro,
           otherbeverages,
           mufat,
           pufat,
           satfat,
           freesugars,
           sodium,
           energy) {

    # general message
    message("Healthy Eating Food Index-2019 Scoring Algorithm R version 1.4")

    # assign scores based on input data
    outdata <-
      indata |>
        dplyr::mutate(
          # Indicate gram per RA for unsweetened milk and unsweetened plant-based beverages with sufficient protein
          probev_gram_per_RA = 258 ,

          # calculate reference amounts from unsweetened milk and unsweetened plant-based beverages protein foods,
          unsweetmilk_RA = ({{unsweetmilk}} / probev_gram_per_RA),
          unsweetplantbevpro_RA = ({{unsweetplantbevpro}} / probev_gram_per_RA),

          # sum total reference amounts from foods and protein beverages
          totfoodsRA = ({{vegfruits}} + {{wholegrfoods}} + {{nonwholegrfoods}} + {{profoodsanimal}} + {{profoodsplant}} + {{otherfoods}} +
            unsweetmilk_RA + unsweetplantbevpro_RA),

          # Component 1 - Vegetables and fruits

          # ratio
          RATIO_VF = ifelse(totfoodsRA > 0, ({{vegfruits}} / totfoodsRA), NA),

          # score
          HEFI2019C1_VF = ifelse(totfoodsRA > 0, (20 * (RATIO_VF / 0.50)), 0),
          HEFI2019C1_VF = ifelse(HEFI2019C1_VF > 20, 20, HEFI2019C1_VF),

          # Component 2 - Whole-grain foods

          # ratio
          RATIO_WGTOT = ifelse(totfoodsRA > 0, ({{wholegrfoods}} / totfoodsRA), NA),

          # score
          HEFI2019C2_WHOLEGR = ifelse(totfoodsRA > 0, (5 * (RATIO_WGTOT / 0.25)), 0),
          HEFI2019C2_WHOLEGR = ifelse(HEFI2019C2_WHOLEGR > 5, 5, HEFI2019C2_WHOLEGR),

          # Component 3 - Grain foods ratio

          # total
          totgrain = ({{wholegrfoods}} + {{nonwholegrfoods}}),

          # ratio
          RATIO_WGGR = ifelse(totgrain > 0, ({{wholegrfoods}} / totgrain), NA),

          # score
          HEFI2019C3_GRRATIO = ifelse(totgrain > 0, (5 * (RATIO_WGGR)), 0),
          HEFI2019C3_GRRATIO = ifelse(HEFI2019C3_GRRATIO > 5, 5, HEFI2019C3_GRRATIO),

          # Component 4 - Protein foods

          # total
          totpro = ({{profoodsanimal}} + {{profoodsplant}} + unsweetmilk_RA + unsweetplantbevpro_RA),

          # ratio
          RATIO_PRO = ifelse(totfoodsRA > 0, (totpro / totfoodsRA), NA),

          # score
          HEFI2019C4_PROFOODS = ifelse(totfoodsRA > 0, (5 * (RATIO_PRO / 0.25)), 0),
          HEFI2019C4_PROFOODS = ifelse(HEFI2019C4_PROFOODS > 5, 5, HEFI2019C4_PROFOODS),

          # Component 5 - Plant-based protein foods

          # ratio
          RATIO_PLANT = ifelse(totpro > 0, (({{profoodsplant}}+unsweetplantbevpro_RA) / totpro), NA),

          # score
          HEFI2019C5_PLANTPRO = ifelse(totpro > 0, (5 * (RATIO_PLANT / 0.50000001)), 0),
          HEFI2019C5_PLANTPRO = ifelse(HEFI2019C5_PLANTPRO > 5, 5, HEFI2019C5_PLANTPRO),

          # Component 6 - Beverages

          # total
          totbev = ({{waterhealthybev}} + {{unsweetmilk}} + {{unsweetplantbevpro}} + {{otherbeverages}}),

          # ratio
          RATIO_BEV = ifelse(totbev > 0, (({{waterhealthybev}} + {{unsweetmilk}} + {{unsweetplantbevpro}}) / totbev), NA),

          # score
          HEFI2019C6_BEVERAGES = ifelse(totbev > 0, (10 * (RATIO_BEV)), 0),
          HEFI2019C6_BEVERAGES = ifelse(HEFI2019C6_BEVERAGES > 10, NA, HEFI2019C6_BEVERAGES), # > 10 indicates that more bev were consumed than total bev (impossible)

          # Component 7 - Fatty acids ratio

          # input limits
          FATmin = 1.1,
          FATmax = 2.6,

          # sum
          unsatfat = ({{mufat}} + {{pufat}}),

          # ratio
          RATIO_FA = ifelse({{satfat}} > 0, (({{mufat}} + {{pufat}}) / {{satfat}}), NA),

          # score
          HEFI2019C7_FATTYACID = ifelse({{satfat}} == 0,
            ifelse(unsatfat > 0, 5, 0),
            5 * ((RATIO_FA - FATmin) / (FATmax - FATmin))
          ),
          HEFI2019C7_FATTYACID = ifelse(is.na(RATIO_FA), HEFI2019C7_FATTYACID,
            ifelse((RATIO_FA >= FATmax), 5,
              ifelse((RATIO_FA <= FATmin), 0, HEFI2019C7_FATTYACID)
            )
          ),

          # Component 8 - Saturated fats

          # input limits
          SFAmin = 10,
          SFAmax = 15,

          # ratio
          SFA_PERC = ifelse(({{energy}} > 0), 100 * ({{satfat}} * 9 / {{energy}}), NA),

          # score
          HEFI2019C8_SFAT = ifelse(({{energy}} == 0), 0,
            5 - (5 * (SFA_PERC - SFAmin) / (SFAmax - SFAmin))
          ),
          HEFI2019C8_SFAT = ifelse((SFA_PERC < SFAmin), 5, HEFI2019C8_SFAT),
          HEFI2019C8_SFAT = ifelse((SFA_PERC >= SFAmax), 0, HEFI2019C8_SFAT),

          # Component 9 - Free sugars

          # input limits
          SUGmin = 10,
          SUGmax = 20,

          # ratio
          SUG_PERC = ifelse(({{energy}} > 0), 100 * ({{freesugars}} * 4 / {{energy}}), NA),

          # score
          HEFI2019C9_FREESUGARS = ifelse(({{energy}} == 0), 0,
            10 - (10 * (SUG_PERC - SUGmin) / (SUGmax - SUGmin))
          ),
          HEFI2019C9_FREESUGARS = ifelse((SUG_PERC < SUGmin), 10, HEFI2019C9_FREESUGARS),
          HEFI2019C9_FREESUGARS = ifelse((SUG_PERC >= SUGmax), 0, HEFI2019C9_FREESUGARS),

          # Component 10 - Sodium

          # input limits
          SODmin = 0.9,
          SODmax = 2.0,

          # ratio
          SODDEN = ifelse({{energy}} > 0, ({{sodium}} / {{energy}}), NA),

          # score
          HEFI2019C10_SODIUM = ifelse(({{energy}} == 0), 0,
            10 - (10 * (SODDEN - SODmin) / (SODmax - SODmin))
          ),
          HEFI2019C10_SODIUM = ifelse((SODDEN < SODmin), 10, HEFI2019C10_SODIUM),
          HEFI2019C10_SODIUM = ifelse((SODDEN >= SODmax), 0, HEFI2019C10_SODIUM),

          # Total score: the sum of the HEFI2019 component scores

          HEFI2019_TOTAL_SCORE = (HEFI2019C1_VF + HEFI2019C2_WHOLEGR + HEFI2019C3_GRRATIO + HEFI2019C4_PROFOODS +
            HEFI2019C5_PLANTPRO + HEFI2019C6_BEVERAGES + HEFI2019C7_FATTYACID + HEFI2019C8_SFAT +
            HEFI2019C9_FREESUGARS + HEFI2019C10_SODIUM)
        ) |> # end of mutate
        dplyr::select( -c(
          "probev_gram_per_RA", "unsweetmilk_RA", "unsweetplantbevpro_RA", "totgrain", "totpro",
          "unsatfat", "FATmin", "FATmax", "totbev", "SFAmin", "SFAmax", "SUGmin", "SUGmax", "SODmin", "SODmax"
        )) # end of select, temporary variables are cleared

    # define labels
    labels <- list(
        totfoodsRA            = 'Total foods (RA/d)',
        RATIO_VF              = 'Ratio of vegetable and fruits over total foods',
        RATIO_WGTOT           = 'Ratio of whole-grain foods over total foods',
        RATIO_WGGR            = 'Ratio of whole-grain foods over total grains',
        RATIO_PRO             = 'Ratio of protein foods over total foods',
        RATIO_PLANT           = 'Ratio of plant-based over protein foods',
        RATIO_BEV             = 'Ratio of beverages over total beverages',
        RATIO_FA              = 'Ratio of unsaturated over saturated fats',
        SFA_PERC              = 'Percent of calories from saturated fat',
        SUG_PERC              = 'Percent of calories from free sugars',
        SODDEN                = 'Ratio of sodium over total energy',
        HEFI2019C1_VF         = 'HEFI2019 C1 Vegetables and fruits',
        HEFI2019C2_WHOLEGR    = 'HEFI2019 C2 Whole-grain foods',
        HEFI2019C3_GRRATIO    = 'HEFI2019 C3 Grain foods ratio',
        HEFI2019C4_PROFOODS   = 'HEFI2019 C4 Protein foods',
        HEFI2019C5_PLANTPRO   = 'HEFI2019 C5 Plant-based protein foods',
        HEFI2019C6_BEVERAGES  = 'HEFI2019 C6 Beverages',
        HEFI2019C7_FATTYACID  = 'HEFI2019 C7 Fatty acids ratio',
        HEFI2019C8_SFAT       = 'HEFI2019 C8 Saturated fats',
        HEFI2019C9_FREESUGARS = 'HEFI2019 C9 Free sugars',
        HEFI2019C10_SODIUM    = 'HEFI2019 C10 Sodium',
        HEFI2019_TOTAL_SCORE  = 'Total Healthy Eating Food Index (/80)')

    # label the variables in <outdata>
    for (var_name in names(outdata)) {
      attr(outdata[[var_name]], "label") <- labels[[var_name]]
    }

    return(outdata ) # end of return
  }
