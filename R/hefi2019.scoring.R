#' Healthy Eating Food Index-2019 Scoring Algorithm
#'
#' @description This function scores dietary constituents provided in the input data frame
#'   according to the Healthy Eating Food Index (HEFI)-2019 scoring algorithm.
#'   The original variables are kept in the output data frame.
#'
#' @details When no foods, beverages or energy are reported, ratios are
#'   not calculated and a score of `0` is assigned to the corresponding
#'   components. Missing data for any dietary constituents will results in
#'   missing components score(s) and total score.
#'
#' @details Suggested layout for the input data frame:
#'   the function should ideally be applied to a data frame where rows correspond
#'   to individuals and dietary constituents are columns.
#'
#' @details Caution variable names:
#'   `totfoodsRA`, `RATIO_VF`, `RATIO_WGTOT`, `RATIO_WGGR`,
#'   `RATIO_PRO`, `RATIO_PLANT`, `RATIO_FA`, `RATIO_BEV`,
#'   `SFA_PERC`, `SUG_PERC`, `SODDEN` are reserved for this function.
#'
#' @param indata Input data frame with dietary constituents
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
#' @examples
#' \dontrun{
#' mydata_scored <-
#' hefi2019(indata             = mydata,
#'          vegfruits          = "RA_vegfruits",
#'          wholegrfoods       = "RA_wholegrfoods",
#'          nonwholegrfoods    = "RA_nonwgfoods",
#'          profoodsanimal     = "RA_profoodsanimal",
#'          profoodsplant      = "RA_profoodsplant",
#'          otherfoods         = "RA_otherfoods",
#'          waterhealthybev    = "G_waterhealthybev",
#'          unsweetmilk        = "G_milk",
#'          unsweetplantbevpro = "G_plantbevpro",
#'          otherbeverages     = "G_otherbeverages" ,
#'          mufat              = "G_mufa" ,
#'          pufat              = "G_pufa" ,
#'          satfat             = "G_sfa" ,
#'          freesugars         = "G_freesugars",
#'          sodium             = "MG_sodium",
#'          energy             = "energy")
#' }
#'
#' @references
#' Brassard et al. Appl Physiol Nutr Metab. 2022. Development of the Healthy Eating Food Index (HEFI)-2019 measuring adherence to Canada's Food Guide 2019 recommendations on healthy food choices. <https://doi.org/10.1139/apnm-2021-0415>
#'
#' Brassard et al. Appl Physiol Nutr Metab. 2022. Evaluation of the Healthy Eating Food Index (HEFI)-2019 measuring adherence to Canada's Food Guide 2019 recommendations on healthy food choices. <https://doi.org/10.1139/apnm-2021-0416>
#'
#' @seealso Health Canada. Open Government Portal. 2023. The Healthy Eating Food Index 2019. Record ID: 29892c85-2ff5-484c-873c-f494ffba6e1b. <https://open.canada.ca/data/en/dataset/29892c85-2ff5-484c-873c-f494ffba6e1b>
#'
#' @return A data frame (\code{indata}) with additional variables including:
#' * Density of intakes (i.e., ratios of dietary constituents)
#' * HEFI-2019 total score
#' * HEFI-2019 component scores
#'
#' @export

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

    message("Healthy Eating Food Index-2019 Scoring Algorithm R version 1.5")

    # Indicate gram per RA for unsweetened milk and unsweetened plant-based beverages with sufficient protein
    probev_gram_per_RA <- 258

    # calculate reference amounts from unsweetened milk and unsweetened plant-based beverages protein foods
    unsweetmilk_RA <- indata[[unsweetmilk]] / probev_gram_per_RA
    unsweetplantbevpro_RA <- indata[[unsweetplantbevpro]] / probev_gram_per_RA

    # sum total reference amounts from foods and protein beverages
    totfoodsRA <- indata[[vegfruits]] + indata[[wholegrfoods]] + indata[[nonwholegrfoods]] +
      indata[[profoodsanimal]] + indata[[profoodsplant]] + indata[[otherfoods]] +
      unsweetmilk_RA + unsweetplantbevpro_RA

    ## add to data
    indata[["totfoodsRA"]] <- totfoodsRA

    # scoring functions
    score_adequacy <- function(num, den, standard, point_max, save_ratio=TRUE, point_min=0){
      ratio <- ifelse(den > 0, num / den, NA)
      score <- ifelse(den > 0, (point_max * (ratio / standard)), point_min)
      score <- ifelse(score > point_max, point_max, score)
      if(save_ratio) return(list(ratio,score)) else return(score)
    }
    score_ratio <- function(num, den, standard_max, standard_min, point_max, save_ratio=TRUE, point_min=0){
      ratio <- ifelse(den > 0, num / den, NA)
      score <- ifelse(den == 0,
                      ifelse(num > 0, point_max, point_min),
                      point_max * ((ratio - standard_min) / (standard_max - standard_min)))
      score <- ifelse(is.na(ratio), score,
                      ifelse(ratio >= standard_max, point_max,
                             ifelse(ratio <= standard_min, point_min, score)))
      if(save_ratio) return(list(ratio,score)) else return(score)
    }
    score_moderation <- function(num, den, standard_max, standard_min, point_max, save_ratio=TRUE, point_min=0){
      ratio <- ifelse(den > 0, num / den, NA)
      score <- ifelse(den == 0, point_min, point_max - (point_max * (ratio - standard_min) / (standard_max - standard_min)))
      score <- ifelse(ratio < standard_min, point_max, score)
      score <- ifelse(ratio >= standard_max, point_min, score)
      if(save_ratio) return(list(ratio,score)) else return(score)
    }

    # Component 1 - Vegetables and fruits
    HEFI2019C1_VF <-
      score_adequacy(
        num       = indata[[vegfruits]],
        den       = totfoodsRA,
        standard  = 0.5,
        point_max = 20,
        point_min = 0)

    if (is.list(HEFI2019C1_VF)) {
      indata[["RATIO_VF"]] <- HEFI2019C1_VF[[1]]
      indata[["HEFI2019C1_VF"]] <- HEFI2019C1_VF[[2]]
    } else {
      indata[["HEFI2019C1_VF"]] <- HEFI2019C1_VF
    }

    # Component 2 - Whole-grain foods
    HEFI2019C2_WHOLEGR <-
      score_adequacy(
        num       = indata[[wholegrfoods]],
        den       = totfoodsRA,
        standard  = 0.25,
        point_max = 5,
        point_min = 0)

    if (is.list(HEFI2019C2_WHOLEGR)) {
      indata[["RATIO_WGTOT"]] <- HEFI2019C2_WHOLEGR[[1]]
      indata[["HEFI2019C2_WHOLEGR"]] <- HEFI2019C2_WHOLEGR[[2]]
    } else {
      indata[["HEFI2019C2_WHOLEGR"]] <- HEFI2019C2_WHOLEGR
    }

    #  Component 3 - Grain foods ratio
    HEFI2019C3_GRRATIO <-
      score_adequacy(
        num       = indata[[wholegrfoods]],
        den       = indata[[wholegrfoods]] + indata[[nonwholegrfoods]],
        standard  = 1,
        point_max = 5,
        point_min = 0)

    if (is.list(HEFI2019C3_GRRATIO)) {
      indata[["RATIO_WGGR"]] <- HEFI2019C3_GRRATIO[[1]]
      indata[["HEFI2019C3_GRRATIO"]] <- HEFI2019C3_GRRATIO[[2]]
    } else {
      indata[["HEFI2019C3_GRRATIO"]] <- HEFI2019C3_GRRATIO
    }

    # Component 4 - Protein foods
    HEFI2019C4_PROFOODS <-
      score_adequacy(
        num       = indata[[profoodsanimal]] + indata[[profoodsplant]] + unsweetmilk_RA + unsweetplantbevpro_RA,
        den       = totfoodsRA,
        standard  = 0.25,
        point_max = 5,
        point_min = 0)

    if (is.list(HEFI2019C4_PROFOODS)) {
      indata[["RATIO_PRO"]] <- HEFI2019C4_PROFOODS[[1]]
      indata[["HEFI2019C4_PROFOODS"]] <- HEFI2019C4_PROFOODS[[2]]
    } else {
      indata[["HEFI2019C4_PROFOODS"]] <- HEFI2019C4_PROFOODS
    }

    # Component 5 - Plant-based protein foods
    HEFI2019C5_PLANTPRO <-
      score_adequacy(
        num       = indata[[profoodsplant]] + unsweetplantbevpro_RA,
        den       = indata[[profoodsanimal]] + indata[[profoodsplant]] + unsweetmilk_RA + unsweetplantbevpro_RA,
        standard  = 0.50000001,
        point_max = 5,
        point_min = 0)

    if (is.list(HEFI2019C5_PLANTPRO)) {
      indata[["RATIO_PLANT"]] <- HEFI2019C5_PLANTPRO[[1]]
      indata[["HEFI2019C5_PLANTPRO"]] <- HEFI2019C5_PLANTPRO[[2]]
    } else {
      indata[["HEFI2019C5_PLANTPRO"]] <- HEFI2019C5_PLANTPRO
    }

    # Component 6 - Beverages
    HEFI2019C6_BEVERAGES <-
      score_adequacy(
        num       = indata[[waterhealthybev]] + indata[[unsweetmilk]] + indata[[unsweetplantbevpro]],
        den       = indata[[waterhealthybev]] + indata[[unsweetmilk]] + indata[[unsweetplantbevpro]] + indata[[otherbeverages]],
        standard  = 1,
        point_max = 10,
        point_min = 0)

    if (is.list(HEFI2019C6_BEVERAGES)) {
      indata[["RATIO_BEV"]] <- HEFI2019C6_BEVERAGES[[1]]
      indata[["HEFI2019C6_BEVERAGES"]] <- HEFI2019C6_BEVERAGES[[2]]
    } else {
      indata[["HEFI2019C6_BEVERAGES"]] <- HEFI2019C6_BEVERAGES
    }

    # Component 7 - Fatty acids ratio
    HEFI2019C7_FATTYACID <-
      score_ratio(
        num          = indata[[mufat]] + indata[[pufat]],
        den          = indata[[satfat]],
        standard_max = 2.6,
        standard_min = 1.1,
        point_max    = 5,
        point_min    = 0)

    if (is.list(HEFI2019C7_FATTYACID)) {
      indata[["RATIO_FA"]] <- HEFI2019C7_FATTYACID[[1]]
      indata[["HEFI2019C7_FATTYACID"]] <- HEFI2019C7_FATTYACID[[2]]
    } else {
      indata[["HEFI2019C7_FATTYACID"]] <- HEFI2019C7_FATTYACID
    }

    # Component 8 - Saturated fats
    HEFI2019C8_SFAT <-
      score_moderation(
        num          = 100 * (indata[[satfat]] * 9),
        den          = indata[[energy]] ,
        standard_max = 15,
        standard_min = 10,
        point_max    = 5,
        point_min    = 0)

    if (is.list(HEFI2019C8_SFAT)) {
      indata[["SFA_PERC"]] <- HEFI2019C8_SFAT[[1]]
      indata[["HEFI2019C8_SFAT"]] <- HEFI2019C8_SFAT[[2]]
    } else {
      indata[["HEFI2019C8_SFAT"]] <- HEFI2019C8_SFAT
    }

    # Component 9 - Free sugars
    HEFI2019C9_FREESUGARS <-
      score_moderation(
        num          = 100 * (indata[[freesugars]] * 4),
        den          = indata[[energy]] ,
        standard_max = 20,
        standard_min = 10,
        point_max    = 10,
        point_min    = 0)

    if (is.list(HEFI2019C9_FREESUGARS)) {
      indata[["SUG_PERC"]] <- HEFI2019C9_FREESUGARS[[1]]
      indata[["HEFI2019C9_FREESUGARS"]] <- HEFI2019C9_FREESUGARS[[2]]
    } else {
      indata[["HEFI2019C9_FREESUGARS"]] <- HEFI2019C9_FREESUGARS
    }

    # Component 10 - Sodium
    HEFI2019C10_SODIUM <-
      score_moderation(
        num          = indata[[sodium]],
        den          = indata[[energy]],
        standard_max = 2.0,
        standard_min = 0.9,
        point_max    = 10,
        point_min    = 0)

    if (is.list(HEFI2019C10_SODIUM)) {
      indata[["SODDEN"]] <- HEFI2019C10_SODIUM[[1]]
      indata[["HEFI2019C10_SODIUM"]] <- HEFI2019C10_SODIUM[[2]]
    } else {
      indata[["HEFI2019C10_SODIUM"]] <- HEFI2019C10_SODIUM
    }

    # Total score: the sum of the HEFI2019 component scores
    indata[["HEFI2019_TOTAL_SCORE"]] <-
      indata[["HEFI2019C1_VF"]] + indata[["HEFI2019C2_WHOLEGR"]] +
      indata[["HEFI2019C3_GRRATIO"]] + indata[["HEFI2019C4_PROFOODS"]] +
      indata[["HEFI2019C5_PLANTPRO"]] + indata[["HEFI2019C6_BEVERAGES"]] +
      indata[["HEFI2019C7_FATTYACID"]] + indata[["HEFI2019C8_SFAT"]] +
      indata[["HEFI2019C9_FREESUGARS"]] + indata[["HEFI2019C10_SODIUM"]]

    # Added variable labels
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

    ## apply the labels
    for (var_name in names(indata)) {
      attr(indata[[var_name]], "label") <- labels[[var_name]]
    }

    return(indata)
  }
