
# Healthy Eating Food Index-2019

<!-- badges: start -->
<!-- badges: end -->

## Background

Canada’s Food Guide (CFG) 2019 includes dietary guidance both on food
choices and food-related behaviors. The Healthy Eating Food Index 2019
(HEFI-2019) is a scoring tool developed to measure adherence to the
recommendations on food choices in CFG. The HEFI-2019 has 10 components,
of which 5 are based on the intake of foods, 1 on beverages, and 4 on
nutrients, with the total score ranging from 0 to 80 points.

## Scoring algorithm

The goal of the *hefi2019* code is to apply the HEFI-2019 scoring
algorithm to dietary constituents in the dataset provided by the user.
The original variables are kept in the output data set, along with new
variables including: 1) density of intakes (i.e., ratios of dietary
constituents); 2) the HEFI-2019 component scores; and 3) the total
HEFI-2019 score. See Brassard et al. (Appl Physiol Nutr Metab 2021;2022) for details regarding
the development and the evaluation of the HEFI-2019, as well as
information on classification of foods. Of note, when no foods,
beverages or energy is reported, ratios are not calculated and a score
of 0 is assigned to the corresponding components.

## R Version

### Installation

You can install the released version of the *hefi2019* from
[GitHub](https://github.com) with:

``` r
devtools::install_github("didierbrassard/hefi2019")
```

### Suggested layout for the input dataset

The function should ideally be applied to a dataset in the “long”
format, where observations are rows and dietary constituents are
columns. Other layouts are also possible.

### Example

The scoring algorithm is used by indicating the name of the input data
set and the name of each variable representing dietary constituents of
the HEFI-2019. In the case where a given dietary constituents was not
reported in an entire sample, a 0 can be assigned to that variable. Upon
execution, the title of the function is displayed.

``` r
#library(hefi2019)
devtools::load_all()
#> ℹ Loading hefi2019

mydata_scored <- hefi2019(indata         = mydata,
                         vegwfruit       = RA_qty_vegwfruit,
                         wholegrfoods    = RA_qty_grfoodswhole,
                         nonwholegrfoods = RA_qty_nonwgfoods,
                         profoodsanimal  = RA_qty_profoodsanimal,
                         profoodsplant   = RA_qty_profoodsplant,
                         otherfoods      = RA_qty_other,
                         water_and_other_healthy = water_and_other_healthy,
                         unsweetmilk     = milk,
                         unsweetplantbevpro = 0, # notice that this food was not consumed and is given a value of 0
                         otherbev        = otherbev ,
                         mufat           = mufa ,
                         pufat           = pufa ,
                         satfat          = sfa ,
                         sugars          = sugfree,
                         kcal            = energy,
                         sodium          = sodium )
#> Healthy Eating Food Index-2019 Scoring Algorithm R version 1.1
```

## Scoring Algorithm Output

### Density of intakes

The scoring algorithm creates 10 variables for density of intakes:
*RATIO\_VF, RATIO\_WGTOT, RATIO\_WGGR, RATIO\_PRO, RATIO\_PLANT,
RATIO\_BEV, RATIO\_UNSFAT, SFA\_PERC, SUG\_PERC, and SODDEN*.

### Total HEFI-2019 and its components

The variable corresponding to the total HEFI-2019 is
*HEFI2019\_TOTAL\_SCORE* and the 10 variables corresponding to each
component of the HEFI-2019 are *HEFI2019C1\_VF, HEFI2019C2\_WHOLEGR,
HEFI2019C3\_GRRATIO, HEFI2019C4\_PROFOODS, HEFI2019C5\_PLANTPRO,
HEFI2019C6\_BEVERAGES, HEFI2019C7\_FATTYACID, HEFI2019C8\_SFAT,
HEFI2019C9\_SUGARS, and HEFI2019C10\_SODIUM*.
