
# Healthy Eating Food Index-2019 Scoring Algorithm (R version)

<!-- badges: start -->
<!-- badges: end -->

The goal of the *hefi2019* package is to apply the Healthy Eating Food
Index (HEFI)-2019 scoring algorithm to dietary constituents in the data
set provided by the user. The original variables are kept in the output
data set, along with new variables including: 1) density of intakes
(i.e., ratios of dietary constituents); 2) the HEFI-2019 component
subscores; and 3) the total HEFI-2019 score. See Lamarche et al., 2021
and Brassard et al., 2021 for details regarding the development and the
evaluation of the HEFI-2019.

## Installation

You can install the released version of the *hefi2019* from
[GitHub](https://github.com) with:

``` r
devtools::install_github("didierbrassard/hefi2019")
```

## Example

The scoring algorithm is used by indicating the name of the input data
set and the name of each variable representing dietary constituents of
the HEFI-2019. Of note, in the case where a given dietary constituents
was not consumed in the entire sample, a 0 can be assigned to that
variable. Upon execution, the title of the function is displayed.

``` r
#library(hefi2019)
devtools::load_all()
#> Loading hefi2019

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
