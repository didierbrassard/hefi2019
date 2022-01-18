
# Healthy Eating Food Index-2019

<!-- badges: start -->
<!-- badges: end -->

## Background

[Canada’s Food Guide (CFG) 2019](https://food-guide.canada.ca/en/)
includes dietary guidance both on healthy food choices and eating
behaviors. The Healthy Eating Food Index (HEFI)-2019 is a scoring tool
developed to measure adherence to recommendations on healthy food
choices in CFG (Brassard, Elvidge Munene, St-Pierre, Guenther, et al.
2022). The HEFI-2019 has 10 components, of which 5 are based on the
intake of foods, 1 on beverages, and 4 on nutrients. The total HEFI-2019
score has a maximum of 80 points.

## Scoring algorithm

The goal of the *hefi2019* code is to apply the HEFI-2019 scoring
algorithm to dietary constituents in the dataset provided by the user.
The original variables are kept in the output data set, along with new
variables including:

1.  density of intakes (i.e., ratios of dietary constituents);
2.  the HEFI-2019 component scores; and
3.  the total HEFI-2019 score.

See the development and evaluation articles for more details as well as
information on classification of foods (Brassard, Elvidge Munene,
St-Pierre, Guenther, et al. 2022; Brassard, Elvidge Munene, St-Pierre,
Gonzalez, et al. 2022). Of note, when no foods, beverages or energy is
reported, ratios are not calculated and a score of 0 is assigned to the
corresponding components.

### Suggested layout for the input dataset

The scoring algorithm should ideally be applied to a dataset in the
“long” format, where observations are rows and dietary constituents are
columns, as shown below. Other layouts are also possible.

    #> # A tibble: 5 × 5
    #>   Participants Food1 Food2 Food3 Food...
    #>          <int> <int> <int> <int> <chr>  
    #> 1            1     2     1     9 ...    
    #> 2            2     4     4     5 ...    
    #> 3            3     2     7     6 ...    
    #> 4            4     5     4     5 ...    
    #> 5            5     6     4     2 ...

[SAS](./SAS/hefi2019.scoring.macro.sas) and R versions of the scoring
algorithm are available.

## R Version

### Installation

You can install the released version of the *hefi2019* from
[GitHub](https://github.com) with:

``` r
devtools::install_github("didierbrassard/hefi2019")
```

### Example application

The scoring algorithm is used by indicating the name of the input data
set and the name of each variable representing dietary constituents of
the HEFI-2019. In the case where a given dietary constituents was not
reported in an entire sample, a 0 can be assigned to that variable. Upon
execution, the title of the function is displayed.

``` r
#library(hefi2019)
devtools::load_all()
#> ℹ Loading hefi2019

# Apply the scoring algorithm to user-provided data
mydata_scored <- hefi2019(indata         = mydata,
                         vegwfruits      = RA_qty_vegwfruit,
                         wholegrfoods    = RA_qty_grfoodswhole,
                         nonwholegrfoods = RA_qty_nonwgfoods,
                         profoodsanimal  = RA_qty_profoodsanimal,
                         profoodsplant   = RA_qty_profoodsplant,
                         otherfoods      = RA_qty_other,
                         water_and_other_healthy = water_and_other_healthy,
                         unsweetmilk     = milk,
                         unsweetplantbevpro = 0, # 0 = food was not consumed
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

# References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-RN1229" class="csl-entry">

Brassard, D., L. A. Elvidge Munene, S. St-Pierre, A. Gonzalez, P. M.
Guenther, M. Jessri, J. Vena, et al. 2022. “Evaluation of the Healthy
Eating Food Index (HEFI)-2019 Measuring Adherence to Canada’s Food Guide
2019 Recommendations on Healthy Food Choices.” Journal Article. *Appl
Physiol Nutr Metab*. <https://doi.org/10.1139/apnm-2021-0416>.

</div>

<div id="ref-RN1226" class="csl-entry">

Brassard, D., L. A. Elvidge Munene, S. St-Pierre, P. M. Guenther, S. I.
Kirkpatrick, J. Slater, S. Lemieux, et al. 2022. “Development of the
Healthy Eating Food Index (HEFI)-2019 Measuring Adherence to Canada’s
Food Guide 2019 Recommendations on Healthy Food Choices.” Journal
Article. <https://doi.org/10.1139/apnm-2021-0415>.

</div>

</div>
