# Healthy Eating Food Index-2019

- [Background](#background)
  - [Useful resources](#useful-resources)
- [Scoring Algorithm](#scoring-algorithm)
  - [Suggested Layout for the Input
    Dataset](#suggested-layout-for-the-input-dataset)
  - [SAS Version](#sas-version)
  - [R Version](#r-version)
    - [Installation](#installation)
    - [Basic example](#basic-example)
- [Scoring Algorithm Output](#scoring-algorithm-output)
  - [Density of intakes](#density-of-intakes)
  - [Total HEFI-2019 and its
    components](#total-hefi-2019-and-its-components)
- [Selected Publications: Research Uses of the
  HEFI-2019](#selected-publications-research-uses-of-the-hefi-2019)
- [References](#references)

# Background

[Canada’s Food Guide (CFG) 2019](https://food-guide.canada.ca/en/)
includes dietary guidance both on healthy food choices and eating
behaviors. The Healthy Eating Food Index (HEFI)-2019 is a scoring tool
developed to measure adherence to recommendations on healthy food
choices in CFG (Brassard, Elvidge Munene, St-Pierre, Guenther, et al.
2022). The HEFI-2019 has 10 components, of which 5 are based on the
intake of foods, 1 on beverages, and 4 on nutrients. The total HEFI-2019
score has a maximum of 80 points.

## Useful resources

Health Canada has a web page dedicated to [Canada’s Food Guide Research
Tools](https://food-guide.canada.ca/en/research-tools/).  
For the HEFI-2019, Health Canada has also made available a series of
resources and files to assist in the calculation of scores. Notably, the
data and other documents are available under the name [The Healthy
Eating Food Index
2019](https://open.canada.ca/data/en/dataset/29892c85-2ff5-484c-873c-f494ffba6e1b)
on the Open Government data portal. These files are especially helpful
to assist in the preliminary steps, i.e., the classification of foods
and beverages, the addition of data on free sugars as well as data on
reference amounts per gram of foods.

# Scoring Algorithm

The goal of the `hefi2019` code is to apply the HEFI-2019 scoring
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

## Suggested Layout for the Input Dataset

The scoring algorithm should ideally be applied to a dataset in the
“long” format, where observations are rows and dietary constituents are
columns. Other layouts are also possible.

| Participants | Food1 | Food2 | Food3 | Food… |
|-------------:|------:|------:|------:|:------|
|            1 |     2 |     1 |     9 | …     |
|            2 |     4 |     4 |     5 | …     |
|            3 |     2 |     7 |     6 | …     |
|            4 |     5 |     4 |     5 | …     |
|            5 |     6 |     4 |     2 | …     |

[SAS](./SAS/hefi2019.scoring.macro.sas) and
[R](./R/hefi2019.scoring.macro.R) versions of the scoring algorithm are
available. Both versions will yield the same HEFI-2019 scores and output
when applied to the same data.

## SAS Version

A detailed example application of the HEFI-2019 for the *Canadian
Community Health Survey 2015 - Nutrition* is presented in the
[Example_SAS_cchs](https://github.com/didierbrassard/hefi2019/tree/master/Example_SAS_cchs)
folder.

The SAS code illustrates how to prepare data and how to calculate
HEFI-2019 scores according to the population ratio method (Freedman et
al. 2008) as well as based on measurement error-corrected dietary
intakes using the National Cancer Institute multivariate method (Zhang
et al. 2011).

- [01_CCHS2015_Data_preparation.sas](https://github.com/didierbrassard/hefi2019/blob/master/Example_SAS_cchs/01_CCHS2015_Data_preparation.sas):
  preparation of data files
- [02_CCHS2015_Descriptive1d.sas](https://github.com/didierbrassard/hefi2019/blob/master/Example_SAS_cchs/02_CCHS2015_Descriptive1d.sas):
  population ratio method
- [03A_CCHS2015_DescriptiveU.sas](https://github.com/didierbrassard/hefi2019/blob/master/Example_SAS_cchs/03A_CCHS2015_DescriptiveU.sas):
  measurement error correction with the multivariate method
- [03B_CCHS2015_DescriptiveU.sas](https://github.com/didierbrassard/hefi2019/blob/master/Example_SAS_cchs/03B_CCHS2015_DescriptiveU.sas):
  estimation of usual intake distribution

## R Version

A detailed example application of the HEFI-2019 for the *Canadian
Community Health Survey 2015 - Nutrition* is presented in the
[Example_R_cchs](https://github.com/didierbrassard/hefi2019/tree/master/Example_R_cchs)
folder.

The R code illustrates how to prepare data and how to calculate
HEFI-2019 scores according to the population ratio method.

- [01_CCHS2015_Data_preparation.md](https://github.com/didierbrassard/hefi2019/blob/master/Example_R_cchs/Output/01_CCHS2015_Data_preparation.md)
- [02_CCHS2015_Descriptive1d.md](https://github.com/didierbrassard/hefi2019/blob/master/Example_R_cchs/Output/02_CCHS2015_Descriptive1d.md)

A simple application for the R version is shown below.

### Installation

You can install the released version of the `hefi2019` from
[GitHub](https://github.com/didierbrassard/hefi2019) with:

``` r
devtools::install_github("didierbrassard/hefi2019")
```

### Basic example

The scoring algorithm is used by indicating the name of the input data
set and the name of each variable representing dietary constituents of
the HEFI-2019. In the case where a given dietary constituents was not
reported in an entire sample, a 0 can be assigned to that variable. Upon
execution, the title of the function is displayed.

``` r
# Install the hefi2019 scoring algorithm from GitHub, if not already one
# devtools::install_github("didierbrassard/hefi2019")

# Load library
library(hefi2019)

# Apply the scoring algorithm to user-provided data
mydata_scored <- 
  hefi2019(indata             = mydata,
           vegfruits          = RA_vegfruits,
           wholegrfoods       = RA_wholegrfoods,
           nonwholegrfoods    = RA_nonwgfoods,
           profoodsanimal     = RA_profoodsanimal,
           profoodsplant      = RA_profoodsplant,
           otherfoods         = RA_otherfoods,
           waterhealthybev    = G_waterhealthybev,
           unsweetmilk        = G_milk,
           unsweetplantbevpro = G_plantbevpro,
           otherbeverages     = G_otherbeverages ,
           mufat              = G_mufa ,
           pufat              = G_pufa ,
           satfat             = G_sfa ,
           freesugars         = G_freesugars,
           sodium             = MG_sodium,
           energy             = energy
           )
## Healthy Eating Food Index-2019 Scoring Algorithm R version 1.4
```

# Scoring Algorithm Output

## Density of intakes

The scoring algorithm creates 10 variables for density of intakes:
*RATIO_VF, RATIO_WGTOT, RATIO_WGGR, RATIO_PRO, RATIO_PLANT, RATIO_BEV,
RATIO_UNSFAT, RATIO_FA, SFA_PERC, SUG_PERC, and SODDEN*.

## Total HEFI-2019 and its components

The variable corresponding to the total HEFI-2019 is
*HEFI2019_TOTAL_SCORE* and the 10 variables corresponding to each
component of the HEFI-2019 are *HEFI2019C1_VF, HEFI2019C2_WHOLEGR,
HEFI2019C3_GRRATIO, HEFI2019C4_PROFOODS, HEFI2019C5_PLANTPRO,
HEFI2019C6_BEVERAGES, HEFI2019C7_FATTYACID, HEFI2019C8_SFAT,
HEFI2019C9_FREESUGARS, and HEFI2019C10_SODIUM*.

# Selected Publications: Research Uses of the HEFI-2019

**Epidemiology & Health Outcomes Research**

- Brassard, D., Manikpurage, H. D., Theriault, S., Arsenault, B. J., &
  Lamarche, B. (2022). [Greater adherence to the 2019 Canada’s Food
  Guide recommendations on healthy food choices reduces the risk of
  cardiovascular disease in adults: a prospective analysis of UK Biobank
  data](https://doi.org/10.1093/ajcn/nqac256). Am J Clin Nutr, 116(6),
  1748-1758.

**Nutrition Surveillance**

- Lee, J. J., Ahmed, M., Julia, C., Ng, A. P., Paper, L., Lou, W. Y., &
  L’Abbe, M. R. (2023). [Examining the diet quality of Canadian adults
  and the alignment of Canadian front-of-pack labelling regulations with
  other front-of-pack labelling systems and dietary
  guidelines](https://doi.org/10.3389/fpubh.2023.1168745). Front Public
  Health, 11, 1168745.
- Brassard, D., & Chevalier, S. (2023). [Relationship between adherence
  to the 2019 Canada’s Food Guide recommendations on healthy food
  choices and nutrient intakes in older
  adults](https://doi.org/10.1016/j.tjnut.2023.07.005). J Nutr, 153(9),
  2699-2708.
  - [Pre-print version (freely
    available)](https://doi.org/10.1101/2023.02.13.23285868)

**Randomized controlled trials**

- Bernier, E., Plante, A. S., Lemieux, P., Robitaille, J., Lemieux, S.,
  Desroches, S., Belanger-Gravel, A., Maheux-Lacroix, S., Weisnagel, S.
  J., Demers, S., Camirand Lemyre, F., Boulet, M., Baillargeon, J. P., &
  Morisset, A. S. (2023). [Promoting healthy eating in early pregnancy
  in individuals at risk of gestational diabetes mellitus: does it
  improve glucose homeostasis? A study protocol for a randomized control
  trial](https://doi.org/10.3389/fnut.2023.1336509). Front Nutr, 10,
  1336509.
- Olstad, D. L., Beall, R., Spackman, E., Dunn, S., Lipscombe, L. L.,
  Williams, K., Oster, R., Scott, S., Zimmermann, G. L., McBrien, K. A.,
  Steer, K. J. D., Chan, C. B., Tyminski, S., Berkowitz, S., Edwards, A.
  L., Saunders-Smith, T., Tariq, S., Popeski, N., White, L., . . .
  Campbell, D. J. T. (2022). [Healthy food prescription incentive
  programme for adults with type 2 diabetes who are experiencing food
  insecurity: protocol for a randomised controlled trial, modelling and
  implementation studies](https://doi.org/10.1136/bmjopen-2021-050006).
  BMJ Open, 12(2), e050006.

**Sustainability**

- Rochefort, G., Brassard, D., Desroches, S., Robitaille, J., Lemieux,
  S., Provencher, V., & Lamarche, B. (2023). [Transitioning to
  sustainable dietary patterns: learnings from animal-based and
  plant-based dietary patterns in French Canadian
  adults](https://doi.org/10.3389/fnut.2023.1148137). Frontiers in
  Nutrition, 10.
- Rochefort, G., Brassard, D., Paquette, M. C., Robitaille, J., Lemieux,
  S., Provencher, V., & Lamarche, B. (2022). [Adhering to Canada’s Food
  Guide Recommendations on Healthy Food Choices Increases the Daily Diet
  Cost: Insights from the PREDISE
  Study](https://doi.org/10.3390/nu14183818). Nutrients, 14(18).

**Validation Research**

- Lee, J. J., Ahmed, M., Julia, C., Ng, A. P., Paper, L., Lou, W. Y., &
  L’Abbe, M. R. (2023). [Examining the diet quality of Canadian adults
  and the alignment of Canadian front-of-pack labelling regulations with
  other front-of-pack labelling systems and dietary
  guidelines](https://doi.org/10.3389/fpubh.2023.1168745). Front Public
  Health, 11, 1168745.
- Hutchinson, J. M., Dodd, K. W., Guenther, P. M., Lamarche, B., Haines,
  J., Wallace, A., Perreault, M., Williams, T. E., Louzada, M., Jessri,
  M., Lemieux, S., Olstad, D. L., Prowse, R., Simpson, J. R., Vena, J.
  E., Szajbely, K. & Kirkpatrick, S. I. (2023). [The Canadian Food
  Intake Screener for assessing alignment of adults’ dietary intake with
  the 2019 Canada’s Food Guide healthy food choices recommendations:
  scoring system and construct
  validity](https://doi.org/10.1139/apnm-2023-0018). Appl Physiol Nutr
  Metab, 48(8), 620-633.
- Mercier, A. P., Rochefort, G., Fortier, J., Parent, G., Provencher,
  V., Lemieux, S., & Lamarche, B. (2022). [Development and Validation of
  a Short Questionnaire Assessing the Behavior of Local Food Procurement
  in Quebec, Canada](https://doi.org/10.1093/cdn/nzac097). Curr Dev
  Nutr, 6(9), nzac097.

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

<div id="ref-Freedman2008" class="csl-entry">

Freedman, L. S., P. M. Guenther, S. M. Krebs-Smith, and P. S. Kott.
2008. “A Population’s Mean Healthy Eating Index-2005 Scores Are Best
Estimated by the Score of the Population Ratio When One 24-Hour Recall
Is Available.” Journal Article. *J Nutr* 138 (9): 1725–29.
<https://doi.org/10.1093/jn/138.9.1725>.

</div>

<div id="ref-Zhang2011" class="csl-entry">

Zhang, S., D. Midthune, P. M. Guenther, S. M. Krebs-Smith, V. Kipnis, K.
W. Dodd, D. W. Buckman, J. A. Tooze, L. Freedman, and R. J. Carroll.
2011. “A New Multivariate Measurement Error Model with Zero-Inflated
Dietary Data, and Its Application to Dietary Assessment.” Journal
Article. *Ann Appl Stat* 5 (2B): 1456–87.
<https://doi.org/10.1214/10-AOAS446>.

</div>

</div>
