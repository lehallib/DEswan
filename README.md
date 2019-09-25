
<!-- README.md is generated from README.Rmd. Please edit that file -->
DEswan package
==============

Run a Differential Expression - Sliding Window ANalysis (DEswan) as described by Lehallier et al. (<https://www.biorxiv.org/content/10.1101/751115v1>)

Installation
------------

You can install DEswan from github with:

``` r
# install.packages("devtools")
devtools::install_github("lehallib/DEswan",build_vignettes = T)
```

DEswan approach
===============

DEswan explore linear and non linear relationships between a quantitative trait (l) and one or more features.

Considering a vector l of k unique values, we iteratively use l<sub>k</sub> as the center of a window of size x and group samples in parcels below and above l<sub>k</sub>

i.e. \[l<sub>k</sub>-x/2 ; l<sub>k</sub>\[ and \]l<sub>k</sub> ; l<sub>k</sub>+x/2\]

To test for differential expression, we use the following linear model:

Feature<sub>i</sub> ~ α+β<sub>1</sub> l<sub>k</sub>(below/above)+ε

l<sub>k</sub>(below-above) being binarized according to the parcels around l<sub>k</sub>. Covariates can be included in the modeling as follows:

Feature<sub>i</sub> ~ α+β<sub>1</sub> l<sub>k</sub>(Low/High)+β<sub>2</sub> Covariate<sub>a</sub>+...+β<sub>x</sub> Covariate<sub>x</sub>+ε

Type II sum of squares are calculated using the Anova function of the R car package.

When analyzing the links between l and more than one feature, we recommend to estimate q-values for each l<sub>k</sub> using Benjamini–Hochberg correction. To assess the robustness and relevance of DE-SWAN results, we recommend to test multiple parcel widths and different p/q-values thresholds.

DEswan example
--------------

See the vignette for examples
