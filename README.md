
## Code for Tourette Syndrome Analysis in “Using Small-Area Estimation (SAE) to Estimate Prevalence of Child Health Outcomes at the Census Regional, State, and County Levels”

Small area estimation (SAE) is a powerful tool with many strengths.
Primarily, it estimates prevalence rates in areas with small sample
sizes by borrowing information from areas with larger sample sizes.
Moreover, it is also a flexible technique. SAE can employ
individual-level as well as area-level predictors for data that is
publicly available (e.g., Tourette Syndrome), for data that requires
simplifying assumptions (e.g., overweight/obesity; Zgodic et al.
(2021)), and for data that relies on other SAE models (e.g., autism
spectrum disorder). SAE can also incorporate spatial information using
intrinsic conditional autoregressive random intercept terms (ICAR;
Banerjee, Carlin, and Gelfand (2004); Cressie (1993)). To aggregate
group level estimated prevalences, a common tool is poststratification,
which creates weighted averages that reflect a population’s underlying
distribution of characteristics.

The code files in this repository provide an example of SAE with
poststratification using data from the National Survey of Children’s
Health. The outcome of interest is Tourette Syndrome status at the child
level. The SAS code file `code_model_ts.sas` reads in dataset
`nsch_ts_data.csv` and shows how to apply SAE at the state level. This
code files also writes SAE model outputs required for
poststratification: random effects BLUPs `ts_model_RE.csv`, fixed
effects coefficients `ts_model_est.csv`, and fixed effects covariance
matrix `ts_model_cov.csv`.

The fitted regression model from the SAE analysis results in prevalence
estimates for all strata relevant to the model variables. For instance,
in a model that uses race-ethnicity (four categories) and biological sex
(two categories), there are 8 different
![(8 = 4 \\times 2)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%288%20%3D%204%20%5Ctimes%202%29 "(8 = 4 \times 2)")
possible combinations (strata) of demographic variables. Poststratification combines the strata specific estimates into a single estimate via a weighted sum.  Specifically, each strata specific estimate is multiplied by the number of children the small area has in the strata (i.e., population counts) and divided by the total number of children in that small area. This results in a single small area estimate that reflects each small area’s demographic make-up. The
R code file `code_poststratification_ts.R` carries out the
poststratification process. The files containing counts to create
poststratification weights are `2016_ts_county_n.csv`,
`2017_ts_county_n.csv`, and `2018_ts_county_n.csv`. Remaining files in
this directory are used to merge relevant dataframes together
(`FIPSST.csv`, `fips_to_county.csv`).

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Banerjee04" class="csl-entry">

Banerjee, S., B. P. Carlin, and A. E. Gelfand. 2004. *Hierarchical
Modeling and Analysis for Spatial Data*. Chapman & Hall.

</div>

<div id="ref-Cressie93" class="csl-entry">

Cressie, N. 1993. *Statistics for Spatial Data*. John Wiley & Sons.

</div>

<div id="ref-Zgodic20" class="csl-entry">

Zgodic, A., J. M. Eberth, C. B. Breneman, M. E. Wende, A. T. Kaczynski,
A. D. Liese, and A. C. McLain. 2021. “Estimates of Childhood Overweight
and Obesity at the Region, State, and County Levels: A Multilevel
Small-Area Estimation Approach.” *American Journal of Epidemiology*.

</div>

</div>
