# `certedata`
This is an [R package](https://www.r-project.org) used by the Data Management department at [Certe](https://www.certe.nl), a medical laboratory in the Netherlands.

![Certe-logo img](man/figures/logo-img.png)![Certe-logo txt](man/figures/logo-txt.png)

Read about copyright [here](#copyright).

**Download the [manual here](https://github.com/msberends/certedata/raw/master/man/figures/manual.pdf).**

#### Acknowledgements
We use [<img src="man/figures/logo_r.png" alt="R programming language" width="20px">](https://www.r-project.org) for our professional analyses, because the [free and open source R programming language](https://en.m.wikipedia.org/wiki/R_(programming_language)) leans to the cutting edge of data science, giving businesses like ours the latest data analysis tools at no charge because it is open-source software (under [GPLv2](https://www.r-project.org/COPYING)). We use [RStudio](https://www.rstudio.com/products/RStudio/) (also open-source, under [AGPLv3](https://www.rstudio.com/products/)) to work with R, which is an awesome [IDE](https://en.wikipedia.org/wiki/Integrated_development_environment). Our [`citations()`](man/citations.Rd) function automatically includes these software packages. We thank all developers and contributors in these projects.

## Why this package?
This R package contains functions to make microbiological, epidemiological and quality control data analysis easier for data scientists and data analysts at Certe ([www.certe](https://www.certe.nl)). 
This R package is also being used for academic research by some of this package's authors, who are PhD students of the [University of Groningen](https://www.rug.nl/).

Most functions rely heavily on R package [`dplyr`](https://github.com/tidyverse/dplyr). All base functions containing `na.rm` are overwritten to make them `na.rm = TRUE` by default. These functions are: `all`, `any`, `max`, `mean`, `median`, `min`, `pmax`, `pmin`, `prod`, `quantile`, `range`, `sd`, `sum` and `var`. The default `type` parameter of the `quantile` function is also overwritten, to make its methodology comply with SPSS and Minitab standards.

Theme colours of Certe are automatically applied to all plots created with `plot2`, which is based on [`ggplot2`](https://github.com/tidyverse/ggplot2). For convience, R Markdown reports can be made based on a Certe template, and all number formats get a decimal comma when `Sys.isdecimalcomma() == TRUE` (which checks the system localization).

All functions and parameters are in English, but all manuals are in Dutch.

## How to use it?
```r
# Call it with:
library(certedata)

# For a list of functions:
help(package = "certedata")

# Create a tibble with random data for testing
tbl1 <- tibble(
  x = rep(LETTERS[1:6], 3),
  type = paste("Type", c(rep(1, 6), rep(2, 6), rep(3, 6))),
  value = round(runif(18) * 500)
)
```
### `str2()`
New function to view structure; `str2()` which also outputs amount of missing data and all unique values:
```r
str(tbl1)
# Classes ‘tbl_df’, ‘tbl’ and 'data.frame': 18 obs. of  3 variables:
#  $ x    : chr  "A" "B" "C" "D" ...
#  $ type : chr  "Type 1" "Type 1" "Type 1" "Type 1" ...
#  $ value: num  132 6 322 472 206 280 267 101 53 16 ...

str2(tbl1)
# 
# Class: tibble (data.frame -> tbl -> tbl_df)
# Size:  18 obs. of 3 variables
# 
# ========  =========  ====  =====  ======  =====================================================
# Variable  Class      <NA>  <NA>%  Unique  Unique values (sorted ascending)
# ========  =========  ====  =====  ======  =====================================================
# $x        character     0     0%       6  "A" "B" "C" "D" "E" "F"
# $type     character     0     0%       3  "Type 1" "Type 2" "Type 3"
# $value    numeric       0     0%      18  36 69 99 101 103 104 132 198 203 216 272 280 322 3...
# ========  =========  ====  =====  ======  =====================================================
```
### `plot2()`
Example of a default `plot2`, a new wrapper for `ggplot` which also supports quasiquotation:
```r
plot2(tbl1, title = "Title text")
```

![plot2_ex1](man/figures/plot2_ex1.png)

`plot2`, unlike `ggplot`, uses a wider range for the y axis (1.25 times maximum value) let it start at y = 0, both like Microsoft Excel. It also prints datalabels at default.

```r
# plot2 applies theme colours of Certe at default, but also understands others:
plot2(tbl1, colours = "certe")   # default
plot2(tbl1, colours = "rug")     # Rijksuniversiteit Groningen (University of Groningen)
plot2(tbl1, colours = "ggplot2") # default in ggplot2
plot2(tbl1, colours = "heatmap") # from grDevices::heat.colors()
plot2(tbl1, colours = "rainbow") # from grDevices::rainbow()
plot2(tbl1, colours = "viridis") # from viridis::viridis()

# And also understands most geoms of ggplot:
plot2(tbl1, type = "bar")             # same as plot2.bar(tbl1)
plot2(tbl1, type = "line")            # same as plot2.line(tbl1)
plot2(tbl1, type = "boxplot")         # same as plot2.boxplot(tbl1)
plot2(tbl1$value, type = 'histogram') # same as plot2.histogram(tbl1$value)
```

### Databases included in package
```r
# Dataset with ATC antibiotics codes, official names and DDD's (oral and parenteral)
ablist        # A tibble: 420 x 12

# Dataset with bacteria codes and properties like gram stain and aerobic/anaerobic
bactlist      # A tibble: 2,507 x 10

# Dataset with codons of all 20 essential amino acids
codonlist      # A tibble: 64 x 3
```

### Epidemiological functions
Two new S3 classes: `mic` for MIC values (e.g. from Vitek of Phoenix) and `rsi` for antimicrobial drug interpretations (i.e. S, I and R). Both are actually ordered factors under the hood (an MIC of `<=1` being lower than `2`, and for class `rsi` factors are ordered as `S < I < R`). 
Both classes have extensions for existing functions like `print`, `summary` and `plot`.
```r
# Transform values to new classes
mic_data <- as.mic(c(">=32", "1.0", "8", "<=0.128", "8", "16", "16"))
rsi_data <- as.rsi(c(rep("S", 474), rep("I", 36), rep("R", 370)))
```
Quick overviews when just printing objects:
```r
mic_data
# Class 'mic': 7 isolates
# 
# <NA>  0 
# Min.  <=0.128 
# Max.  >=32 
# List: <=0.128 1 8 16 >=32 

rsi_data
# Class 'rsi': 880 isolates
# 
# Unavailable: 0 
# Sum of S:    474 
# Sum of IR:   406 
# Sum of R:    370 
# Sum of I:    36 
# 
#   %S  %IR   %I   %R 
# 53.9 46.1  4.1 42.0 
```

A plot of `rsi_data`:
```r
plot(rsi_data)
```

![plot2_ex4](man/figures/plot2_ex4.png)

Other epidemiological functions:

```r
# Apply EUCAST Expert Rules v3.1 (latest) to antibiotic columns
interpretive_reading(...)

# Determine key antibiotic based on bacteria ID
key_antibiotics(...)
# Check if key antibiotics are equal
key_antibiotics_equal(...)

# Selection of first isolates of any patient
first_isolate(...)

# Calculate resistance levels of antibiotics
rsi(...)
# Predict resistance levels of antibiotics
rsi_predict(...)

# Get name of antibiotic by ATC code
abname(...)
abname("J01CR02", from = "atc", to = "umcg") # "AMCL"

# Calculate age of patients
age(...)
# Categorize patients age to age groups
age.group(...)
```

## How to get it?
This package is only available here on GitHub, but respects the [CRAN Repository Policy](https://cran.r-project.org/web/packages/policies.html).

*Installation commands:*
```r
library(devtools)
install_github("msberends/certedata")
```

*Working behind a proxy? Then use:*
```r
library(httr)
library(devtools)
set_config(use_proxy("yourproxydomain.com",
                     8080,
                     "username",
                     "password",
                     "any")) # change "any" to "basic" or "digest" if needed
install_github("msberends/certedata")
reset_config()
```

## Authors

  - [Berends MS](https://github.com/msberends)<sup>1,3</sup>, *package developer*
  - Meijer BC<sup>2</sup>, *contributor*
  - [Hassing EEA](https://github.com/erwinhassing)<sup>1</sup>, *contributor*
  - [Luz CF](https://github.com/ceefluz)<sup>3</sup>, *contributor*
  
<sup>1</sup> Department of Medical, Market and Innovation (MMI), Certe Medische diagnostiek & advies, Groningen, the Netherlands

<sup>2</sup> Department of Medical Microbiology (MMB), Certe Medische diagnostiek & advies, Groningen, the Netherlands

<sup>3</sup> Department of Medical Microbiology, University of Groningen, University Medical Center Groningen, Groningen, the Netherlands

## Copyright
[![License](man/figures/license.svg)](https://github.com/msberends/certedata/blob/master/LICENSE)
[![Copyright](man/figures/copyright.svg)](https://www.certe.nl)

Copyright holder and funder: Certe Medische diagnostiek & advies. 

This R package is licensed under the [GNU General Public License (GPL) v2.0](https://github.com/msberends/certedata/blob/master/LICENSE). In a nutshell, this means that this package:

![](man/figures/check.png) may be used for commercial purposes

![](man/figures/check.png) may be used for private purposes

![](man/figures/check.png) may be modified, although:

  - Modifications **must** be released under the same license when distributing the package
  - Changes made to the code **must** be documented

![](man/figures/check.png) may be distributed, although:

  - Source code **must** be made available when the package is distributed
  - A copy of the license and copyright notice **must** be included with the package.

![](man/figures/stop.png) comes with a LIMITATION of liability

![](man/figures/stop.png) comes with NO warranty

----

© 2018 Certe

[www.certe.nl](https://www.certe.nl)
