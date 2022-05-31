[![R-CMD-check](https://github.com/KWB-R/kwb.fcr/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/kwb.fcr/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/kwb.fcr/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/kwb.fcr/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/kwb.fcr/branch/main/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.fcr)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.fcr)]()
[![R-Universe_Status_Badge](https://kwb-r.r-universe.dev/badges/kwb.fcr)](https://kwb-r.r-universe.dev/)

This risk assessment is based on the TGD on risk assessment by the 
European Commission. Possible Endpoints are soil organisms, groundwater
organisms and human health via food consumption.Every variable can be entered 
as probability distribution to include uncertainties or site specific
variations.

## Installation

For installing the latest release of this R package run the following code below:

```r
# Enable repository from kwb-r
options(repos = c(
  kwbr = 'https://kwb-r.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))

# Download and install kwb.fcr in R
install.packages('kwb.fcr')

# Browse the kwb.fcr manual pages
help(package = 'kwb.fcr')
```

## Usage 

Checkout the [Documentation](articles/documentation.html) article on how to use 
this R package.
