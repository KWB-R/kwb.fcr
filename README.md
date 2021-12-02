[![R-CMD-check](https://github.com/KWB-R/kwb.fcr/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/kwb.fcr/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/kwb.fcr/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/kwb.fcr/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/kwb.fcr/branch/main/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.fcr)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.fcr)]()
[![R-Universe_Status_Badge](https://kwb-r.r-universe.dev/badges/kwb.fcr)](https://kwb-r.r-universe.dev/)

# kwb.fcr
Fertilizer chemical risk assessment: This risk assessment is based on the TGD on risk assessment by the 
  European Comission. Possible Endpoints are soil organisms, groundwater
  organisms and human health via food consumption.Every variable can be entered 
  as probability distribution to include uncertainties or site specific
  variations.

## Installation

For details on how to install KWB-R packages checkout our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).

```r
### Optionally: specify GitHub Personal Access Token (GITHUB_PAT)
### See here why this might be important for you:
### https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat

# Sys.setenv(GITHUB_PAT = "mysecret_access_token")

# Install package "remotes" from CRAN
if (! require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

# Install KWB package 'kwb.fcr' from GitHub
remotes::install_github("KWB-R/kwb.fcr")
```

## Documentation

Release: [https://kwb-r.github.io/kwb.fcr](https://kwb-r.github.io/kwb.fcr)

Development: [https://kwb-r.github.io/kwb.fcr/dev](https://kwb-r.github.io/kwb.fcr/dev)
