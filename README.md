
# lonlatapi

<!-- badges: start -->
<!-- badges: end -->

**{lonlatapi}** finds OA (Census output area) data from longitude and latitude data

## Installation

You can install the development version of lonlatapi like so:

``` r
devtools::install_github("francisbarton/lonlatapi")

```

## Examples


``` r
library(lonlatapi)

find_oa(lon = -1.234, lat = 54.567)
get_full_oa_data(lon = -1.234, lat = 54.567)

```

``` r
oacd <- find_oa(lon = -1.234, lat = 54.567)
get_full_oa_data(oacd)

```
