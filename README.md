
# lonlatapi

<!-- badges: start -->
<!-- badges: end -->

**{lonlatapi}** finds OA (Census output area) data from longitude and latitude data.

It's really slow because it downloads boundary data at every step.
It would be a lot quicker if it used the local boundary data that a package like `{geographr}` provides.
But this package would need to be written quite differently.

`{lonlatapi}` depends on [boundr](https://github.com/francisbarton/boundr).

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

## Further development

Currently `lonlatapi` only works for a single lonlat supplied.
I would like to make it so it could handle a list of lonlats, and efficiently re-use any downloaded boundary data within that list.
