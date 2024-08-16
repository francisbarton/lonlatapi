get_ctry <- function(lon, lat) {
  countries <- boundr::bounds("ctry", country_filter = "GB", resolution = "BUC")

  pt <- create_sf_point(lon, lat)
  country <- countries |>
    sf::st_filter(pt, .predicate = sf::st_contains) |>
    dplyr::pull("ctry21nm")

  list(country = country, pt = pt)
}




get_rgn <- function(...) {
  args <- rlang::list2(...) |>
    purrr::list_flatten()
  country <- args[["country"]]
  if (country == "England") {
    regions <- boundr::bounds("rgn", resolution = "BUC")

    pt <- process_point(!!!args)

    region <- regions |>
      sf::st_filter(pt, .predicate = sf::st_contains) |>
      dplyr::pull("rgn21nm")
  } else {
    region <- country
  }

  list(region = region, pt = pt)
}




get_lad <- function(...) {
  args <- rlang::list2(...) |>
    purrr::list_flatten()
  region <- args[["region"]]

  # includes Scotland and Wales, though these are not regions!
  lads <- boundr::bounds("lad", "rgn", {{ region }}, resolution = "BUC")

  pt <- process_point(!!!args)
  lad <- lads |>
    sf::st_filter(pt, .predicate = sf::st_contains) |>
    dplyr::pull("lad23nm")

  list(lad = lad, pt = pt)
}



get_msoa <- function(...) {
  args <- rlang::list2(...) |>
    purrr::list_flatten()
  lad <- args[["lad"]]
  msoas <- boundr::bounds("msoa", "lad", {{ lad }}, resolution = "BFC")

  pt <- process_point(!!!args)
  msoa <- msoas |>
    sf::st_filter(pt, .predicate = sf::st_contains) |>
    dplyr::pull("msoa21nm")

  list(msoa = msoa, pt = pt)
}




get_lsoa <- function(...) {
  args <- rlang::list2(...) |>
    purrr::list_flatten()
  msoa <- args[["msoa"]]
  lsoas <- boundr::bounds("lsoa", "msoa", {{ msoa }}, resolution = "BFC")

  pt <- process_point(!!!args)
  lsoa <- lsoas |>
    sf::st_filter(pt, .predicate = sf::st_contains) |>
    dplyr::pull("lsoa21nm")

  list(lsoa = lsoa, pt = pt)
}




get_oa <- function(...) {
  args <- rlang::list2(...) |>
    purrr::list_flatten()
  lsoa <- args[["lsoa"]]
  oas <- boundr::bounds("oa", "lsoa", {{ lsoa }}, resolution = "BFC")

  pt <- process_point(!!!args)
  oa <- oas |>
    sf::st_filter(pt, .predicate = sf::st_contains) |>
    dplyr::pull("oa21cd")

  list(oa = oa, pt = pt)
}





process_point <- function(...) {
  args <- rlang::list2(...)
  lon <- args[["lon"]]
  lat <- args[["lat"]]
  pt <- args[["pt"]]

  if (is.null(lon) | is.null(lat)) {
    assertthat::assert_that(!is.null(pt), msg = "sf point object not supplied")
  }
  if (is.null(pt)) {
    assertthat::assert_that(!any(is.null(c(lon, lat))), msg = "lon and lat values not supplied")
    assertthat::assert_that(all(is.numeric(c(lon, lat))), msg = "lon and lat values not supplied as numeric values")
    pt <- create_sf_point(lon, lat)
  }

  pt_msg <- glue::glue(
    "process_point: An sfc point object has not been created."
  )
  assertthat::assert_that(inherits(pt, "sfc"), msg = pt_msg)

  pt
}





create_sf_point <- function(lon, lat) {
  sf::st_point(c(lon, lat)) |>
    sf::st_sfc(crs = 4326)
}
