#' Build a table of geographical data for an OA
#'
#' Pass in either an OA code or a pair of longitude and latitude values
#'
#' @param oacd character. The ONS code for a Census OA (output area). For
#'  example, "E00174111"
#' @inheritParams find_oa
#' @returns A tibble with 28 (expected) columns giving wider geographical data
#'  for the selected location
#' @export
get_full_oa_data <- function(oacd = NULL, lon = NULL, lat = NULL) {
  if (is.null(oacd)) oacd <- find_oa(lon, lat)

  out <- list(
    get_oa_lookup(oacd, 3),
    get_oa_lookup(oacd, 8),
    get_oa_lookup(oacd, 9),
    get_oa_lookup(oacd, 1),
    get_oa_lookup(oacd, 6)
  ) |>
    purrr::reduce(dplyr::left_join)

  get_max_names <- function(tbl, prefix, n = 2) {
    nms <- grep(glue::glue("^{prefix}"), names(tbl), value = TRUE) |>
      sort() |>
      utils::tail(n)
  }

  keep_names <- c(
    get_max_names(out, "oa", 1),
    get_max_names(out, "lsoa"),
    get_max_names(out, "msoa"),
    get_max_names(out, "wd"),
    get_max_names(out, "lad"),
    get_max_names(out, "ltla"),
    get_max_names(out, "utla"),
    get_max_names(out, "pcon"),
    get_max_names(out, "sicbl", 3),
    get_max_names(out, "icb", 3),
    get_max_names(out, "nhser", 3),
    get_max_names(out, "rgn"),
    get_max_names(out, "ctry")
  )

  out |>
    dplyr::select(any_of(keep_names)) |>
    dplyr::distinct()
}



#' Find a Census OA (output area) from longitude and latitude values
#'
#' @param lon numeric. A longitude (east-west) value, for example `-1.234`
#' @param lat numeric. A latitude (south-north) value, for example `54.567`
#' @returns The ONS code for a Census OA (output area) as a character string.
#'  For example, "E00174111"
#' @export
find_oa <- function(lon, lat) {
  pluck_oa <- function(x) purrr::pluck(x, "oa")
  purrr::compose(
    pluck_oa, get_oa, get_lsoa, get_msoa, get_lad, get_rgn, get_ctry
  )(lon, lat)
}




get_oa_lookup <- function(...) {
  purrr::partial(
    boundr::create_lookup_table,
    lookup = "oa",
    within = "oa",
    within_names = NULL,
    # within_codes    # supplied in `...`
    return_width = "full",
    lookup_year = NULL,
    within_year = NULL,
    country_filter = "UK|GB|EW|EN|SC|WA",
    # option          # supplied in `...`
    chatty = FALSE
  )(...)
}
