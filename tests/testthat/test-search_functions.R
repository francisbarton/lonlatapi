"country query returns a tibble" |>
  test_that({
    countries <- boundr::bounds("ctry", country_filter = "GB", resolution = "BUC")
    expect_s3_class(countries, "sf")
    expect_s3_class(countries, "data.frame")
    expect_equal(nrow(countries), 4) # huh? should be 3 in GB?
  })


"sf_point makes sense" |>
  test_that({
    lon <- -1.345
    lat <- 54.678
    pt <- sf::st_point(c(lon, lat))
    expect_s3_class(pt, "sfg")
    expect_length(pt, 2)
  })



"st_contains works" |>
  test_that({
    countries <- boundr::bounds("ctry", country_filter = "GB", resolution = "BUC")

    # just some random point (oh hello Middlesbrough!)
    lon <- -1.234
    lat <- 54.567

    pt <- sf::st_point(c(lon, lat)) |>
      sf::st_sfc(crs = 4326)
    expect_s3_class(pt, "sfc")

    expect_in("geometry", names(countries))
    expect_in("ctry21nm", names(countries))

    country_list <- countries |>
      tidyr::nest(.by = "ctry21nm") |>
      dplyr::pull("data", name = "ctry21nm")

    expect_length(country_list, nrow(countries))
    expect_type(country_list, "list")
    expect_named(country_list)

    check <- country_list |>
      purrr::map_lgl(\(x) sf::st_contains(x, pt, sparse = FALSE)[1, 1])

    expect_vector(check, logical(), length(country_list))

    check_cont <- \(x, pt) sf::st_contains(x, pt, sparse = FALSE)[1, 1]

    check_tbl <- countries |>
      dplyr::mutate(contains_pt = check_cont(geometry, pt))

    expect_vector(check_tbl[["contains_pt"]], logical(), length(country_list))
    expect_length(check_tbl, ncol(countries) + 1)

    filter_tbl <- countries |>
      sf::st_filter(pt, .predicate = sf::st_contains)

    expect_equal(nrow(filter_tbl), 1)

    filter_list <- country_list |>
      purrr::keep(\(x) check_cont(x, pt))

    expect_length(filter_list, 1)

    ctry_name <- filter_tbl |>
      dplyr::pull(ctry21nm)

    expect_vector(ctry_name, character(), 1)

    # test that it hasn't brought its geometry with it
    expect_identical(ctry_name, sf::st_drop_geometry(ctry_name))

    expect_equal(ctry_name, "England")
  })


"country query returns a name" |>
  test_that({
    # just some random point (oh hello Middlesbrough!)
    lon <- -1.234
    lat <- 54.567

    ctry <- get_ctry(lon, lat)[["country"]]
    expect_equal(ctry, "England")
  })


"region query returns a name" |>
  test_that({
    # just some random point (oh hello Middlesbrough!)
    lon <- -1.234
    lat <- 54.567

    regions <- boundr::bounds("rgn", resolution = "BUC")

    pt <- create_sf_point(lon, lat)
    # only 1 region should contain the point
    rgn_row <- regions |>
      sf::st_filter(pt, .predicate = sf::st_contains)
    expect_equal(nrow(rgn_row), 1)
    expect_s3_class(rgn_row, "sf")

    rgn_nm <- rgn_row |>
      dplyr::pull(rgn21nm)

    expect_equal(rgn_nm, "North East")
  })



"lad query returns a name" |>
  test_that({
    lon <- -1.234
    lat <- 54.567

    lad_nm <- get_lad(region = "North East", lon, lat) |>
      purrr::pluck("lad")

    expect_equal(lad_nm, "Middlesbrough")
  })



"msoa query returns a name" |>
  test_that({
    lon <- -1.234
    lat <- 54.567

    msoa_nm <- get_msoa("Middlesbrough", lon, lat) |>
      purrr::pluck("msoa")

    expect_equal(msoa_nm, "Middlesbrough 001")
  })



"lsoa query returns a name" |>
  test_that({
    lon <- -1.234
    lat <- 54.567

    lsoa_nm <- get_lsoa("Middlesbrough 001", lon, lat) |>
      purrr::pluck("lsoa")

    expect_equal(lsoa_nm, "Middlesbrough 001D")
  })




"oa query returns a code" |>
  test_that({
    lon <- -1.234
    lat <- 54.567

    oacd <- get_oa("Middlesbrough 001D", lon, lat) |>
      purrr::pluck("oa")

    expect_equal(oacd, "E00174111") # turns out
  })
