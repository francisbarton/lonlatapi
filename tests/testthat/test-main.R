"get max names works" |>
  test_that({
    test_tbl <- tibble::tibble(
      col221 = sample(letters, 10),
      col222 = sample(letters, 10),
      col211 = sample(LETTERS, 10),
      col212 = sample(LETTERS, 10),
      num211 = rnorm(10),
      num221 = rnorm(10)
    )

    prefix <- "col"
    n <- 2

    nms <- grep(glue::glue("^{prefix}"), names(test_tbl), value = TRUE)
    expect_equal(nms, c("col221", "col222", "col211", "col212"))

    nms <- grep(glue::glue("^{prefix}"), names(test_tbl), value = TRUE) |>
      sort()
    expect_equal(nms, c("col211", "col212", "col221", "col222"))

    nms <- grep(glue::glue("^{prefix}"), names(test_tbl), value = TRUE) |>
      sort() |>
      tail(n)
    expect_equal(nms, c("col221", "col222"))
  })


"test how dots work" |>
  test_that({
    test_fn <- function(string, ...) {
      args <- rlang::dots_list(..., string = string, .homonyms = "first")
      args[["string"]]
    }

    out1 <- test_fn("yes")
    expect_equal(out1, "yes")

    out2 <- test_fn("yes", string = "but also")
    expect_equal(out2, "but also")

    test_fn <- function(string, ...) {
      args <- rlang::dots_list(..., string = string, .homonyms = "first")
      args[["string"]]
    }

    args_list <- list(string = "in a list?")
    out3 <- test_fn("yes", !!!args_list)
    expect_equal(out3, "in a list?")
  })

"dots test 2" |>
  test_that({
    lon <- -1.234
    lat <- 54.567

    ctry_out <- get_ctry(lon, lat)
    expect_type(ctry_out, "list")
    expect_length(ctry_out, 2)
  })

"dots test 3" |>
  test_that({
    # mimic/reconstruct get_rgn()
    test_fn <- function(...) {
      args <- rlang::list2(...) |>
        purrr::list_flatten()
      args[["country"]]
    }

    out1 <- test_fn(list(country = "England", blah = "blah"))
    expect_equal(out1, "England")
    # add superfluous argument outside list
    out2 <- test_fn(list(country = "England", blah = "blah"), other = "hmm")
    expect_equal(out2, "England")

    lon <- -1.234
    lat <- 54.567

    ctry_out <- get_ctry(lon, lat)
    rgn_out <- get_rgn(ctry_out)
    expect_type(rgn_out, "list")
    expect_length(rgn_out, 2)
  })

"dots test 4" |>
  test_that({
    lon <- -1.234
    lat <- 54.567

    rgn_out2 <- get_ctry(lon, lat) |>
      get_rgn()
    expect_type(rgn_out2, "list")
    expect_length(rgn_out2, 2)
    expect_equal(rgn_out2[["region"]], "North East")
    expect_type(rgn_out2[["pt"]], "list")
    expect_s3_class(rgn_out2[["pt"]], "sfc")
  })


# Originally get_rgn() and co. all returned a single string that could then be
# passed as the first argument to another function.
# However I then decided that once a pt had been created from a lat/lon pair,
# it would be better to also pass that to the next function in the chain.
# This avoids having to re-pass the lat and lon each time (as originally
# written) and supports composing a chain of functions that just take the
# `...` dots rather than requiring a string and a lat and lon to be supplied.
"handle get_* functions returning a list not a single string" |>
  test_that({
    t1 <- get_ctry(lon = -1.234, lat = 54.567)
    expect_equal(t1[["country"]], "England")
    t2 <- get_rgn(t1, lon = -1.234, lat = 54.567)
    expect_equal(t2[["region"]], "North East")

    # combine those two but still pass in lon and lat to get_rgn():
    rgn1 <- get_ctry(lon = -1.234, lat = 54.567) |>
      get_rgn(lon = -1.234, lat = 54.567)
    expect_equal(rgn1[["region"]], "North East")

    # try passing the `pt` argument via the dots,
    # which saves having to re-pass lon and lat:
    rgn2 <- get_ctry(lon = -1.234, lat = 54.567) |>
      get_rgn()
    expect_equal(rgn2[["region"]], "North East")

    # OK now purrr::compose should work, in theory...
    rgn3 <- purrr::compose(
      get_rgn, get_ctry
    )(lon = -1.234, lat = 54.567)
    expect_equal(rgn3[["region"]], "North East")
  })



"find_oa() works to return an OA code" |>
  test_that({
    oacd <- find_oa(lon = -1.234, lat = 54.567)
    expect_equal(oacd, "E00174111")
  })



"we make a nice table" |>
  test_that({
    oacd <- "E00174111"

    df1 <- get_oa_lookup(oacd, 3)
    expect_equal(nrow(df1), 1)
    expect_true(inherits(df1, "data.frame"))

    df2 <- get_oa_lookup(oacd, 8)
    df3 <- get_oa_lookup(oacd, 9)
    df4 <- get_oa_lookup(oacd, 1)
    df5 <- get_oa_lookup(oacd, 6)

    out <- list(
      df1, df2, df3, df4, df5
    ) |>
      purrr::reduce(dplyr::left_join)

    expect_equal(nrow(out), 1)
    expect_true(inherits(out, "data.frame"))
    # expect_equal(ncol(out), 39) # reinstate if helpful
  })


"we make a very tidy table" |>
  test_that({
    oacd <- "E00174111"

    df1 <- get_oa_lookup(oacd, 3)
    df2 <- get_oa_lookup(oacd, 8)
    df3 <- get_oa_lookup(oacd, 9)
    df4 <- get_oa_lookup(oacd, 1)
    df5 <- get_oa_lookup(oacd, 6)

    out <- list(
      df1, df2, df3, df4, df5
    ) |>
      purrr::reduce(dplyr::left_join)

    get_max_names <- function(tbl, prefix, n = 2) {
      nms <- grep(glue::glue("^{prefix}"), names(tbl), value = TRUE) |>
        sort() |>
        tail(n)
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

    expect_length(keep_names, 28)

    out2 <- out |>
      dplyr::select(any_of(keep_names)) |>
      dplyr::distinct()

    expect_equal(nrow(out2), 1)
    expect_true(inherits(out2, "data.frame"))
    expect_equal(ncol(out2), length(keep_names))
  })
