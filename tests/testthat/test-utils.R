

sf_dateline <- sf::st_polygon(list(matrix(c(179, -43,
                                 -185, -43,
                                 -181, -44,
                                 179, -43),
                          ncol = 2, byrow = TRUE)))

sf_dateline <- sf::st_sfc(sf_dateline, crs = 4326)

test_that("lon_wrap_180 returns object with original crs", {
  expect_equal(sf::st_crs(sf_dateline),
               sf::st_crs(lon_wrap_180(sf_dateline)))

  sf_dateline <- sf::st_transform(sf_dateline, 3111)
  expect_equal(sf::st_crs(sf_dateline),
               sf::st_crs(lon_wrap_180(sf_dateline)))
})

test_that("east_most_tile_x returns correct value for all zoom levels", {
  zoom <- 1:18
  expect_equal(purrr::map_dbl(zoom, east_most_tile_x),
              2^(1:18)  - 1 )
})

test_that("east_most_tile_x returns error for incorrect zoom levels", {
  expect_error(east_most_tile_x(0))
  expect_error(east_most_tile_x(19))
  expect_error(east_most_tile_x(TRUE))
  expect_error(east_most_tile_x("2"))
  expect_error(east_most_tile_x(NA))
})
