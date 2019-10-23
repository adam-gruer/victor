library(sf)

sf_dateline <- st_polygon(list(matrix(c(179, -43,
                                 -185, -43,
                                 -181, -44,
                                 179, -43),
                          ncol = 2, byrow = TRUE)))

sf_dateline <- st_sfc(sf_dateline, crs = 4326)

test_that("lon_wrap_180 returns object with original crs", {
  expect_equal(st_crs(sf_dateline),
               st_crs(lon_wrap_180(sf_dateline)))

  sf_dateline <- st_transform(sf_dateline, 3111)
  expect_equal(st_crs(sf_dateline),
               st_crs(lon_wrap_180(sf_dateline)))
})


