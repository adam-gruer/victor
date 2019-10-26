test_that("binding two tile's layer with differing attributes", {
  perth <-  list(longitude = 115.906105,latitude = -31.842274)
  au_west <- spoils(zoom = 4, longitude = perth$longitude, latitude = perth$latitude)

  sydney <- list(longitude = 151.131706,latitude = -33.805967)
  aus_se <- spoils(zoom = 4, longitude = sydney$longitude, latitude = sydney$latitude)


})
