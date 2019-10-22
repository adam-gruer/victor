lon_wrap_180 <- function(sf) {
  sf <- sf::as_Spatial(sf)
  sf <- sp::spTransform(sf,"+proj=longlat +datum=WGS84 +lon_wrap=180")
  sf::st_as_sf(sf)
}
