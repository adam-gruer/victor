lon_wrap_180 <- function(sf) {
  sf <- sf::as_Spatial(sf)
  sf <- sp::spTransform(sf,"+proj=longlat +datum=WGS84 +lon_wrap=180")
  sf::st_as_sf(sf)
}

is_anti_meridian_tile <- function(x, y, zoom){
  t_bbox <- slippymath::tile_bbox(x,y,zoom)
  t_bbox <- matrix(t_bbox, nrow = 2, byrow = TRUE)
  t_bbox <- slippymath::merc_to_lonlat(t_bbox)
  any(round(t_bbox[,1],10) %in% c(180.0, -180.0))
}


