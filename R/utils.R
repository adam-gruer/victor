lon_wrap_180 <- function(sf) {

  #store original crs
  original_crs <- sf::st_crs(sf)

  # transform to projection that can handle crossing the dateline
  sf <- lwgeom::st_transform_proj(sf,"+proj=longlat +datum=WGS84 +lon_wrap=180")

  # transform back to original projection
  sf <- lwgeom::st_transform_proj(sf, original_crs)

  # set crs to original.  st_transform_proj doen't set EPSG
  st_crs(sf) <- original_crs
  sf
  }

is_anti_meridian_tile <- function(x, y, zoom){
  t_bbox <- slippymath::tile_bbox(x,y,zoom)
  t_bbox <- matrix(t_bbox, nrow = 2, byrow = TRUE)
  t_bbox <- slippymath::merc_to_lonlat(t_bbox)
  any(round(t_bbox[,1],10) %in% c(180.0, -180.0))
}


