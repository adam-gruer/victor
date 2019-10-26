#' Wrap coordinates that are on the anti-meridian (date line)
#'
#' features in tiles that are the east most for a zoom level
#' (x is 2^(zoom) - 1) or the west most (x is 0) have longitudes
#' that go from positive to negative (e.g 179 to -182). This function
#' converts to a projection (+proj=longlat +datum=WGS84 +lon_wrap=180) that
#' wraps these longitudes and then converts back to the original projection
#'
#' @param sf object
#'
#' @return a sf object with the same  projection as supplied
#'
#' @examples
lon_wrap_180 <- function(sf) {

  #store original crs
  original_crs <- sf::st_crs(sf)

  # transform to projection that can handle crossing the dateline
  sf <- lwgeom::st_transform_proj(sf,"+proj=longlat +datum=WGS84 +lon_wrap=180")

  # transform back to original projection
  sf <- lwgeom::st_transform_proj(sf, original_crs)

  # set crs to original.  st_transform_proj doen't set EPSG
  sf::st_crs(sf) <- original_crs
  sf
}

#' Get the x index of the east most tile for a zoom level
#'
#' @param zoom an integer in the range 1:18
#'
#' @return
#'
#' @examples
east_most_tile_x <- function(zoom){
  if(any(
         !is.numeric(zoom),
         !zoom %in% 1:18)
         ) abort("incorrect zoom must be a whole number between 1 and 18 inclusive")

  (2^zoom) - 1

}

is_east_most_tile <- function(tilenum){
  if(tilenum$zoom == 1) return(FALSE)
  tilenum$x == east_most_tile_x(tilenum$zoom)
}

is_west_most_tile <- function(tilenum){
  if(tilenum$zoom == 1) return(FALSE)
  tilenum$x == 0
}

is_anti_meridian_tile <- function(tilenum){
  is_west_most_tile(tilenum) ||
      is_east_most_tile(tilenum)
}


