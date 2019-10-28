#' To the victor go the spoils.
#'
#' Request vector tile from Mapbox and receive a list
#' of simple feature data frames , one per layer (e.g. roads, water, places)
#'
#' @param longitude numeric left most point ( I think?) of tile in degrees.
#'  TODO accept projection native coords
#' @param latitude numeric top most point ( I think?) of tile in degrees.
#'  TODO accept projection native coords
#' @param zoom integer the zoom level for the request tile
#' in the
#' \href{https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames}{Slippy Map Tilenames}
#' specification. 1 = the world, 18 is on top of your house
#' @param nrows integer how many rows of tiles
#' @param ncols integer how many coumns of tiles
#' @param crs the EPSG code for the coordinate reference system of your supplied
#' longitude and latitude. The returned simple features will be in the same crs
#' @param tileset_id character a standard mapbox or custom tileset
#'
#' @return a list of simple feature data frames ,
#'  one per layer (e.g. roads, water, places)
#' @export
#'
#' @examples
#' \dontrun{
#'     spoils()
#'  }
spoils <- function(longitude = 0,
                   latitude = 0,
                   zoom = 1,
                   nrow  = 1,
                   ncol = 1,
                   crs = 4326,
                   tileset_id = "mapbox.mapbox-streets-v8") {

  # TODO convert long and lat in non degrees to degrees st_transform?
  tilenum <- slippymath::lonlat_to_tilenum(longitude, latitude, zoom)
  tilenum$zoom <- zoom


  tilenums <- tile_list(tilenum, ncol, nrow)

  vector_tiles <-  purrr::map(tilenums, mapbox_api, tileset_id = tileset_id)
  vector_tiles <- purrr::map(vector_tiles,"content")

  vector_tiles <- purrr::map2(vector_tiles,
                   tilenums,
                   function(data, tilenum, crs){
                   vector_tile <- protolite::read_mvt_sf(
                            data = data,
                             crs = crs,
                             zxy = c(tilenum$zoom,
                                   tilenum$x,
                                   tilenum$y))
                   if(is_anti_meridian_tile(tilenum)){
                     purrr::map(vector_tile, lon_wrap_180)
                   } else {
                     vector_tile
                   }
                   },
                   crs = crs)
  stitch(vector_tiles)
  # vector_tile <- protolite::read_mvt_sf(vector_tile,
  #                                        crs = crs,
  #                                        zxy = c(tilenum$zoom,
  #                                                tilenum$x,
  #                                                tilenum$y))


}



tile_list <- function(tilenum, ncol, nrow ){

  if(ncol == 1 && nrow == 1) return(tilenum)

  x <- rep(tilenum$x + 0:(ncol-1), each = nrow) - round(median(1:ncol),0)
  y <- rep(tilenum$y + 0:(nrow-1), times = ncol) - round(median(1:nrow),0)
  purrr::map2(x, y,
              function(x, y, zoom){
                list(x = x, y = y, zoom = tilenum$zoom)
              },
              zoom = zoom)

}
