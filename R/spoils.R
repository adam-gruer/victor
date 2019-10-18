#' To the victor go the spoils.
#'
#' Request vector tile from Mapbox and receive a list
#' of simple feature data frames , one per layer (e.g. roads, water, places)
#'
#' @param tileset_id character a standard mapbox or custom tileset
#' @param zoom integer the zoom level for the request tile
#' in the
#' \href{https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames}{Slippy Map Tilenames}
#' specification. 1 = the world, 18 is on top of your house
#' @param longitude numeric left most point ( I think?) of tile in degrees.
#'  TODO accept projection native coords
#' @param latitude numeric top most point ( I think?) of tile in degrees.
#'  TODO accept projection native coords
#' @param crs the EPSG code for the coordinate reference system of your supplied
#' longitude and latitude. The returned simple features will be in the same crs
#'
#' @return a list of simple feature data frames ,
#'  one per layer (e.g. roads, water, places)
#' @export
#'
#' @examples
#' \dontrun{
#'     spoils()
#'  }
spoils <- function(tileset_id = "mapbox.mapbox-streets-v8",
                   zoom = 1,
                   longitude = 0,
                   latitude = 0,
                   crs = 4326) {
  # TODO convert long and lat in non degrees to degrees st_transform?
  tile_coords <- slippymath::lonlat_to_tilenum(longitude, latitude, zoom)
  x <- tile_coords$x
  y <- tile_coords$y
  vector_tile <- mapbox_api(tileset_id, zoom, x, y)$content
  protolite::read_mvt_sf(vector_tile, crs = crs, c(zoom, x, y))
}



# deg2num <- function(zoom, lon_deg, lat_deg){
#   lat_rad <-  deg2rad(lat_deg)
#   n <- 2.0^zoom
#
#   xtile <- as.integer((lon_deg + 180.0) / 360.0 * n)
#   ytile <- as.integer((1.0 - asinh(tan(lat_rad)) / pi) / 2.0 * n)
#   c(xtile = xtile, ytile = ytile)
# }
#
# rad2deg <- function(rad) {(rad * 180) / (pi)}
# deg2rad <- function(deg) {(deg * pi) / (180)}

