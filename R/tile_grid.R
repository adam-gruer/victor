#' Generate a grid of nine tiles the top left of centre tile is
#' the supplied longitude and latitude
#'
#' @param zoom integer required zoom level 1- 17
#' @param bbox the bounding box to fit onto a grid of tiles. Must be either a
#'   'bbox' object created with sf::st_bbox or a vector of length 4 with names:
#'   'xmin', 'xmax', 'ymin', 'ymax'.
#'
#' @return a list of simple features
#' @export
#'
#' @examples
tile_grid <- function(bbox, zoom) {
  slippymath::bbox_to_tile_grid(bbox, zoom)
}
