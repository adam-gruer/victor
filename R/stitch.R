

stitch <- function(tiles){
  purrr::map(tiles, names_df)
  # t_tiles <- transpose(tiles)
  # layers <- map(t_tiles, function(layer){
  #     nms <- map(layer, names)
  #
  # } )
  # rbind(t_tiles$admin[[1]], NULL)



  #layer_names <- unique(map(tiles, names))[[1]]
  # layer_names <-  map(tiles, names)
  # layer_names <- Reduce(intersect, layer_names)
  #
  # layers <- map(layer_names, function(layer_name){
  #   assign(layer_name, map(tiles, layer_name) )
  #   nms <- map(eval(sym(layer_name)), names)
  #   nms <- Reduce(intersect, nms)
  #   assign(layer_name, Reduce(function(a,b){
  #     rbind(a, b[nms])
  #   }, x = eval(sym(layer_name))))
  # })
  # names(layers) <- layer_names
  # layers
}

layers <- function(tile_df) {
  names(tile_df)
}



