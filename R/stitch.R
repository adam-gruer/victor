

stitch <- function(tiles){
  layer_names <- purrr::reduce(tiles, intersect_tile_layers)
  #names(layer_names) <- layer_names

 layers <- map(layer_names, function(layer_name){

   reduce(tiles, function(tile_a, tile_b){
     rbind(tile_a[[layer_name]], tile_b[[layer_name]])
   })

  })
 names(layers) <- layer_names
 layers



 # fields <-  purrr::map_chr(tiles,layer_fields, layers = layers)
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

intersect_tile_layers <- function(tile_a, tile_b) {
  intersect(names(tile_a), names(tile_b))
}

layer_fields <- function(tile, layers) {

  map2_chr(layers, function(layer,tile ){

  }, tile = tile)
}


