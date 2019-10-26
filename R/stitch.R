

stitch <- function(tiles){
  layer_names <- union_tile_layers(tiles)

 layers <- purrr::map(layer_names, function(layer_name){

   purrr::map(tiles, layer_name) %>%
     purrr::reduce(bind_tiles_layer)

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

union_tile_layers <- function(tiles) {
  purrr::map(tiles, names) %>%
    purrr::reduce(function(names_a,names_b) union(names_a,names_b))
}

layer_fields <- function(tile, layers) {

  map2_chr(layers, function(layer,tile ){

  }, tile = tile)
}

bind_tiles_layer <- function(tile_a_layer, tile_b_layer = NULL){
  if(is.null(tile_b_layer)) return(tile_a_layer)
  fields_a <- names(tile_a_layer)
  fields_b <- names(tile_b_layer)

  not_in_b <- setdiff(fields_a, fields_b)
  not_in_a <- setdiff(fields_b, fields_a )

  if(length(not_in_b) > 0){
  purrr::walk(not_in_b, function(field){
    tile_b_layer[[field]] <<- NA
  })
  }

  if(length(not_in_a) > 0){
  purrr::walk(not_in_a, function(field){
    tile_a_layer[[field]] <<- NA
  })
  }

  rbind(tile_a_layer, tile_b_layer[, names(tile_a_layer)])
}


