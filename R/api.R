
mapbox_api <- function(tileset_id = "mapbox.mapbox-streets-v8",
                       tilenum = list(zoom = 1,
                       x = 0,
                       y = 0)) {
  # TODO 2X and style parameters /v4/{tileset_id}/{zoom}/{x}/{y}{@2x}.{format}
  # https://docs.mapbox.com/api/maps/#vector-tiles

  # build request
  # TODO argument checking
  # TODO access toke checking
  mapbox_access_token <- Sys.getenv("MAPBOX_API_KEY")

  api_version <- "v4"
  y.mvt <- glue::glue(tilenum$y, ".mvt")
  query_str <- glue::glue("access_token={mapbox_access_token}")

  url <- httr::modify_url("https://api.mapbox.com",
    path = c(api_version, tileset_id, tilenum$zoom, tilenum$x, y.mvt),
    query = query_str
  )

  # make request
  ua <- httr::user_agent("https://github.com/adam-gruer/victor")
  resp <- httr::GET(url, ua)

  # handle response
  if (httr::http_type(resp) != "application/vnd.mapbox-vector-tile") {
    stop("API did not return vnd.mapbox-vector-tile", call. = FALSE)
  }

  if (httr::http_error(resp)) {
    stop(
      sprintf(
        "Mapbox Vector Tiles API request failed [%s]\n",
        httr::status_code(resp)
      ),
      call. = FALSE
    )
  }

  # construct Value
  structure(
    list(
      content = httr::content(resp),
      path = glue::glue(tileset_id, tilenum$zoom, tilenum$x, tilenum$y),
      response = resp
    ),
    class = "mapbox_vector_tile"
  )
}



# print.mapbox_api <- function(x, ...) {
#   cat("<Mapbox Vector Tiles ", x$path, ">\n", sep = "")
#   utils::str(x$content)
#   invisible(x)
# }


# single_character <- function(character) {
#   api_str <- paste0("/api/character/",character)
#   content <- goodplacequotes_api(api_str)$content
#   content <- sample(content, 1)[[1]]
#   structure(content, class="goodshirt")
#
# }
