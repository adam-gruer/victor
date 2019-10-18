
<!-- README.md is generated from README.Rmd. Please edit that file -->

# victor

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of victor is to interact with the [Mapbox Vector Tile
API](https://docs.mapbox.com/help/glossary/vector-tiles-api/) ( [also
here](https://docs.mapbox.com/api/maps/#vector-tiles) ) in R. Grab a
vector tile, work with it using the simple features package sf. Plot
static maps with ggplot2, tmap, etc.

A [Mapbox Access Token](link%20here) is required to access the API. See
these instructions … link to mapdeck instructions?

Inspired and fuelled by:

  - mapdeck
  - protolite
  - ceramic
  - slippymath
  - rmapzen
  - sf

## Installation

Install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("adam-gruer/victor")
```

## Example

Let’s create a map of Melburn and surrounding cities:

``` r
library(victor)
library(sf)
library(tidyverse)


melburn <- spoils(zoom = 7, longitude = 144.8430, latitude = -37.7311)

roads <- filter(melburn$road,!st_is(geometry, "POINT")) 
road_shields <-  filter(melburn$road,
                        st_is(geometry, "POINT"),
                        str_starts(ref, "M") ) %>% 
                  group_by(ref) %>% 
                  filter(row_number() == 1)
places <- filter(melburn$place_label, symbolrank < 11)

ggplot() +
  geom_sf(data = melburn$water, fill = "lightblue") +
  geom_sf_label(aes(label = ref), data = road_shields) +
  geom_sf(aes(colour = class), data = roads) +
  geom_sf_label(aes(label = name, size = symbolrank),
                data = places) +
  scale_size(trans = "reverse", range = c(4,6)) +
  theme_void() +
  theme( panel.grid.major = element_line(size = 0),
         plot.background = element_rect(fill = "antiquewhite"),
         legend.position = "none") 
```

<img src="man/figures/README-example-1.png" width="100%" />
