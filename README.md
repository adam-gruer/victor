
<!-- README.md is generated from README.Rmd. Please edit that file -->

# victor

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of victor is to interact with the [Mapbox Vector Tile
API](https://docs.mapbox.com/help/glossary/vector-tiles-api/) ( [also
here](https://docs.mapbox.com/api/maps/#vector-tiles) ) in R. Grab a
vector tile(s), work with it using the simple features package sf. Plot
static maps with ggplot2, tmap, etc.

Inspired and/or fuelled by:

  - [protolite](https://github.com/jeroen/protolite)
  - [slippymath](https://github.com/MilesMcBain/slippymath)
  - [ceramic](https://github.com/hypertidy/ceramic)
  - [mapdeck](https://github.com/SymbolixAU/mapdeck)
  - [rmapzen](https://github.com/tarakc02/rmapzen)
  - [sf](https://github.com/r-spatial/sf)

## Installation

Install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("adam-gruer/victor")
```

## Mapbox Token

You will need a [Mapbox Access
Token](https://docs.mapbox.com/help/how-mapbox-works/access-tokens/) to
use their API. Once you have a token add a line to your .Renviron file

    MAPBOX_API_KEY=your_acess_token

The easiest way to edit .Renviron is with the usethis pavkage
`usethis::edit_r_environ()`. You will have to restart R after editing
the file for the token to be available.

## Example

Let’s create a map of Melburn and surrounding cities:

Use the spoils function to retrieve a tile for a given location and zoom
It returns a list of simple features data frames. One for each layer of
data provided by mapbox.
<!-- To view a summary of available layers , call summary(). -->

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
places <- filter(melburn$place_label, symbolrank < 11) %>% 
  mutate(name = case_when(name == "Melbourne" ~ "Melburn",
                          TRUE ~ as.character(name)))

ggplot() +
  geom_sf(data = melburn$water, fill = "lightblue") +
  geom_sf(aes(colour = class), data = roads) +
  geom_sf_label(aes(label = ref), data = road_shields) +
  
  geom_sf_label(aes(label = name, size = symbolrank),
                data = places) +
  scale_size(trans = "reverse", range = c(4,6)) +
  theme_void() +
  theme( panel.grid.major = element_line(size = 0),
         plot.background = element_rect(fill = "antiquewhite"),
         legend.position = "none") 
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

Melburn CBD , zoomed in

``` r

cbd <- spoils(zoom = 15, long = 144.958869, lat =-37.820318)

ggplot() + geom_sf(data = cbd$water, fill = "lightblue") +
  geom_sf(data = cbd$building, aes(fill = type )) +
  geom_sf(data = cbd$road) +
  geom_sf_label(data = cbd$natural_label,
                aes (label = name),
                alpha = 0.4,
                nudge_x = 0.002) +
  geom_sf_label(data = filter(cbd$poi_label, 
                              category_en == "Aquarium"), 
                aes(label = name),
                alpha = 0.4) +
    theme_void() +
  theme( panel.grid.major = element_line(size = 0),
         plot.background = element_rect(fill = "antiquewhite1")) 
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />
