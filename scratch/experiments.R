library(tidyverse)
library(sf)
library(slippymath)

#new zealand zoom 4 ------- features crossing anti-meridian (+180,-180)

slippymath::lonlat_to_tilenum(172.078171,-42.576470, 4)

nz <- victor:::mapbox_api(zoom = 4, x = 14, y = 9)
nz <- protolite::read_mvt_sf(nz$content, zxy = c(4, 0, 9))

water <- nz$water
water

t2 <-  lwgeom::st_transform_proj(water,"+proj=longlat +datum=WGS84 +lon_wrap=180" ) %>%
  lwgeom::st_transform_proj(4326)

t1 <- victor:::mapbox_api(tilenum = list(zoom = 4, x = 14, y = 9))
t1 <- protolite::read_mvt_sf(t1$content, zxy = c(4, 14, 9))
t1 <- t1$water

t2 <- victor:::mapbox_api(tilenum = list(zoom = 4, x = 15, y = 9))
t2 <- protolite::read_mvt_sf(t2$content, zxy = c(4, 15, 9))
t2 <- t2$water
t2 <-  lwgeom::st_transform_proj(t2,"+proj=longlat +datum=WGS84 +lon_wrap=180" ) %>%
  lwgeom::st_transform_proj(4326)

t3 <- victor:::mapbox_api(tilenum = list(zoom = 4, x = 14, y = 10))
t3 <- protolite::read_mvt_sf(t3$content, zxy = c(4, 14, 10))
t3 <- t3$water

t4 <- victor:::mapbox_api(tilenum = list(zoom = 4, x = 15, y = 10))
t4  <- protolite::read_mvt_sf(t4$content, zxy = c(4, 15, 10))
t4 <- t4$water
t4 <-  lwgeom::st_transform_proj(t4,"+proj=longlat +datum=WGS84 +lon_wrap=180" ) %>%
  lwgeom::st_transform_proj(4326)
lwgeom::st_transform_proj(t4, 4326)
st_crs
t4


stitched <- rbind(t1, t2) %>% rbind(t3) %>% rbind(t4)

ggplot() + geom_sf(data = stitched, fill = "pink", colour = NA)
water <- victor:::lon_wrap_180(water)
ggplot() + geom_sf(data = water)

# test
st_bbox(water)[c("xmin", "xmax")]
t_bbox <- slippymath::tile_bbox(0,10,4)
t_bbox <- matrix(t_bbox, nrow = 2, byrow = TRUE)
t_bbox <- merc_to_lonlat(t_bbox)
any(round(t_bbox[,1],10) %in% c(180.0, -180.0))

tile_bbox <- function(zoom) {
  t_bbox <- slippymath::tile_bbox(0,10,4)
  t_bbox <- matrix(t_bbox, nrow = 2, byrow = TRUE)
  t_bbox <- merc_to_lonlat(t_bbox)
}

showMethods("spTransform")
rgdal:::spTransform.SpatialLinesDataFrame

sf <- sp::spTransform(sf,"+proj=longlat +datum=WGS84 +lon_wrap=180")
nz$water
water <- nz$water
water
lwgeom::st_transform_proj(water,"+proj=longlat +datum=WGS84 +lon_wrap=180" )
sf <- sp::spTransform(sf,"+proj=longlat +datum=WGS84 +lon_wrap=180")

t_bbox[,1] == c(-180.0, -157.5)
c(180.0, 157) %in% c(180.0, -180.0)

#stitching----------


grid_9 <- matrix(rep(c(14,9),each = 9), ncol = 2)  +
  matrix(c(-1,-1,
           0,-1,
           1, -1,
           -1, 0,
           0,0,
           1,0,
           -1,1,
           0,1,
           1,1),
         ncol = 2 , byrow = TRUE)
grid_9 <- rbind(grid_9, matrix(c(0,9, 0,10), ncol = 2, byrow = TRUE))
x <- grid_9[,1]
y <- grid_9[,2]

zoom = rep(4, length(x))
crs = rep(4326, length(x))


tiles <- stitch(zoom, x, y, crs)

water <- map(tiles,"water")
water <- reduce(water, rbind)
rbind(water[[6]], water[[7]])
%>%
  Reduce(rbind, .)
reduce(tiles$water, rbind)
water

ggplot() +
  geom_sf(data = water[c(1,2,4,5,7,8,10, 11),],
          fill = "lightblue",
          colour = NA)





