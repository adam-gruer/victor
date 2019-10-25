library(tidyverse)
library(sf)
library(slippymath)

#new zealand zoom 4 ------- features crossing anti-meridian (+180,-180)

slippymath::lonlat_to_tilenum(172.078171,-42.576470, 4)
world <- victor::mapbox_api()
nz_sth <- victor:::mapbox_api(tilenum = list(zoom = 4, x = 15, y = 10))
nz_sth <- protolite::read_mvt_sf(nz_sth$content, zxy = c(4, 15, 10))
ggplot(nz_sth$water) + geom_sf()
ggplot(nz_sth$water %>% lon_wrap_180()) + geom_sf()

nz_nth <- victor:::mapbox_api(tilenum = list(zoom = 4, x = 15, y = 9))
nz_nth <- protolite::read_mvt_sf(nz_nth$content, zxy = c(4, 15, 9))
ggplot(nz_nth$water) + geom_sf()

ggplot(nz_nth$water %>% lon_wrap_180()) + geom_sf()

ggplot(rbind(nz_nth$water %>% lon_wrap_180(),
           nz_sth$water %>% lon_wrap_180())) +
  geom_sf()

nz <- list(nz_nth, nz_sth)
 ggplot() +
   geom_sf(data = reduce(map(nz,"admin")  %>% map(lon_wrap_180), rbind)) +
   geom_sf(data = reduce(map(nz,"water") %>% map(lon_wrap_180), rbind))  +
   geom_sf(data = reduce(map(nz,"place_label") %>% map(lon_wrap_180), rbind))


stitched <- stitch(nz)

auckland <- list(longitude = 174.859085, latitude = -36.678057 )
christchurch <- list(longitude = 172.639503, latitude = -43.537598)

auckland_tile <- slippymath::lonlat_to_tilenum(auckland$longitude,
                                               auckland$latitude,
                                               zoom = 4)
auckland_tile[["zoom"]] <- 4
unlist(auckland_tile)

christchurch_tile <- slippymath::lonlat_to_tilenum(christchurch$longitude,
                                               christchurch$latitude,
                                               zoom = 4)
christchurch_tile[["zoom"]] <- 4
unlist(christchurch_tile)

is_anti_meridian_tile(auckland_tile)
is_anti_meridian_tile(christchurch_tile)

nth <- mapbox_api(tilenum = auckland_tile)
sth <- mapbox_api(tilenum =christchurch_tile)
unlist(auckland_tile)
nth <- protolite::read_mvt_sf(nth$content, zxy = c(4, 15, 9))
sth <- protolite::read_mvt_sf (sth$content, zxy = c(4, 15, 10))

plot(rbind((nth$water), (sth$water)))
plot(rbind(lon_wrap_180(sth$water), lon_wrap_180(nth$water)))

nth <- if(is_anti_meridian_tile(list(zoom =4, x = 15,y = 9))){
  purrr::map(nth, lon_wrap_180)
} else {
  nth
}
identical(nth, nz_nth)
nth$water
nz_nth$water

plot(nth$water)

sth <- if(is_anti_meridian_tile(list(zoom =4, x = 15,y = 10))){
  purrr::map(sth, lon_wrap_180)
} else {
  sth
}

plot(sth$water)

plot(rbind(nth$water, sth$water))

reduce(list(nth, sth), function(a,b){
  rbind(a[["water"]], b[["water"]])
}) %>% plot()





nz_nth <- spoils(zoom = 4,
                 longitude = auckland$longitude,
                 latitude = auckland$latitude )

nz_sth <- spoils(zoom = 4,
                 longitude = christchurch$longitude,
                 latitude = christchurch$latitude )
nz <- stitch(list(nz_nth, nz_sth))

ggplot() +
  geom_sf(data = nz$admin)

plot(rbind(nz_nth$water, nz_sth$water))
plot(nz$water)

ggplot() +

  geom_sf(data = nz_sth$water)

map(nz, tile_layers)
water <-  reduce(map(nz,"water") %>% map(lon_wrap_180), rbind)
place_label <- reduce(map(nz,"place_label") %>% map(lon_wrap_180) , rbind)

 ggplot() +
   geom_sf(data = water) +
   geom_sf(data = place_label)

 nz_place_label <- place_label %>%
   st_join(st_bbox(water) %>% st_as_sfc() %>% st_sf(), join = st_intersects, left = FALSE)
not_nz_place_label <- place_label %>%
  st_join(st_bbox(water) %>% st_as_sfc() %>% st_sf(), join = st_disjoint, left = FALSE)

ggplot() +
  geom_sf(data = water) +
  geom_sf(data = st_bbox(water) %>% st_as_sfc() %>% st_sf(), fill = NA,
          colour = "purple") +
  geom_sf(data = nz_place_label, colour = "green") +
  geom_sf(data = not_nz_place_label, colour = "red")

water_bbox_sf <- st_bbox(water) %>% st_as_sfc() %>% st_sf()
water_2 <- st_difference(water, water_bbox_sf )
ggplot() +
  geom_sf(data = water_2, colour = "yellow") +
  geom_sf(data = water_bbox_sf , fill = NA,
          colour = "purple") +
  geom_sf(data = nz_place_label, colour = "green") +
  geom_sf(data = not_nz_place_label, colour = "red")

water_2

tilenum <- list(zoom = 4, x = 14, y = 9)
au_se <- victor:::mapbox_api(tilenum = tilenum)
au_se <- protolite::read_mvt_sf(au_se$content, zxy = unlist(tilenum))
ggplot(au_se$water) + geom_sf()
ggplot(au_se$water %>% lon_wrap_180()) + geom_sf()

tilenum <- list(zoom = 4, x = 14, y = 10)
au_tas <- victor:::mapbox_api(tilenum = tilenum)
au_tas <- protolite::read_mvt_sf(au_tas$content, zxy = unlist(tilenum))
ggplot(au_tas$water) + geom_sf()

ggplot(rbind(au_ne$water, au_tas$water)) +
  geom_sf()

tilenum <- list(zoom = 4, x = 0, y = 9)
pac_1 <- victor:::mapbox_api(tilenum = tilenum)
pac_1 <- protolite::read_mvt_sf(pac_1$content, zxy = unlist(tilenum))
ggplot(pac_1$water) + geom_sf()
ggplot(pac_1$water %>% lon_wrap_180()) + geom_sf()

tilenum <- list(zoom = 4, x = 0, y = 10)
pac_2 <- victor:::mapbox_api(tilenum = tilenum)
pac_2 <- protolite::read_mvt_sf(pac_2$content, zxy = unlist(tilenum))
ggplot(pac_2$water) + geom_sf()
ggplot(pac_2$water %>% lon_wrap_180()) + geom_sf()

ggplot(rbind(au_ne$water, au_tas$water)) +
  geom_sf()

ggplot(rbind(au_ne$water,
             au_tas$water,
             (nz_nth$water),
             (nz_sth$water),
             lon_wrap_180(pac_1$water),
             lon_wrap_180(pac_2$water))) +
  geom_sf()

slippymath::lonlat_to_tilenum(172.078171,-42.576470, 4)

nz_1 <- spoils(zoom = 4, longitude = 172.078171 , latitude = -42.576470 )
ggplot(nz_1$water) + geom_sf()

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





