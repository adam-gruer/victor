#new zealand zoom 4 ------- features crossing anti-meridian (+180,-180)

slippymath::lonlat_to_tilenum(172.078171,-42.576470, 4)

nz <- victor:::mapbox_api(zoom = 4, x = 15, y = 10)
nz <- protolite::read_mvt_sf(nz$content, zxy = c(4, 15, 10))
x <- st_bbox(nz$water)[c("xmin", "xmax")]
xmin <- x["xmin"]
xmax <- x["xmax"]
sign(xmin) == sign(xmax)


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





