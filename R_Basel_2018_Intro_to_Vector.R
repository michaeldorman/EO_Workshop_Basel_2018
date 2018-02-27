## install.packages("sf")
## install.packages("mapview")
## install.packages("units")
## install.packages("dplyr")
## devtools::install_github("tidyverse/ggplot2")

setwd("/home/michael/Dropbox/Presentations/p_2017_08_R_workshop/michael/data")

library(sf)

point = st_as_sfc("POINT (30 10)")[[1]]
linestring = st_as_sfc("LINESTRING (30 10, 10 30, 40 40)")[[1]]
polygon = st_as_sfc("POLYGON ((35 10, 45 45, 15 40, 10 20, 35 10),(20 30, 35 35, 30 20, 20 30))")[[1]]
multipoint = st_as_sfc("MULTIPOINT ((10 40), (40 30), (20 20), (30 10))")[[1]]
multilinestring = st_as_sfc("MULTILINESTRING ((10 10, 20 20, 10 40),(40 40, 30 30, 40 20, 30 10))")[[1]]
multipolygon = st_as_sfc("MULTIPOLYGON (((40 40, 20 45, 45 30, 40 40)),((20 35, 10 30, 10 10, 30 5, 45 20, 20 35),(30 20, 20 15, 20 25, 30 20)))")[[1]]
geometrycollection = st_as_sfc("GEOMETRYCOLLECTION (POLYGON((30 20, 45 40, 10 40, 30 20)),LINESTRING (10 10, 20 20, 10 30),POINT (40 20))")[[1]]
pol = st_as_sfc("POLYGON((30 20, 45 40, 10 40, 30 20))")[[1]]
l = st_as_sfc("LINESTRING (10 10, 20 20, 10 30)")[[1]]
p = st_as_sfc("POINT (40 20)")[[1]]
opar = par()
par(mfrow = c(2, 4), mar = c(1,1,1,1))
plot(point, main = "POINT", col = "blue", cex = 1.8, lwd = 2)
plot(linestring, main = "LINESTRING", col = "blue", lwd = 2)
plot(polygon, main = "POLYGON", border = "blue", col = "#0000FF33", lwd = 2)
plot(1, type="n", axes=F, xlab="", ylab="")
plot(multipoint, main = "MULTIPOINT", col = "blue", cex = 1.8, lwd = 2)
plot(multilinestring, main = "MULTILINESTRING", col = "blue", lwd = 2)
plot(multipolygon, main = "MULTIPOLYGON", border = "blue", col = "#0000FF33", lwd = 2)
plot(geometrycollection, main = "GEOMETRYCOLLECTION", col = NA, border = NA, lwd = 2)
plot(pol, border = "blue", col = "#0000FF33", add = TRUE, lwd = 2)
plot(l, col = "blue", add = TRUE, lwd = 2)
plot(p, col = "blue", add = TRUE, cex = 1.8, lwd = 2)
par(opar)

pnt1 = st_point(c(34.812831, 31.260284))
pnt1
class(pnt1)

pnt2 = st_point(c(34.798443, 31.243288))

geom = st_sfc(pnt1, pnt2, crs = 4326)
geom

dat = data.frame(
  name = c("Beer-Sheva North", "Beer-Sheva Center")
)
dat

pnt = st_sf(dat, geom)
pnt

st_geometry(pnt)

st_set_geometry(pnt, NULL)

st_coordinates(pnt)

library(mapview)

mapview(pnt)

head(quakes)

quakes_pnt = st_as_sf(
  x = quakes, 
  coords = c("long", "lat"), 
  crs = 4326
)

head(quakes_pnt)

plot(quakes_pnt)

states = st_read(
  dsn = "/home/michael/Dropbox/Presentations/p_2017_08_R_workshop/michael/data/cb_2015_us_state_5m.shp", 
  stringsAsFactors = FALSE
)
tracks = st_read(
  dsn = "/home/michael/Dropbox/Presentations/p_2017_08_R_workshop/michael/data/hurmjrl020.shp", 
  stringsAsFactors = FALSE
)

dim(states)

plot(states)

dim(tracks)

plot(tracks)
plot(st_geometry(states), border = "grey")

plot(st_geometry(states), border = "grey")
plot(st_geometry(tracks), col = "red", add = TRUE)

mapview(tracks, zcol = "wind_mph", legend = TRUE)

states = st_transform(states, 2163)
tracks = st_transform(tracks, 2163)

plot(states)

tracks = tracks[tracks$year > 1949, ]
strong = unique(tracks$btid[tracks$category == "H5"])
tracks_h5 = tracks[tracks$btid %in% strong, ]

states$area = st_area(states)

states$area[1:3]

class(states$area)

library(units)

states$area = set_units(states$area, km^2)

states$area[1:3]

opar = par()
plot(states[, "area"])
par(opar)

int = st_intersects(states, states, sparse = FALSE)

int[1:4, 1:4]

int1 = apply(int, 2, rev)
int1 = t(int1)
image(int1, col = c("lightgrey", "red"), asp = 1, axes = FALSE)
axis(3, at = seq(0, 1, 1/(nrow(int1)-1)), labels = states$state, las = 2, lwd = 0, lwd.ticks = 1, cex.axis = 0.75)
axis(2, at = seq(0, 1, 1/(nrow(int1)-1)), labels = rev(states$state), las = 1, pos = -0.01, lwd = 0, lwd.ticks = 1, cex.axis = 0.75)

set.seed(1)
x = st_multipoint(matrix(runif(10), ncol = 2))
x = st_buffer(st_sfc(lapply(1:3, function(x) st_point(c(x,x)))), 0.2 * 1:3)
opar = par(mfrow=c(1,5), mar = c(0,0,1,0))
plot(x, border = '#ff333388')
plot(st_centroid(x), add = TRUE, pch = 3)
title("st_centroid")
plot(x, border = '#ff333388')
plot(st_buffer(x, dist = 0.1), add = TRUE, pch = 3)
plot(st_buffer(x, dist = 0.2), add = TRUE, pch = 3)
plot(st_buffer(x, dist = 0.3), add = TRUE, pch = 3)
plot(st_buffer(x, dist = 0.4), add = TRUE, pch = 3)
plot(st_buffer(x, dist = 0.5), add = TRUE, pch = 3)
title("st_buffer")
s = split(x, 1:3)
s = lapply(s, st_sample, size = 5)
s = lapply(s, st_combine)
s = do.call(c, s)
plot(x, border = '#ff333388')
plot(s, add = TRUE, pch = 3)
title("st_sample")
plot(s, col = '#ff333388', pch = 3)
plot(st_convex_hull(s), add = TRUE, pch = 3)
title("st_convex_hull")
s = st_union(s)
v = st_voronoi(s)
plot(s, col = '#ff333388', pch = 3)
plot(v, col = NA, border = 1, axes = FALSE, add = TRUE)
title("st_voronoi")
par(opar)

states_ctr = st_centroid(states)

plot(st_geometry(states), border = "grey")
plot(st_geometry(states_ctr), col = "red", pch = 3, add = TRUE)

x = st_point(c(0, 0))
x = st_buffer(x, 0.5)
y = st_point(c(0.5, 0))
y = st_buffer(y, 0.5)
xy = c(x, y)
opar = par(mfrow=c(1,5), mar = c(0,0,1,0))
plot(xy, border = NA)
plot(x, add = TRUE, col = '#ff333388')
plot(y, add = TRUE, col='#33ff3388')
title("x: red, y: green")
plot(xy, border = 'grey')
plot(st_intersection(x, y), col = 'lightblue', add = TRUE)
title("intersection(x, y)")
plot(xy, border = 'grey')
plot(st_difference(x, y), col = 'lightblue', add = TRUE)
title("difference(x, y)")
plot(xy, border = 'grey')
plot(st_sym_difference(x, y), col = 'lightblue', add = TRUE)
title("sym_difference(x, y)")
plot(xy, border = 'grey')
plot(st_union(x, y), col = 'lightblue', add = TRUE)
title("union(x, y)")
par(opar)

tracks_int = st_intersection(tracks, states)

plot(st_geometry(states), border = "lightgrey")
plot(tracks_int[, "state"], lwd = 3, add = TRUE)

class(tracks_int$geometry)

tracks_int = st_cast(tracks_int, "MULTILINESTRING")

class(tracks_int$geometry)

tracks_int$length = st_length(tracks_int)

tracks_int$length = set_units(tracks_int$length, km)

library(dplyr)

track_lengths = 
  tracks_int %>% 
  st_set_geometry(NULL) %>% 
  group_by(state) %>% 
  summarize(length = sum(length)) %>% 
  as.data.frame
head(track_lengths)
states = merge(
  states, 
  track_lengths, 
  by = "state", 
  all.x = TRUE
)

head(states)

plot(states[, "length"])

states$track_density = states$length / states$area

plot(states[, "track_density"])

plot(states)

tracks_states = st_join(
  states[, "state"],
  tracks[, "name"]
  )

tracks_states

left = st_join(
  states[, "state"],
  tracks[, "name"]
  )

inner = st_join(
  states[, "state"],
  tracks[, "name"],
  left = FALSE
  )

opar = par(mfrow = c(1, 2))
plot(left[, "name"], main = "Left join")
plot(inner[, "name"], main = "Inner join")
par(opar)

tracks_h5_agg = aggregate(
  tracks_h5[, "name"], 
  data.frame(id = tracks_h5$btid), 
  unique
  )

tracks_h5_agg

plot(tracks_h5_agg[, "name"], lwd = 3, key.size = lcm(3))
plot(st_geometry(states), border = "lightgrey", add = TRUE)

states_tracks_h5 = st_join(
  states, 
  tracks_h5_agg, 
  left = FALSE
  )

states_tracks_h5

library(ggplot2)

ggplot() +
  geom_sf(
    data = states
    ) +
  geom_sf(
    data = tracks_h5_agg, 
    aes(colour = name)
  )

ggplot() +
  geom_sf(
    data = states, 
    fill = NA, colour = "black", size = 0.1
  ) +
  geom_sf(
    data = states_tracks_h5, 
    fill = "red", colour = NA, size = 0.3, alpha = 0.2
  ) +
  facet_wrap(~ name) +
  theme_bw()

ggplot() +
  geom_sf(data = states, fill = NA, colour = "darkgrey", size = 0.1) +
  geom_sf(data = states_tracks_h5, fill = "red", colour = "black", size = 0.3, alpha = 0.2) +
  geom_sf(data = tracks_h5, aes(size = wind_mph, colour = wind_mph)) +
  geom_sf(data = tracks_h5, colour = "black", size = 0.2) +
  coord_sf(xlim = st_bbox(states)[c(1,3)], ylim = st_bbox(states)[c(2,4)]) +
  scale_size_continuous(range = c(0.1, 5), guide = FALSE) +
  scale_colour_distiller("Wind speed\n(miles / hour)", palette = "Spectral") +
  facet_wrap(~ name, ncol = 3) +
  theme_bw() +
  theme(
    panel.grid.major = element_line(colour = "transparent"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = c(0.8, 0.17)
    )
