library(sf)
library(rgdal)
library(raster)
library(rgeos)
## install.packages("sf")
## install.packages("mapview")
## install.packages("units")
## install.packages("dplyr")

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
opar = par(mfrow = c(2, 4), mar = c(1,1,1,1))
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

crs = rgdal::make_EPSG()

library(rgdal)

pol_sp = readOGR("data", "cb_2015_us_state_5m")
class(pol_sp)

library(sf)

pol_sf = st_read("data/cb_2015_us_state_5m.shp")
class(pol_sf)

x = st_as_sf(pol_sp)
class(x)

x = as(pol_sf, "Spatial")
class(x)

library(raster)

r = raster("data/dem.tif")
r
class(r)

proj4string(pol_sp)
st_crs(pol_sf)
proj4string(r)

library(rgeos)

p1 = readWKT("POLYGON((0 1,0.95 0.31,0.59 -0.81,-0.59 -0.81,-0.95 0.31,0 1))")
p2 = readWKT("POLYGON((2 2,-2 2,-2 -2,2 -2,2 2),(1 1,-1 1,-1 -1,1 -1,1 1))")
opar = par()
par(mfrow=c(2,3), mar = c(0, 0, 0.75, 0))
plot(gBuffer(p1,width=-0.2),col='grey',xlim=c(-1.5,1.5),ylim=c(-1.5,1.5))
plot(p1,border='red',lwd=2,add=TRUE);title("width: -0.2")
plot(gBuffer(p1,width=0),col='grey',xlim=c(-1.5,1.5),ylim=c(-1.5,1.5))
plot(p1,border='red',lwd=2,add=TRUE);title("width: 0")
plot(gBuffer(p1,width=0.2),col='grey',xlim=c(-1.5,1.5),ylim=c(-1.5,1.5))
plot(p1,border='red',lwd=2,add=TRUE);title("width: 0.2")
plot(gBuffer(p2,width=-0.2),col='grey',pbg='white',xlim=c(-2.5,2.5),ylim=c(-2.5,2.5))
plot(p2,border='red',lwd=2,add=TRUE);title("width: -0.2")
plot(gBuffer(p2,width=0),col='grey',pbg='white',xlim=c(-2.5,2.5),ylim=c(-2.5,2.5))
plot(p2,border='red',lwd=2,add=TRUE);title("width: 0")
plot(gBuffer(p2,width=0.2),col='grey',pbg='white',xlim=c(-2.5,2.5),ylim=c(-2.5,2.5))
plot(p2,border='red',lwd=2,add=TRUE);title("width: 0.2")
par(opar)

gDistance(pol_sp[1, ], pol_sp[2, ], byid = TRUE)
st_distance(pol_sf[1, ], pol_sf[2, ])

slope_asp = terrain(
  r, 
  opt = c("slope", "aspect"), 
  unit = "degrees"
  )
opar = par(mfrow = c(1,3), mar = c(1,1,1,5))
plot(r, main = "Elevation (m)", col = terrain.colors(30), axes = FALSE)
contour(r, add = TRUE)
plot(slope_asp[[1]], main = "Slope (°)", col = heat.colors(30) %>% rev, axes = FALSE)
plot(slope_asp[[2]], main = "Aspect (°)", col = rainbow(30)[1:28], axes = FALSE)
par(opar)

library(geosphere)

data(wrld)
plot(wrld, type = 'l', asp = 1, col = "grey")
LA = c(-118.40, 33.95)
NY = c(-73.78, 40.63)
gc = greatCircle(LA, NY)
lines(gc, lwd=2, col='blue')
gci = gcIntermediate(LA, NY)
lines(gci, lwd=4, col='green')
points(rbind(LA, NY), col='red', pch=20, cex=2)
mp = midPoint(LA, NY)
points(mp, pch='*', cex=3, col='orange')

LA = c(-118.40, 33.95)
NY = c(-73.78, 40.63)
gci = gcIntermediate(LA, NY)
head(gci)

library(rmapshaper)

pol_sp_s1 = ms_simplify(pol_sp, keep = 0.05)
pol_sp_s2 = ms_simplify(pol_sp, keep = 0.01)

opar = par(mfrow = c(1, 3))
plot(pol_sp)
plot(pol_sp_s1)
plot(pol_sp_s2)
par(opar)

library(gstat)
library(automap)

data(meuse)
data(meuse.riv)
coordinates(meuse) = ~ x + y
data(meuse.grid)
gridded(meuse.grid) = ~ x + y
grid = raster(meuse.grid)
f = log(zinc) ~ 1
v = autofitVariogram(f, meuse)
g = gstat(formula = log(zinc) ~ 1, model = v$var_model, data = meuse)
predicted = interpolate(grid, g)
predicted = mask(predicted, grid)
plot(predicted)
polygon(meuse.riv, asp = 1, col = "lightblue")
plot(meuse, pch = 1, cex = log(meuse$zinc) / 5, add = TRUE)

data(meuse)
data(meuse.riv)
coordinates(meuse) = ~ x + y
data(meuse.grid)
gridded(meuse.grid) = ~ x + y
grid = raster(meuse.grid)
grid[!is.na(grid)] = 1

plot(grid)
plot(meuse, add = TRUE)

v = autofitVariogram(log(zinc) ~ 1, meuse)
v$var_model
plot(v)

g = gstat(
  formula = log(zinc) ~ 1, 
  model = v$var_model, 
  data = meuse
  )

predicted = interpolate(grid, g)

plot(predicted)
contour(predicted, add = TRUE)

predicted = mask(predicted, grid)

plot(predicted)
contour(predicted, add = TRUE)

library(spdep)

nc = st_read(system.file("shape/nc.shp", package = "sf"))
nc = as(nc, "Spatial")
nc$rate = nc$SID79 / nc$BIR79
nb = poly2nb(nc)
plot(nc, border = "grey")
plot(nb, coordinates(nc), add = TRUE, col = "black")

file = system.file("shape/nc.shp", package = "sf")
nc = st_read(file)
nc$rate = nc$SID79 / nc$BIR79

opar = par()
plot(nc[, "rate"])
par(opar)

nc = as(nc, "Spatial")
nb = poly2nb(nc)

summary(nb)

plot(nc, border = "grey")
plot(nb, coordinates(nc), add = TRUE, col = "black")

nbw = nb2listw(nb)

summary(nbw)

moran.test(nc$rate, nbw)

library(spatstat)

data(cells)
U = distmap(cells)
contour(U, main = "")
plot(cells, add = TRUE, col = "red", pch = 3)

cells
japanesepines
redwood

opar = par(mfrow = c(1, 3))
plot(cells)
plot(japanesepines)
plot(redwood)
par(opar)

r = seq(0, sqrt(2)/6, by = 0.005)
Kcells = envelope(
  cells, 
  fun = Kest, 
  r = r, 
  nsim = 99
  )
Kpines = envelope(
  japanesepines,
  fun = Kest,
  r = r,
  nsim = 99
  )
Kredwood = envelope(
  redwood,
  fun = Kest,
  r = r,
  nsim = 99
  )

opar = par(mfrow = c(1, 3))
plot(Kcells)
plot(Kpines)
plot(Kredwood)
par(opar)

library(osmdata)

q = opq(bbox = "Beer-Sheva, Israel")
q = add_osm_feature(q, key = "highway")
dat = osmdata_sf(q)
lines = dat$osm_lines
pol = dat$osm_polygons
pol = st_cast(pol, "MULTILINESTRING")
pol = st_cast(pol, "LINESTRING")
lines = rbind(lines, pol)
lines = lines[, c("osm_id", "highway")]
lines = st_transform(lines, 32636)
plot(lines)

library(mapview)
states = st_read("data/us-states.geojson")
mapview(states, zcol = "density", legend = TRUE)

pal = colorBin(
  palette = "YlOrRd",
  domain = states$density,
  bins = c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
)

pal(153)

labels = paste0(
    "<b>",
    states$name,
    "</b><br/>",
    format(round(states$density, 2), nsmall = 2),
    " people / mi<sup>2</sup>"
  ) %>%
  lapply(htmltools::HTML)

leaflet(states) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("CartoDB.DarkMatterNoLabels") %>%
  addPolygons(
    fillColor = ~pal(density),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
      ),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
      )
    ) %>%
  addLegend(
    pal = pal,
    values = ~density,
    opacity = 0.7,
    title = NULL,
    position = "bottomright"
    )
