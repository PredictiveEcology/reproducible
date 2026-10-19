## Toy ledgers shared by the CacheGeo tests
toyCRS <- "EPSG:32618"

## A square, `x0:x1` by `y0:y1` kilometres, as one sf row with a key and a list-column.
toySquare <- function(id, x0, x1, y0 = 0, y1 = 10, crs = toyCRS, param = id) {
  box <- sf::st_polygon(list(rbind(c(x0, y0), c(x1, y0), c(x1, y1), c(x0, y1), c(x0, y0))) )
  geom <- sf::st_sfc(box * 1000, crs = crs)
  out <- sf::st_sf(polygonID = id, geometry = geom)
  out$params <- I(list(list(p = param)))
  out
}

## Three polygons: P1 and P2 share an edge, P3 is apart (a gap from 20 to 30 km).
toyRows <- function() {
  rbind(toySquare("P1", 0, 10), toySquare("P2", 10, 20), toySquare("P3", 30, 40))
}

toyArea <- function(x0, x1, y0 = 2, y1 = 8, crs = toyCRS) toySquare("area", x0, x1, y0, y1, crs)[, "polygonID"]

newToyLedger <- function(dPath, rows = toyRows(), ...) {
  led <- CacheGeoLedger("toy.rds", destinationPath = dPath, key = "polygonID", ...)
  if (!is.null(rows)) CacheGeoWrite(led, rows, verbose = 0)
  led
}
