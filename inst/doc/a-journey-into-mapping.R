## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, collapse = TRUE, 
                      fig.width = 7, fig.height = 4,
                      fig.align = "center", message = FALSE, warning = FALSE)


## ---- message = FALSE---------------------------------------------------------
library('mapping')

## ---- message = FALSE, eval=FALSE---------------------------------------------
#  remotes::install_github('dataallaround/mapping')

## -----------------------------------------------------------------------------
data("popWR")
str(popWR)

## -----------------------------------------------------------------------------
data("popEU")
str(popEU)

## -----------------------------------------------------------------------------
data("popIT")
str(popIT)

## -----------------------------------------------------------------------------
data("popUS")
str(popUS)

## -----------------------------------------------------------------------------
coord_eu <- loadCoordEU(unit = "nuts0")

## -----------------------------------------------------------------------------
library(tmap)
tm_shape(coord_eu) + tm_borders()

## ---- eval=FALSE--------------------------------------------------------------
#  checkNamesIT(popIT$ID, unit = "provincia")

## -----------------------------------------------------------------------------
head(getNamesEU(unit = "nuts0"))
head(getNamesEU(unit = "nuts0", all_levels = FALSE))

## -----------------------------------------------------------------------------
str(popIT)

## -----------------------------------------------------------------------------
it <- IT(data = popIT, unit = "provincia", year = "2018",colID = "ID")

## -----------------------------------------------------------------------------
str(it,1)

## -----------------------------------------------------------------------------
library(tmap)
tm_shape(it) + tm_borders() + tm_fill("totale")

## -----------------------------------------------------------------------------
it <- IT(data = popIT, unit = "provincia", 
         year = "2018",colID = "ID",
         subset = ~ I(regione == "Lazio"))

## -----------------------------------------------------------------------------
tm_shape(it) + tm_borders()

## -----------------------------------------------------------------------------
it <- IT(data = popIT, unit = "provincia", 
         year = "2018",colID = "ID",
         add = ~I(maschi/totale) + I(femmine/totale), 
         new_var_names = c("Male percentage", "Female percentage"),
         print = FALSE)

str(it,1)

## -----------------------------------------------------------------------------
str(popEU)
eu <- EU(data = popEU, unit = "nuts0", colID = "GEO", 
         matchWith = "id", check.unit.names = FALSE)

## -----------------------------------------------------------------------------
coord_eu <- loadCoordEU(unit = "nuts0")
mappingEU(data = coord_eu)

## -----------------------------------------------------------------------------
eu <- EU(data = popEU, unit = "nuts0", colID = "GEO", 
         matchWith = "iso2", check.unit.names = FALSE)
mappingEU(eu, var = "total")

## -----------------------------------------------------------------------------
mappingEU(data = popEU,unit = "nuts0", colID = "GEO", matchWith = "iso2", var = "total")

## -----------------------------------------------------------------------------
eu_aggr <- EU(data = popEU, unit = "nuts1", colID = "GEO", 
         matchWith = "id", check.unit.names = FALSE,
         aggregation_var = "total", aggregation_unit = "nuts0", aggregation_fun = sum)

mappingEU(eu_aggr, var = "total")

## -----------------------------------------------------------------------------
eu <- EU(data = popEU, unit = "nuts1", colID = "GEO", 
         matchWith = "id", check.unit.names = FALSE)


mappingEU(eu, var = "total", aggregation_unit = "nuts0", aggregation_fun = sum)

## -----------------------------------------------------------------------------
mappingEU(eu, var = c("male","female"))

## -----------------------------------------------------------------------------
mappingEU(eu, var = "total", 
          subset = ~I(country == "Spain" | country == "Italy"))

## -----------------------------------------------------------------------------
data("popUS")
us <- US(data = popUS, unit = "state", matchWith = "name")
mappingUS(us)

## -----------------------------------------------------------------------------
mappingUS(us, var = "population")

mappingUS(us, var = "population", add_text = "state_id",
          options = mapping.options(nclass = 10, legend.portrait = FALSE))

## ---- eval=FALSE--------------------------------------------------------------
#  mappingUS(us, var = "population", options = mapping.options(nclass = 10, legend.portrait = FALSE))

## -----------------------------------------------------------------------------
mappingUS(us, var = "population", aggregation_unit = "division", facets = "division")

## -----------------------------------------------------------------------------
mappingUS(us, var = "population", subset = ~I(region == "Northeast"), facets = "id")

## -----------------------------------------------------------------------------
eu <- EU(data = popEU, unit = "nuts0", colID = "GEO", 
         matchWith = "id", check.unit.names = FALSE)
mappingEU(eu, var = "total", type = "interactive")


## ---- eval=FALSE--------------------------------------------------------------
#  mappingEU(eu, var = "total", type = "interactive",
#                        subset = ~I(country == "Spain" | country == "Italy"))

## ---- eval=FALSE--------------------------------------------------------------
#  mappingEU(eu, var = "total", type = "interactive",
#                        aggregation_unit = "nuts0")

## ---- eval=FALSE--------------------------------------------------------------
#  mappingEU(eu, var = c("male","female"), type = "interactive")

## -----------------------------------------------------------------------------
library(dplyr)

data("popIT")
popIT <- popIT
coords <- loadCoordIT(unit = "provincia", year = '2019')
cr <- left_join(coords, popIT, by = c( "provincia" = "ID"))
mapping(cr)
mapping(cr, var = "maschi")

## -----------------------------------------------------------------------------
library(sf)
nc = st_read(system.file("shape/nc.shp", package="sf"))
class(nc)
mapping(nc)

## ---- eval=FALSE--------------------------------------------------------------
#  mapping.options()

## -----------------------------------------------------------------------------
mapping.options("palette.cont")
mapping.options("legend.position")

## -----------------------------------------------------------------------------
mapping.options(legend.position = c("left","bottom"))
mapping.options("legend.position")

## -----------------------------------------------------------------------------
map <- mappingEU(eu, var = "total")
map_options <- mappingEU(eu, var = "total", 
                         options = mapping.options(list(legend.position = c("left","bottom"),
                                                        title = "EU total population",
                                                        map.frame = FALSE,
                                                        col.style = "pretty")))

library(tmap)
tmap_arrange(map, map_options)
mapping.options.reset()

## -----------------------------------------------------------------------------
mappingIT(data = it, var = "totale")
mappingIT(data = it, var = c("maschi", "femmine"))
mappingIT(data = it, var = c("maschi", "femmine"), options = mapping.options(palette.cont = c("Greens", "Blues"), legend.position = c("left","bottom")))

mappingIT(data = it, var = "ripartizione")



## ---- eval=FALSE--------------------------------------------------------------
#  mappingIT(data = it, var = "totale", add_text = "code")

## ---- eval=FALSE--------------------------------------------------------------
#  mappingIT(data = it, var = "totale", type = "interactive")
#  mappingIT(data = it, var = "totale", type = "interactive",
#            options = mapping.options(legend.position = c("left","bottom")))
#  
#  mappingIT(data = it, var = c("maschi", "femmine"), type = "interactive",
#            options = mapping.options(interactive.control.collapse = FALSE))
#  mappingIT(data = it, var = c("maschi", "femmine"), type = "interactive",
#            options = mapping.options(palette.cont = c("BuGn", "BuPu")))
#  
#  
#  mappingIT(data = it, var = "totale", type = "interactive",
#            options = mapping.options(col.style = "pretty"))
#  mappingIT(data = it, var = "totale", type = "interactive",
#            options = mapping.options(col.style = "equal"))
#  mappingIT(data = it, var = "totale", type = "interactive",
#            options = mapping.options(col.style = "equal", nclass = 7))
#  
#  mappingIT(data = it, var = "ripartizione", type = "interactive")
#  mappingIT(data = it, var = "totale", type = "interactive", facets = "ripartizione")
#  

## -----------------------------------------------------------------------------
data(popIT)
it <- IT(data = popIT, unit = "provincia", year = "2018",colID = "ID")

## -----------------------------------------------------------------------------

mappingIT(data = it, var = "totale", aggregation_unit = "regione")
mappingIT(data = it, var = "totale", 
          aggregation_unit = "ripartizione", 
          aggregation_fun = function(x) sum(x, na.rm = TRUE))


mappingIT(data = it, var = "totale", 
          aggregation_unit = "regione", facets = "ripartizione")

## ---- eval=FALSE--------------------------------------------------------------
#  mappingIT(data = it, var = "totale",
#            aggregation_unit = "ripartizione",
#            aggregation_fun = function(x) sum(x, na.rm = TRUE),
#            type = "interactive")
#  
#  
#  mappingIT(data = it, var = "totale",
#            aggregation_unit = "regione", facets = "regione")
#  

## -----------------------------------------------------------------------------
data("popEU")
popEU <- popEU
euNuts2 <- EU(data = popEU, colID = "GEO",unit = "nuts2",matchWith = "id")
str(euNuts2,1)

mappingEU(data = euNuts2, var = "total", 
          aggregation_unit = "nuts0", aggregation_fun = mean)


## -----------------------------------------------------------------------------
mappingEU(data = euNuts2, var = "total", 
          aggregation_unit = "nuts0", aggregation_fun = sum)
mappingEU(data = euNuts2, var = "total", subset = ~I(iso3 == "FRA"),
          aggregation_unit = "nuts1", aggregation_fun = sum, facets = "nuts1")

## ---- eval = FALSE------------------------------------------------------------
#  data("popWR")
#  popWR <- popWR
#  wr <- WR(data = popWR, colID = "country_code",
#           matchWith = "iso3_eh", check.unit.names = FALSE,
#           res = "low")
#  
#  mappingWR(data = wr, var = "total",
#            aggregation_unit = "continent",
#            aggregation_fun = function(x) sum(x, na.rm = TRUE))
#  
#  
#  mappingWR(data = wr, var = "total",
#            aggregation_unit = "continent",
#            aggregation_fun = function(x) sum(x, na.rm = TRUE),
#            type = "interactive")
#  

