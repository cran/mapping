# mapping

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/mapping)](https://cran.r-project.org/package=mapping)
[![CRAN\_MonthlyDownloads](http://cranlogs.r-pkg.org/badges/mapping)](https://cran.r-project.org/package=mapping)


Maps are an inportant tool to visualise variables distribution acroos different spatial object. The mapping process require to link the data with coordinates and then generate the correspondent map. This package provide coordinates, linking and mapping functions for an automatic, flexible and easy approach of mapping workflows of differnt geographical statistical unit.Geographical coordinates are provided in the package and automatically linked with the input data to generate maps with internal provided functions or external functions.


## Installation

You can install the released version of `mapping` from CRAN:

```{r}
install.packages("mapping")
```

or the development version from GitHub:

```{r}
devtools::install_github("serafinialessio/mapping", build = TRUE, build_opts = c("--no-resave-data", "--no-manual"))
```

## Usage

The main functions, generic functions and some examples are included in the vignette **A journey into *mapping* **, which is available as

```{r}
vignette("A journey into mapping")
```

A graphical list of the available plots can be look at

```{r}
vignette("Type of plots in mapping")

```

Details on single country functions are also available are


```{r}
vignette("mapping World")
vignette("mapping European Union")
vignette("mapping Italy")
vignette("mapping USA")
```



## 