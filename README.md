# JGGEarthquake
## Capstone for Mastering software development in R

The functions provided by this package use data from the [U.S. National Oceanographic and Atmospheric Administration (NOAA)](https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1) Significant Earthquake Database. 

## Required set-up for this package

Currently, this package exists in a development version on GitHub. To use the package, you need to install it directly from GitHub using the `install_github` function from `devtools`. 

You can use the following code to install the development version of `jggEarthquake`: 

```R
library(devtools)
install_github("grandez/jggEarthquake")
library(jggEarthquake)
```

## Functions

According the requirements of project, following functions are available:

* Data tidying functions:
     + `eq_get_data`
     + `eq_clean_data`
     + `eq_location_clean`
     + `eq_map`
* Geom for creating visualisations:
     + `geom_timeline`
     + `geom_timeline_label`

But to simplify the use some *wrappers* has been added:

* **eq_load_data**: Load and prepare data for visualizations
* **eq_geom_timeline**: Generate the plot associated using geom_timeline
* **eq_geom_timeline_label**: Generate the plot associated using 

For more info about all these functions check the vignettes.

