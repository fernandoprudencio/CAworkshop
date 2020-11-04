#' workshop #2: calculate climatology
#' @author Fernando Prudencio

rm(list = ls())

####' Installing packages
pkg <- c("tidyverse", "raster", "ncdf4", "Hmisc")

sapply(
  pkg,
  function(x) {
    is.there <- x %in% rownames(installed.packages())
    if (is.there == FALSE) {
      install.packages(x, dependencies = T)
    }
  }
)

####' Load packages
library(tidyverse)
library(raster)
library(ncdf4)
library(Hmisc)

####' Create time series within a dataframe
df <- tibble(
  date = seq(as.Date("1981-01-01"), as.Date("2016-12-01"), by = "1 month")
) %>%
  mutate(id = 1:n())

####' Build a function to calculate climatology
fun.clim <- function(month, years.omit, data) {
  grd.mt <- df %>%
    filter(
      str_sub(date, 6, 7) == month &
        str_sub(date, 1, 4) %nin% years.omit
    )

  data[[grd.mt$id]] %>%
    "*"(1) %>%
    mean(na.rm = T) %>%
    return()
}

####' Apply fun.clim() function
grd.clim <- sapply(
  sprintf("%02d", 1:12),
  FUN = fun.clim,
  years.omit = c(2005, 2010, 2016),
  data = brick("data/raster/PISCOpm.nc")
) %>%
  stack() %>%
  "*"(1)

####' Write raster
writeRaster(grd.clim, "data/raster/PISCOpc.tif", overwrite = TRUE)