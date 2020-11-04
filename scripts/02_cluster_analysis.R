#' workshop #2: cluster analysis
#' @author Fernando Prudencio
#'
#' @data
#' 'grd.pp', gridded rainfall climatological data built by Aybar et al. (2019)
#' 'grd.dem', SRTM digital elevation model resampled to 10km
#'

rm(list = ls())

####' Installing packages
pkg <- c("tidyverse", "raster", "ncdf4", "sf")

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
library(sf)
library(factoextra)
library(cluster)
library(NbClust)

####' Read vector data
sf.basin <- st_read(dsn = "data/vector/basins.gpkg", layer = "ucayali")

####' Read raster data
grd.pp <- brick("data/raster/PISCOpc.tif")
grd.dem <- raster("data/raster/SRTM_10km.nc") # it's already resampled

####' Stacking rainfall climatology and elevation
grd.stck <- raster::stack(grd.pp, grd.dem) %>%
  crop(sf.basin) %>%
  raster::mask(sf.basin)

####' Build dataframe before analysis
df <- coordinates(grd.stck) %>%
  cbind(getValues(grd.stck)) %>%
  as_tibble() %>%
  drop_na()

names(df) <- c("lon", "lat", month.abb, "elev")

####' Partitioning Around Medoids (PAM)
###'   1| standardization of data
stzn.df <- scale(df)

###'   2| calculating the best number of clusters
k.nbclust <- NbClust(
  stzn.df,
  diss = NULL, distance = "euclidean",
  min.nc = 2, max.nc = 10, method = "median"
)

fviz_nbclust(k.nbclust)

###'   3| Calculating distance matrix
##'      3.1| choose between euclidean, gower or manhattan metric
dist.df <- daisy(stzn.df, metric = "euclidean")

###'   4| Cluster analysis by Partitioning Around Medoids
##'       if you insert distance matrix (dist.df), it's necessary that you add
##'       "diss = T" argument into pam() function
set.seed(2020)
clus.df <- pam(dist.df, 3, diss = T, metric = "euclidean")

###'   5| Plot average silhouette value and build table with number of
##'       negative values by cluster
##'      5.1| building average silhouette value plot
fviz_silhouette(clus.df) + theme_bw() + coord_flip()
sltte.plt <- factoextra::fviz_silhouette(
  clus.df, label = FALSE, print.summary = TRUE
) + theme_bw() + coord_flip() +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_text(size = 1)
  )
##'      5.2| saving average silhouette value plot
ggsave(
  sltte.plt,
  filename = "exports/average_silhouette_value_plot.png",
  width = 18, height = 15, units = "cm", dpi = 500
)

##'      5.3| building number of negative value table
sltee.nv <- clus.df$silinfo[[1]] %>%
  as_tibble() %>%
  filter(sil_width < 0) %>%
  group_by(cluster) %>%
  summarise(neg_val = length(sil_width))

#' Plot clusters from the first two components of PCA
#'   1| building cluster plot
clus.plt <- factoextra::fviz_cluster(
  clus.df,
  ggtheme = theme_bw(),
  ellipse.type = "t",
  geom = "point",
  star.plot = TRUE
)
#'   2| saving cluster ploting
ggsave(clus.plt,
  filename = "exports/rainfall_data_clusters.png",
  width = 18, height = 15, units = "cm", dpi = 500
)

#' Building the results to gridded data
xy.coord <- coordinates(grd.stck) %>%
  as_tibble() %>%
  rename("lon" = "x", "lat" = "y")

grid.df <- xy.coord %>%
  left_join(
    cbind(
      df %>% dplyr::select(lon, lat),
      clus = clus.df$clustering
    ),
    by = c("lon", "lat")
  ) %>%
  as_tibble()

names(grid.df) <- c("x", "y", "z")

data.crs <- crs(grd.stck)
data.res <- res(grd.stck)
data.grid <- rasterFromXYZ(grid.df, data.res, data.crs, digits = 0)
data.vector <- rasterToPolygons(data.grid, dissolve = T) %>%
  st_as_sf() %>%
  rename("id" = "z")

st_write(data.vector, dsn = "data/vector/clusters.gpkg", layer = "clusters")

writeRaster(data.grid,
  "data/raster/clusters.tif",
  overwrite = T
)