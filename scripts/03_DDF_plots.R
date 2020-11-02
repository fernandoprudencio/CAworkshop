#' workshop #2: plotting DDF
#' @author Fernando Prudencio
#'

rm(list = ls())

####' Installing packages
pkg <- c("raster", "tidyverse", "sf", "Hmisc", "extrafont", "grid")

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
library(raster)
library(tidyverse)
library(sf)
library(Hmisc)
library(extrafont)
library(grid)

####' Load Rdata
load("data/rdata/values_cluster3.RData")

####' Load constants
k.dry.yr <- c(2005, 2010, 2016) # dry years
k.date.prd <- seq(as.Date("1981-01-01"), as.Date("2016-12-31"), by = "1 day")

####' Read vector data
sf.basin <- st_read(dsn = "data/vector/clusters.gpkg", layer = "clusters") %>%
  dplyr::filter(id == 3)

####' Read raster data
# grd.data <- brick("data/raster/PISCOpd.nc")
# 
# for (i in 1:nlayers(grd.data)) {
#   print(i)
#   vls.day <- lapply(
#     raster::extract(grd.data[[i]], sf.basin),
#     function(x) if (!is.null(x)) mean(x, na.rm = TRUE) else NA
#   ) %>%
#     unlist()
# 
#   if (i == 1) vls.mn <- vls.day else vls.mn <- c(vls.mn, vls.day)
# }
# 
# save(vls.mn, file = "data/rdata/values_cluster3.RData")

####' BUILD A DATAFRAME OF TEMPERATURE DATE
df.rgn.data <- tibble(
  date = k.date.prd,
  values = vls.mn
) %>%
  mutate(month = str_sub(date, 6, 7)) %>%
  group_by(month) %>%
  mutate(decil = quantile(values, .9)) %>%
  ungroup() %>%
  mutate(hdf = ifelse(values >= decil, 1, 0)) %>%
  filter(
    str_sub(date, 6, 7) %nin% c("01", "02")
  )

#' DATAFRAME WITH AVERAGE ACCUMULATED HDF BY CLUSTER REGION
k.years <- 1981:2016
for (i in k.years) {
  if (i == k.years[1]) {
    df.hdf.ac <- df.rgn.data %>%
      filter(substr(date, 1, 4) == i) %>%
      mutate(hdf.ac = cumsum(hdf)) %>%
      dplyr::select(hdf.ac)
    
    names(df.hdf.ac)[i - (k.years[1] - 1)] <- sprintf("yr.%s", i)
  } else {
    df.hdf.ac <- df.hdf.ac %>%
      mutate(id = 1:n()) %>%
      left_join(
        df.rgn.data %>%
          filter(substr(date, 1, 4) == i) %>%
          mutate(hdf.ac = cumsum(hdf)) %>%
          dplyr::select(hdf.ac) %>%
          mutate(id = 1:n()),
        by = "id"
      ) %>%
      dplyr::select(-id)
    
    names(df.hdf.ac)[i - (k.years[1] - 1)] <- sprintf("yr.%s", i)
  }
}

#' DATAFRAME WITH AVERAGE ACCUMULATED HDF BY CLUSTER REGION, DURING DRY YEARS
#'   AND NORMAL YEARS
df.hdf.ac.norm <- df.hdf.ac %>%
  dplyr::select(sprintf("yr.%s", k.years[k.years %nin% c(k.dry.yr)]))

df.hdf <- df.hdf.ac %>%
  mutate(
    hdf.max = df.hdf.ac.norm %>% apply(1, max, na.rm = T),
    hdf.min = df.hdf.ac.norm %>% apply(1, min, na.rm = T),
    hdf.mean = df.hdf.ac.norm %>% apply(1, mean, na.rm = T),
    date = seq(as.Date("2020-03-01"), as.Date("2020-12-31"), by = "1 day")
  ) %>%
  dplyr::select(
    sprintf("yr.%s", k.dry.yr),
    hdf.max, hdf.min, hdf.mean, date
  ) %>%
  gather(key = "type", value = "value", -date, -hdf.max, -hdf.min)

#' DEFINE LABELS FOR LEGEND
lbls <- c(
  "average years under\nnormal conditions",
  "year 2005", "year 2010", "year 2016"
)

#' PLOT TEMPORAL EVOLUTION ACCUMULATED HDF
plt.hdf <- ggplot(df.hdf, aes(date, value, group = type)) +
  labs(
    title = "SEASONAL EVOLUTION OF\nDRY DAY FREQUENCY (DDF)",
    y = "accumulated from hot days"
  ) +
  geom_ribbon(
    aes(ymin = hdf.min, ymax = hdf.max),
    size = .2, fill = "gray", color = "black", alpha = .5
  ) +
  geom_line(aes(linetype = type, color = type, size = type)) +
  scale_linetype_manual(
    values = c("dashed", "solid", "solid", "solid", "solid"), labels = lbls
  ) +
  scale_color_manual(
    values = c("gray", "blue", "black", "green", "red"),
    labels = lbls
  ) +
  scale_size_manual(values = rep(.8, 5), labels = lbls) +
  scale_x_date(
    limits = c(as.Date("2020-03-01"), as.Date("2020-12-31")),
    breaks = seq(as.Date("2020-03-01"), as.Date("2020-12-31"), by = "1 month"),
    date_labels = "%b", expand = expand_scale(mult = c(.02, 0))
  ) +
  scale_y_continuous(
    breaks = seq(0, 150, 20),
    limits = c(-.003, 60),
    expand = expand_scale(mult = c(0, 0))
  ) +
  theme_bw() +
  theme(
    legend.background = element_rect(fill = "white", color = "black", size = .3),
    legend.margin = margin(.25, 4, 6, 4),
    legend.key.width = unit(.35, "cm"),
    legend.key.height = unit(.25, "cm"),
    legend.position = c(0.25, 0.85),
    legend.title = element_blank(),
    legend.text = element_text(size = 8, family = "Source Sans Pro"),
    plot.title = element_text(size = 12, hjust = 1, family = "Source Sans Pro"),
    axis.text.x = element_text(
      size = 10, colour = "black", family = "Source Sans Pro",
      face = "bold", angle = 45, hjust = -.3, vjust = .1
    ),
    axis.text.y = element_text(
      size = 12, face = "bold", family = "Source Sans Pro", color = "black"
    ),
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      size = 12, face = "bold", family = "Source Sans Pro", color = "black"
    ),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    panel.grid = element_blank(),
    panel.border = element_rect(size = .5, color = "black"),
    plot.margin = margin(1.5, .1, 1, 1, "cm"),
    axis.line.y = element_line(
      size = .5, color = "black"
    )
  )

ggsave(
  plot = plt.hdf, "exports/ddf_cluster3.png",
  width = 10, height = 12, units = "cm", dpi = 1000
)