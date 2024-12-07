# usethis::use_git()
# usethis::use_github(private = T)

usethis::use_git_ignore("dev_history_file.R")
usethis::use_build_ignore("dev_history_file.R")

devtools::document()
devtools::check()

usethis::git_vaccinate()

# rmd
usethis::use_readme_rmd()
#devtools::build_readme() <- pour tricoter le readme

## Ajout badge experimental
rcompendium::add_lifecycle_badge(lifecycle = "experimental")

## Ajout pipe ----
usethis::use_pipe()  # lancer avant un "devtools::document()"


## Ajout des dépendances packages ----
usethis::use_package("dplyr")
usethis::use_package("httr2")
usethis::use_package("sf")

## Ajout de fonctions ----
usethis::use_r("get_geobs_data")


library(sf)
# Fouillebroc area :
# 1.405907,49.341678,1.524525,49.384161
bbox <- c(xmin = 1.405907,
          ymin = 49.341678,
          xmax = 1.524525,
          ymax = 49.384161) %>%
  st_bbox(crs = 4326)

shape_area <- bbox %>%
  sf::st_as_sfc() %>%
  sf::st_sf()
mapview::mapview(shape_area)

communes <- sf::st_read("D:/GIS/shape_normandie/normandie_communes_COG_latest.gpkg")

df_test <- get_geobs_data("OBSTACLE_ICE", shape_area)
df_test$json_parsed <- lapply(df_test$json_ices, jsonlite::fromJSON)

library(dplyr)
library(tidyr)
df_test %>%
  mutate(json_column = purrr::map(json_ices, ~ jsonlite::fromJSON(.))) %>%
  unnest(json_column) %>% View()

library(dplyr)
get_geobs_data("REFERENTIEL_ROE_MONDE", shape_area) %>%
  filter(!is.na(hauteur_chute_icemesuree)) %>%
  select(identifiant_roe, hauteur_chute_icemesuree, date_modification_ouvrage) %>% mapview::mapview()
