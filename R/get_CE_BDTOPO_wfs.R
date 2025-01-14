#' Récupérer les Cours d'eau de la BDTOPO IGN sous forme spatiale
#'
#' @param geo objet spatial de type 'sf'
#'
#' @returns un data.frame au format 'sf'
#' @export
#'
#' @importFrom httr parse_url build_url
#' @importFrom sf st_bbox read_sf
#'
#' @examples
#' \dontrun{
#' get_CE_BDTOPO_wfs(geo = shape_area)
#' }
#'

get_CE_BDTOPO_wfs <- function(geo){

  geo_bbox <- sf::st_bbox(geo)

  url_bdtopo <- "https://data.geopf.fr/wfs/wfs"
  url <- httr::parse_url(url_bdtopo)

  url$query <- list(service = "wfs",
                    version = "2.0.0",
                    REQUEST = "GetFeature",
                    TYPENAME = "BDTOPO_V3:cours_d_eau",
                    outputFormat = "application/json",
                    BBOX = paste0(geo_bbox[c("ymin","xmin","ymax","xmax")], collapse=",")
  )

  httr::build_url(url) %>%
    sf::read_sf()
}


