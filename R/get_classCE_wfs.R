#' Récupérer le classement Liste 1 et 2 des Cours d'eau sous forme spatiale
#'
#' @param liste texte. Nom du classement de continuité écologique ('Liste1', 'Liste2')
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
#' get_classCE_wfs(liste = 'Liste2', shape_area)
#' }
#'
get_classCE_wfs <- function(liste = 'Liste2', geo){

  geo_bbox <- sf::st_bbox(geo)

  url_sandre <- "https://services.sandre.eaufrance.fr/geo/zppn"
  url <- httr::parse_url(url_sandre)

  url$query <- list(service = "WFS",
                    version = "2.0.0",
                    request = "GetFeature",
                    typeName = paste0("SegClassContinuiteEco",{{liste}}),
                    outputFormat="geojson",
                    BBOX = paste0(geo_bbox[c("ymin","xmin","ymax","xmax")], collapse=",")
  )

  httr::build_url(url) %>%
    sf::read_sf()

}
