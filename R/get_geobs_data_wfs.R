#' Récupérer les données de la base GEOBS à partir du flux WFS (BRGM)
#'
#' @param couche nom de la couche à charger
#' @param geo objet spatial de type 'sf'
#'
#' @return un data.frame au format 'sf'
#' @export
#'
#' @importFrom httr parse_url build_url
#' @importFrom sf st_bbox read_sf
#'
#' @examples
#' \dontrun{
#' get_geobs_wfs_data("ROE_MONDE", shape_area)
#' }

get_geobs_wfs_data <- function(couche, geo){

  geo_bbox <- sf::st_bbox(geo)

  url_geobs <- "http://mapsref.brgm.fr/wxs/onema/geobs_monde"
  url <- httr::parse_url(url_geobs)

  url$query <- list(service = "wfs",
                    version = "1.0.0", # optional
                    request = "GetFeature",
                    typename = {{couche}},
                    srs = "EPSG:4326",
                    #MAXFEATURES= '50',
                    bbox = paste(geo_bbox[1],geo_bbox[2],geo_bbox[3],geo_bbox[4], sep=',')
                    #"-1.994019,49.181703,-0.865173,49.777717"
  )

  httr::build_url(url) %>%
    sf::read_sf(crs = 4326)

}
