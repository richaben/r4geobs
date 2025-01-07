#' Récupérer les noms des attributs d'une couche geobs sur le flux WFS
#'
#' @param nom_couche texte. nom de la couche
#'
#' @returns un vecteur de noms des attributs de la couche interrogée.
#' @export
#'
#' @importFrom httr parse_url build_url GET content
#' @importFrom xml2 read_xml xml_find_all xml_attr
#'
#' @examples
#' \dontrun{
#' get_geobs_wfs_attributs("ROE_MONDE")
#' }

get_geobs_wfs_attributs <- function(nom_couche){

  url_geobs <- "http://mapsref.brgm.fr/wxs/onema/geobs_monde"
  url <- httr::parse_url(url_geobs)

  url$query <- list( service = "WFS",
                     version = "1.0.0",
                     request = "DescribeFeatureType",
                     typeName = nom_couche
  )

  attributs <-
  httr::build_url(url) %>%
    httr::GET() %>%
    httr::content(as = "text") %>%
    xml2::read_xml() %>%
    xml2::xml_find_all("//xsd:element") %>%
    xml2::xml_attr("name")

  return(attributs[-1])
}

