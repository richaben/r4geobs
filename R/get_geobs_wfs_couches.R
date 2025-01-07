#' Récupérer le nom des couches disponibles dans le flux WFS Geobs
#'
#' @returns un vecteur de noms de couches disponibles.
#' @export
#'
#' @examples
#' \dontrun{
#' get_geobs_wfs_couches()
#' }

get_geobs_wfs_couches <- function(){

  url_geobs <- "http://mapsref.brgm.fr/wxs/onema/geobs_monde"
  url <- httr::parse_url(url_geobs)

  url$query <- list(service = "wfs",
                    version = "1.0.0", # optional
                    request = "GetCapabilities"
  )

  nom_couches <-
    httr::build_url(url) %>%
    httr::GET() %>%
    httr::content(as = "text") %>%
    xml2::read_xml() %>%
    xml2::xml_find_all("//d1:Name") %>%
    xml2::xml_text()

  return(nom_couches[-1])
}
