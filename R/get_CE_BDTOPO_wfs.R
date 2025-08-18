#' Récupérer les Cours d'eau de la BDTOPO IGN sous forme spatiale
#'
#' @param geo objet spatial de type 'sf'
#'
#' @returns un data.frame au format 'sf'
#' @export
#'
#' @importFrom dplyr mutate across
#' @importFrom httr parse_url GET build_url content
#' @importFrom purrr map_df
#' @importFrom sf st_bbox read_sf
#' @importFrom xml2 read_xml xml_attr
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' get_CE_BDTOPO_wfs(geo = shape_area)
#' }
#'

get_CE_BDTOPO_wfs <- function(geo){

  geo_bbox <- sf::st_bbox(geo)
  bbox_string <- paste0(geo_bbox[c("ymin","xmin","ymax","xmax")], collapse = ",")

  base_url <- "https://data.geopf.fr/wfs/ows"
  parsed <- httr::parse_url(base_url)

  base_params <- list(
    service = "wfs",
    version = "2.0.0",
    REQUEST = "GetFeature",
    TYPENAME = "BDTOPO_V3:cours_d_eau",
    outputFormat = "application/json",
    BBOX = bbox_string
  )

  # resultType = "hits" (api limited to 5000 rows)
  count_parsed <- parsed
  count_parsed$query <- c(base_params, list(resultType = "hits"))
  count_resp <- httr::GET(httr::build_url(count_parsed))

  count_xml <- xml2::read_xml(httr::content(count_resp, as = "text", encoding = "UTF-8"))
  total_features <- suppressWarnings(as.integer(xml2::xml_attr(count_xml, "numberMatched")))

  # if <5000
  if (total_features <= 5000) {
    one_parsed <- parsed
    one_parsed$query <- base_params
    return(sf::read_sf(httr::build_url(one_parsed)))
  }

  # if >5000
  max_features <- 5000
  start_indices <- seq(0, (ceiling(total_features / max_features) - 1) * max_features, by = max_features)

  purrr::map_df(start_indices, function(start_index) {
    page_parsed <- parsed
    page_parsed$query <- c(base_params, list(count = max_features, startIndex = start_index))

    httr::build_url(page_parsed) %>%
      sf::read_sf() %>%
      dplyr::mutate(dplyr::across(!.data$geometry, .fns = as.character))
  })

}


