#' Fonction pour checker la présence de photos sur un ouvrage identifié avec son numéro.
#'
#' @param id_ouvrage Numéro de l'ouvrage ROE
#' @param session Résultat de la connexion/session à GéObs
#'
#' @returns un dataframe avec la présence et le nombre de photos pour l'ouvrage consulté
#' @export
#'
#' @importFrom glue glue
#' @importFrom rvest session_jump_to read_html html_elements
#'
#' @examples
#' \dontrun{
#'
#' ## initier la session geobs
#' ma_session <- init_geobs_session(
#'   login = "john.doe@ofb.gouv.fr",
#'   mdp = "geobs_mon_amour")
#'
#' ## checker pour l'ouvrage "ROE39268"
#' check_photos_roe_data(id_ouvrages = 39268, session = ma_session)
#' }
#'
check_photos_roe_data <- function(id_ouvrage, session) {

  url_fiche <-
    glue::glue("https://geobs.eaufrance.fr/geobs/ficheroe/ficheDescriptionGenerale.action?&id={id_ouvrage}&table=obstacle_referentiel")

  tryCatch({
    fiche_session <-
      rvest::session_jump_to(session, url_fiche)

    photos_list <-
      rvest::read_html(fiche_session) %>%
      rvest::html_elements("section[data-section='images']")

    photos_list <-
      photos_list[2] %>%
      rvest::html_elements("li[id^='listeImage_']")

    nb_photos <- length(photos_list)
    presence_photos <- nb_photos > 0

    result <- data.frame(
      id_ouvrage = id_ouvrage,
      presence_photos = presence_photos,
      nb_photos = nb_photos,
      session = "OK",
      stringsAsFactors = FALSE
    )

    return(result)

  }, error = function(e) {
    return(data.frame(
      id_ouvrage = id_ouvrage,
      presence_photos = NA,
      nb_photos = NA,
      session = paste("ERREUR:", e$message),
      stringsAsFactors = FALSE
    ))
  })
}


