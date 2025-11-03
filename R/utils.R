####-----------------------------####
### Fonctions utiles
####-----------------------------####

#' Fonction pour lire le contenu de l'archive zip contenant les données BDOE
#'
#' @noRd
#' @description Adaptation de la fonction de Cédric M.
#'
#' @param zipfile texte. Chemin de l'archive
#' @param ...
#'
#'
#'
read_zip_bdoe <- function(zipfile, ...){

  temp <- tempfile()

  archive::archive_extract(zipfile, dir = temp)

  filepath <- list.files(
    path = temp,
    full.names = TRUE,
    recursive = TRUE
  )

  filename <-
    filepath %>%
    (function(x) {
      x %>%
        basename() %>%
        gsub(pattern = ".csv",
             replacement = "")
    })

  bdoe_csv <-
    purrr::map(
    .x = filepath,
    .f = function(x) {
      #read.csv2(x)
      vroom::vroom(x,
                   guess_max = 2500,
                   show_col_types = FALSE,
                   progress = FALSE
      )
      }
  ) %>%
    purrr::set_names(filename)

  unlink(temp)

  return(bdoe_csv)

}

#' Fonction pour se connecter à Geobs
#'
#' @noRd
#' @description Adaptation de la fonction de Cédric M.
#'
#' @param login texte. Login (email) compte Geobs
#' @param mdp texte. Mot de passe compte Geobs
#'
#' @importFrom rvest session html_form html_form_set session_submit
#'
#' @examples
#' \dontrun{
#' init_geobs_session(login, mdp)
#' }
#'
init_geobs_session <- function(login, mdp) {

  url_base <- "https://geobs.eaufrance.fr/geobs/login.htm"

  signin.session <- rvest::session(url_base)
  signin.form <- rvest::html_form(signin.session)[[1]]

  filled.signin <- rvest::html_form_set(signin.form,
                                        `j_username` = login,
                                        `j_password` = mdp)

  signed.in <-
    rvest::session_submit(signin.session, filled.signin) %>%
    suppressWarnings()

  return(signed.in)
}
