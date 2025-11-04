#' Fonction pour initier une session de connexion à Geobs
#'
#' @description Initie une connexion à l'application Géobs.
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
