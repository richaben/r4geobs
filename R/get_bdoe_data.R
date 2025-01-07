#' Fonction pour télécharger les données du module BDOE de GéObs.
#'
#' @param login texte. l'adresse mail d'accès à l'application GéObs.
#' @param mdp texte. le mot de passe associé.
#' @param nom_dossier texte. le nom du dossier où seront téléchargées les données.
#' @param lecture booléen. si TRUE, lit le fichier zip téléchargé.
#'
#' @return un fichier zip nommé \code{export_csv_total_bd.zip} contenant les données est téléchargé.
#'  Si le fichier zip est lu, retourne un objet sous forme de liste de dataframe.
#' @export
#'
#' @importFrom cli cli_alert_success col_blue
#' @importFrom httr write_disk
#' @importFrom rvest session html_form html_form_set session_submit session_jump_to
#'
#' @examples
#' \dontrun{
#' get_bdoe_data(login = "john.doe@ofb.gouv.fr",
#'               mdp = 'geobs_mon_amour',
#'               nom_dossier = "data_bdoe")
#'
#' bdoe_data <-
#'   get_bdoe_data(login = "john.doe@ofb.gouv.fr",
#'                 mdp = 'geobs_mon_amour',
#'                 nom_dossier = "data_bdoe",
#'                 lecture = TRUE)
#' }

get_bdoe_data <- function(login,
                          mdp,
                          nom_dossier = NULL,
                          lecture = FALSE){

  if(missing(login) | missing(mdp)){
    stop("login et mdp requis !")
  }

  if (missing(nom_dossier)) {
    if (!dir.exists("data_bdoe")) {dir.create("data_bdoe")}
    chemin_data <- file.path('data_bdoe', "export_csv_total_bd.zip")

  } else {
    if (!dir.exists(nom_dossier)) {dir.create(nom_dossier)}
    chemin_data <- file.path({nom_dossier}, "export_csv_total_bd.zip")

  }

  url_base <- "https://geobs.eaufrance.fr/geobs/login.htm"
  url_export <- "https://geobs.eaufrance.fr/geobs/export.action"

  signin.session <- rvest::session(url_base)

  signin.form <- rvest::html_form(signin.session)[[1]]

  filled.signin <- rvest::html_form_set(signin.form,
                                        `j_username` = login,
                                        `j_password` = mdp)

  signed.in <-
    rvest::session_submit(signin.session, filled.signin) %>%
    suppressWarnings()

  bdoe_session <- rvest::session_jump_to(signed.in, url_export)

  form_result <- rvest::html_form(bdoe_session)[[1]]

  rvest::session_submit(signed.in,
                        form_result,
                        submit="action:ddl",
                        httr::write_disk(chemin_data, overwrite = T))

  cli::cli_alert_success("Fichier t\u00e9l\u00e9charg\u00e9 ! \n{cli::col_blue({chemin_data})}")

  if(lecture == TRUE){

    bdoe_data <-
      read_zip_bdoe(normalizePath(chemin_data)) %>%
      suppressWarnings()

    return(bdoe_data)

  }
}



