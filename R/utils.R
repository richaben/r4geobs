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
