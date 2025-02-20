#' Génère un rapport pour une commune et un département spécifiés.
#'
#' @param commune Une chaîne de caractères ou un vecteur de communes pour lesquelles générer le rapport.
#' @param departement Une chaîne de caractères ou un vecteur de départements pour lesquels générer le rapport.
#' @param output Le chemin complet du fichier où le rapport sera enregistré.
#'
#' @return Aucun retour. Le rapport est généré et sauvegardé à l'emplacement spécifié.
#' @importFrom quarto quarto_render
#' @export

generer_rapport <- function(commune, departement, output) {
  # Vérifier que Quarto est installé
  if (!requireNamespace("quarto", quietly = TRUE)) {
    stop("Le package 'quarto' doit être installé pour générer le rapport.")
  }

  # Localisation du fichier Quarto dans le package
  rapport_path <- system.file("Rapport.qmd", package = "LibrairieQDK")

  if (rapport_path == "") {
    stop("Le fichier rapport.qmd est introuvable dans le package.")
  }

  # Création d'un fichier temporaire pour stocker le rapport
  temp_report <- tempfile(fileext = ".qmd")

  # Copier le fichier modèle vers le fichier temporaire
  file.copy(rapport_path, temp_report, overwrite = TRUE)

  # Génération du rapport avec Quarto
  quarto::quarto_render(
    input = system.file("inst/Rapport.qmd", package = "LibrairieQDK"),
    output_format = "html",
    execute_params = list(
      code_commune = commune,
      code_departement = departement
    ),
    output_file = output
  )


  message("Le rapport a été généré avec succès : ", output)
}
