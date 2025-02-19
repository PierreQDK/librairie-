#' Génère un rapport pour une commune et un département spécifiés.
#'
#' @param commune Une chaîne de caractères ou un vecteur de communes pour lesquelles générer le rapport.
#' @param departement Une chaîne de caractères ou un vecteur de départements pour lesquels générer le rapport.
#' @param output Le chemin complet du fichier où le rapport sera enregistré.
#'
#' @return Aucun retour. Le rapport est généré et sauvegardé à l'emplacement spécifié.
#'
#' @export
generer_rapport <- function(commune, departement, output) {
  # Charger le fichier .qmd depuis le dossier 'inst'
  rapport_template <- system.file("inst", "rapport.qmd", package = "LibrairieQDK")

  # Vérification de la présence du fichier .qmd
  if (rapport_template == "") {
    stop("Le fichier rapport.qmd n'a pas été trouvé dans le dossier inst.")
  }

  # Paramétrer les valeurs pour la génération du rapport
  params <- list(commune = commune, departement = departement)

  # Utiliser Quarto pour compiler le rapport
  quarto::quarto_render(rapport_template, output_file = output, params = params)

  # Message de confirmation
  message("Le rapport a été généré et sauvegardé dans : ", output)
}
