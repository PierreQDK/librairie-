#' Résumé des informations d'une commune
#'
#' Cette fonction affiche un résumé des caractéristiques principales
#' d'une commune donnée, notamment son nom, le nombre d'élus,
#' la distribution des âges des élus, et l'âge de l'élu le plus âgé.
#'
#' @param x Un objet de type `data.frame` représentant une commune et contenant
#' les colonnes `Libellé.de.la.commune` ainsi que les informations sur les élus.
#'
#' @return Affiche un résumé des informations de la commune, notamment :
#' - Nom de la commune
#' - Nombre total d'élus
#' - Distribution des âges des élus
#' - Élu le plus âgé
#'
#' @export
#' @import dplyr
#' @examples
#' # Exemple avec deux jeux de données représentant des communes
#' summary_commune(df_Nantes)
#' summary_commune(df_Faverelles)
summary_commune <- function(df) {
  # Vérifier l'existence de la colonne avant conversion
  if (!"Date.de.naissance" %in% colnames(df)) {
    stop("Erreur: La colonne 'Date.de.naissance' est introuvable")
  }

  # Vérifier si la colonne est vide après conversion
  if (nrow(df) == 0 || all(is.na(df$Date.de.naissance))) {
    stop("Erreur: Pas de dates valides dans la colonne 'Date.de.naissance'")
  }

  # Identifier l'élu le plus âgé
  age_vieux <- min(df$Date.de.naissance, na.rm = TRUE)

  # Vérifier si age_vieux est bien calculé
  if (is.na(age_vieux)) {
    stop("Erreur: Impossible de déterminer l'âge le plus élevé.")
  }

  # Extraire le nom de l'élu
  nom_vieux <- df$Nom.de.l.élu[df$Date.de.naissance == age_vieux]

  # Vérifier si nom_vieux est vide
  if (length(nom_vieux) == 0) {
    stop("Erreur: Aucun élu trouvé pour l'âge le plus vieux.")
  }

  # Vérifier que nom_vieux n'est pas une liste
  nom_vieux <- as.character(nom_vieux[1])  # Prendre le premier nom s'il y en a plusieurs

  cat("Nom et âge de l'élu le plus âgé:", nom_vieux, "-", as.numeric(Sys.Date() - age_vieux) %/% 365, "ans\n")
}


