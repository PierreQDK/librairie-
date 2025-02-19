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
  # Vérifier l'existence des colonnes essentielles
  required_columns <- c("Date.de.naissance", "Nom.de.l.élu", "Libellé.de.la.commune")
  if (!all(required_columns %in% colnames(df))) {
    stop("Erreur: Une ou plusieurs colonnes requises sont absentes")
  }

  # Vérifier si la colonne Date.de.naissance est vide après conversion
  if (nrow(df) == 0 || all(is.na(df$Date.de.naissance))) {
    stop("Erreur: Pas de dates valides dans la colonne 'Date.de.naissance'")
  }

  # 🔹 Extraire les informations essentielles
  nom_de_la_commune <- unique(df$Libellé.de.la.commune)
  if (length(nom_de_la_commune) > 1) {
    stop("Erreur: Plusieurs communes détectées")
  }
  print(paste("Nom de la commune:", nom_de_la_commune))

  # 🔹 Nombre d'élus
  Nbre_elu <- nrow(df)
  print(paste("Nombre d'élus:", Nbre_elu))

  # 🔹 Distribution des âges
  df$Age <- as.numeric(Sys.Date() - df$Date.de.naissance) %/% 365  # Calcul de l'âge
  distribution_age <- quantile(df$Age, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)

  print("Distribution des âges:")
  print(distribution_age)

  # 🔹 Trouver l'élu le plus âgé
  age_vieux <- max(df$Age, na.rm = TRUE)
  nom_vieux <- df$Nom.de.l.élu[df$Age == age_vieux]

  # Si plusieurs élus ont le même âge, en prendre un seul
  if (length(nom_vieux) > 1) {
    nom_vieux <- nom_vieux[1]
  }

  print(paste("Nom et âge de l'élu le plus âgé:", nom_vieux, "-", age_vieux, "ans"))

  # 🔹 Résumé final sous forme de liste pour affichage
  return(list(
    Commune = nom_de_la_commune,
    Nombre_elus = Nbre_elu,
    Distribution_age = distribution_age,
    Elu_plus_age = list(Nom = nom_vieux, Age = age_vieux)
  ))
}
