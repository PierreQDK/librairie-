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
  # Vérification de l'existence de la colonne "Date.de.naissance"
  if (!"Date.de.naissance" %in% colnames(df)) {
    stop("Erreur: La colonne 'Date.de.naissance' est absente.")
  }

  # Vérifier s'il y a des données
  if (nrow(df) == 0) {
    stop("Erreur: Aucune donnée disponible pour cette commune.")
  }

  # Extraction du nom de la commune
  nom_de_la_commune <- unique(df$Libellé.de.la.commune)
  if (length(nom_de_la_commune) > 1) {
    stop("Erreur: Plusieurs communes détectées.")
  }

  # Nombre d'élus
  Nbre_elu <- nrow(df)

  # Vérifier le format de la colonne "Date.de.naissance"
  if (!inherits(df$Date.de.naissance, "Date")) {
    df$Date.de.naissance <- as.Date(df$Date.de.naissance, format = "%Y-%m-%d")
  }

  # Vérifier s'il y a des dates valides
  if (all(is.na(df$Date.de.naissance))) {
    stop("Erreur: Aucune date de naissance valide.")
  }

  # Calculer l'âge des élus
  df$Age <- as.numeric(Sys.Date() - df$Date.de.naissance) %/% 365

  # Vérifier la distribution des âges
  distribution_age <- quantile(df$Age, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)

  # Trouver l'élu le plus âgé
  age_vieux <- max(df$Age, na.rm = TRUE)
  nom_vieux <- df$Nom.de.l.élu[df$Age == age_vieux]

  # Prendre le premier nom si plusieurs élus ont le même âge
  if (length(nom_vieux) > 1) {
    nom_vieux <- nom_vieux[1]
  }

  # Vérification avant return
  print("Vérification des valeurs avant le return :")
  print(nom_de_la_commune)
  print(Nbre_elu)
  print(distribution_age)
  print(nom_vieux)
  print(age_vieux)

  # Retourner sous forme de liste
  return(list(
    Commune = nom_de_la_commune,
    Nombre_elus = Nbre_elu,
    Distribution_age = distribution_age,
    Elu_plus_age = list(Nom = nom_vieux, Age = age_vieux)
  ))
}
