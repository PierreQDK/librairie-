#' Une fonction pour avoir un résumé des informations d'un département
#'
#' Cette fonction génère un graphique en barres horizontales illustrant
#' la répartition des élus selon leur catégorie socio-professionnelle
#' pour un département donné.
#'
#' @param df Un data frame contenant les informations des élus, incluant
#' les colonnes `Libellé.du.département`, `Libellé.de.la.commune`,
#' et `Code.de.la.catégorie.socio.professionnelle`.
#'
#' @return Un graphique en barres représentant les 10 catégories socio-professionnelles
#' les plus fréquentes parmi les élus du département.
#' @export
#' @import dplyr ggplot2
#' @examples
#' # Exemple avec un jeu de données spécifique
#' plot_departement(df_Loire_Atlantique)
#' plot_departement(df_Gers)

summary_departement <- function(df) {
  # Vérification de l'existence de la colonne "Date.de.naissance"
  if (!"Date.de.naissance" %in% colnames(df)) {
    stop("Erreur: La colonne 'Date.de.naissance' est absente.")
  }

  # Vérifier s'il y a des données
  if (nrow(df) == 0) {
    stop("Erreur: Aucune donnée disponible pour ce département.")
  }

  # Extraction du nom du département
  nom_du_departement <- unique(df$Libellé.du.département)
  if (length(nom_du_departement) > 1) {
    stop("Erreur: Plusieurs départements détectés.")
  }

  # S'assurer que nom_du_departement est un vecteur de caractères
  nom_du_departement <- as.character(nom_du_departement)

  # Nombre de communes
  nbre_communes <- length(unique(df$Libellé.de.la.commune))

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

  # Trouver l'élu le plus jeune
  age_jeune <- min(df$Age, na.rm = TRUE)
  nom_jeune <- df$Nom.de.l.élu[df$Age == age_jeune]

  # Prendre le premier nom si plusieurs élus ont le même âge
  if (length(nom_jeune) > 1) {
    nom_jeune <- nom_jeune[1]
  }

  # Vérification avant return
  print("Vérification des valeurs avant le return :")
  print(nom_du_departement)
  print(nbre_communes)
  print(distribution_age)
  print(nom_vieux)
  print(age_vieux)
  print(nom_jeune)
  print(age_jeune)

  # Retourner sous forme de liste
  return(list(
    Departement = nom_du_departement,
    Nombre_communes = nbre_communes,
    Distribution_age = distribution_age,
    Elu_plus_age = list(Nom = nom_vieux, Age = age_vieux),
    Elu_plus_jeune = list(Nom = nom_jeune, Age = age_jeune)
  ))
}
