#' Résumé des informations d'un département
#'
#' Cette fonction affiche un résumé des caractéristiques principales
#' d'un département donné, notamment son nom, le nombre de communes,
#' la distribution des âges des élus, l'élu le plus âgé et l'élu le plus jeune.
#'
#' @param df Un `data.frame` contenant les informations des élus du département.
#' Il doit inclure les colonnes `Libellé.du.département`, `Libellé.de.la.commune`,
#' `Nom.de.l.élu` et `Date.de.naissance`.
#'
#' @return Une liste contenant :
#' \itemize{
#'   \item \strong{Departement} : Nom du département analysé.
#'   \item \strong{Nombre_communes} : Nombre total de communes dans le département.
#'   \item \strong{Distribution_age} : Quartiles de la distribution des âges des élus.
#'   \item \strong{Elu_plus_age} : Liste contenant le nom et l'âge de l'élu le plus âgé.
#'   \item \strong{Elu_plus_jeune} : Liste contenant le nom et l'âge de l'élu le plus jeune.
#' }
#'
#' @details
#' La fonction commence par vérifier que les colonnes requises existent et que les
#' données sont valides. Elle extrait ensuite le nom du département et le nombre de
#' communes. Les âges des élus sont calculés à partir de leur date de naissance,
#' et leur distribution est obtenue via les quartiles. Enfin, les élus les plus âgés
#' et les plus jeunes sont identifiés.
#'
#' @examples
#' # Exemple de jeu de données
#' df_exemple <- data.frame(
#'   Libellé.du.département = rep("Loire-Atlantique", 10),
#'   Libellé.de.la.commune = c("Nantes", "Saint-Nazaire", "Rezé", "Orvault", "Carquefou",
#'                              "Pornic", "Châteaubriant", "La Baule", "Savenay", "Guérande"),
#'   Nom.de.l.élu = c("Jean Dupont", "Marie Curie", "Albert Camus", "Simone Veil",
#'                    "Victor Hugo", "Molière", "Pasteur", "Flaubert", "Balzac", "Rousseau"),
#'   Date.de.naissance = as.Date(c("1960-05-15", "1975-08-20", "1982-03-10", "1950-11-25",
#'                                 "1990-07-01", "1988-02-12", "1945-06-30", "2000-10-05",
#'                                 "1985-11-12", "1978-03-22"))
#' )
#'
#' # Exécution de la fonction
#' summary_departement(df_exemple)
#'
#' @export

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
