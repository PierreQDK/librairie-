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

summary_departement <- function(x) {

  # Vérification de la validité des données
  if (length(table(x$Libellé.de.la.commune)) == 1) {
    stop("L'objet doit contenir plus d'une commune")
  }

  if (length(table(x$Libellé.du.département)) != 1) {
    stop("L'objet doit contenir 1 département unique")
  }

  nom_du_departement <- as.character(x[1,2])

  nbre_communes <- length(table(x$Libellé.de.la.commune))

  Nbre_elu <- sum(grepl("Maire", x$Libellé.de.la.fonction, fixed  = TRUE))

  vect_distribution_age <- calcul_distribution_age(x)

  x$Date.de.naissance <- as.Date(x$Date.de.naissance, format = "%d/%m/%Y")
  x$Age <- as.numeric(difftime(Sys.Date(), x$Date.de.naissance, units = "weeks")) %/% 52.25


  jeune <- x[which.min(x$Age), c("Nom.de.l.élu", "Age", "Libellé.de.la.commune")]
  jeune_nom <- as.character(jeune[1, 1])
  jeune_Age <- as.character(jeune[1, 2])
  jeune_commune <- as.character(jeune[1, 3])

  vieux <- x[which.max(x$Age), c("Nom.de.l.élu", "Age", "Libellé.de.la.commune")]
  vieux_nom <- as.character(vieux[1, 1])
  vieux_Age <- as.character(vieux[1, 2])
  vieux_commune <- as.character(vieux[1, 3])

  # Renvoie un df avec les moyennes par communes
  moyenne_par_commune <- aggregate(Age ~ Libellé.de.la.commune, data = x, FUN = mean)
  haute_moy_age <- moyenne_par_commune[which.max(moyenne_par_commune$Age),]
  faible_moy_age <- moyenne_par_commune[which.min(moyenne_par_commune$Age),]

  commune_haute <- x[grepl(haute_moy_age[1, 1], x$Libellé.de.la.commune), ]
  distribution_age_haute <- calcul_distribution_age(commune_haute)

  commune_faible <- x[grepl(faible_moy_age[1, 1], x$Libellé.de.la.commune), ]
  distribution_age_faible <- calcul_distribution_age(commune_faible)

  # Affichage des résultats
  cat("Nom du département:", nom_du_departement, "\n",
      "Nombre de communes: ", nbre_communes, "\n",
      "Nombre d'élus: ", Nbre_elu, "\n")

  cat("Distribution des âges: ", "min", vect_distribution_age[1],
      ", 25% à", vect_distribution_age[2],
      ", 50% à", vect_distribution_age[3],
      ", 75% à", vect_distribution_age[4],
      ", 100% à", vect_distribution_age[5],"\n",
      "le plus ancien:", vieux_nom, "\n", vieux_Age,"ans", "\n", vieux_commune, "\n",
      "le plus récent:", jeune_nom, "\n", jeune_Age,"ans", "\n", jeune_commune, "\n")

  cat("Commune avec plus haute moyenne d'âge: ",as.character(haute_moy_age[1, 1]), round(haute_moy_age[1, 2]), "ans", "\n",
      "Distribution des âges: ", "min", distribution_age_haute[1],
      ", 25% à", distribution_age_haute[2],
      ", 50% à", distribution_age_haute[3],
      ", 75% à", distribution_age_haute[4],
      ", 100% à", distribution_age_haute[5],"\n")

  cat("Commune avec plus faible moyenne d'âge: ",as.character(faible_moy_age[1, 1]), round(faible_moy_age[1, 2]), "ans", "\n",
      "Distribution des âges: ", "min", distribution_age_faible[1],
      ", 25% à", distribution_age_faible[2],
      ", 50% à", distribution_age_faible[3],
      ", 75% à", distribution_age_faible[4],
      ", 100% à", distribution_age_faible[5],
      "\n", "\n"
  )

}
