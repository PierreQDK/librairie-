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
summary.commune <- function(x){

  if (length(table(x$Libellé.de.la.commune)) != 1) {
    stop("L'objet doit contenir 1 communes")
  }


  nom_de_la_commune <- x[1,6]

  # je sors un vect avec TRUE pour chaque occurence de Maire et je compte nbre TRUE
  Nbre_elu <- sum(grepl("Maire", x$Libellé.de.la.fonction, fixed  = TRUE))

  nom_vieux <- trouver_l_elu_le_plus_age(x)[1, 1]
  age_vieux <- calcul_distribution_age(x)[5]

  distribution_age <- calcul_distribution_age(x)

  cat("Nom de la commune: ",nom_de_la_commune, "\n")
  cat("Nombre d'élus: ",Nbre_elu, "\n")
  cat("Distribution des âges: ", "min", distribution_age[1],
      ", 25% à", distribution_age[2],
      ", 50% à", distribution_age[3],
      ", 75% à", distribution_age[4],
      ", 100% à", distribution_age[5],"\n")
  cat("Nom et âge de l'élu vieux comme le monde: ",nom_vieux, age_vieux,"ans", "\n", "\n")
}

