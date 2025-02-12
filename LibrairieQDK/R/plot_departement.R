#' Visualisation des professions des élus par département
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

plot_departement <- function(df){

  if (length(df) < 16) {
    stop("Le dataframe doit contenir 16 colonnes")
  }
  # je ne séléctionne que les lignes avec Maire(ex: maire-adjoint, Maire, ...)
  Nbre_Maire_ad <- grepl("Maire", df$Libellé.de.la.fonction, fixed  = TRUE)
  # je créer un nouveau data frame qui ne prendra que les occurences précédentes
  df_Maire_ad <- df[Nbre_Maire_ad,]

  Nbre_Maire_d <- !grepl("adjoint", df_Maire_ad$Libellé.de.la.fonction, fixed  = TRUE)

  df_Maire_d <- df_Maire_ad[Nbre_Maire_d,]

  Nbre_Maire <- !grepl("délégué", df_Maire_d$Libellé.de.la.fonction, fixed  = TRUE)

  df_Maire <- df_Maire_d[Nbre_Maire,]

  Nbre_co <- length(table(df$Libellé.de.la.commune))

  print(
    barplot(
      head(table(df_Maire$Code.de.la.catégorie.socio.professionnelle), n = 10),
      horiz = TRUE,
      xlab = "frqc d'apparition d'élu de la catégorie socio",
      ylab = "Libellé des 10 codes professionnels les plus représentés pour les départements",
      main = paste("Nombre de communes: ",Nbre_co,
                   "Département:", df[1, 2])

    )
  )

}
