#' Visualiser la répartition des élus par code professionnel pour une commune
#'
#' Cette fonction génère un graphique en barres horizontales illustrant
#' le nombre d'élus pour chaque code socio-professionnel d'une commune donnée.
#'
#' @param df Un data frame contenant au minimum les colonnes :
#' - `Libellé.de.la.commune` (nom de la commune),
#' - `Libellé.du.département` (nom du département),
#' - `Code.de.la.catégorie.socio.professionnelle` (code professionnel des élus).
#'
#' @return Un graphique en barres représentant la répartition des élus par code professionnel dans la commune.
#' @export
#' @import dplyr ggplot2
#' @examples
#' # Exemple avec deux jeux de données représentant des communes
#' lapply(list(df_Nantes, df_Faverelles), plot_commune)
plot_commune <- function(df){

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

  print(
    barplot(
      table(df_Maire$Code.de.la.catégorie.socio.professionnelle),
      horiz = TRUE,
      xlab = "frqc d'apparition d'élu de la catégorie socio",
      ylab = "Libellé des codes professionnels pour les élus",
      main = paste("Commune: ",df[1, 6], "Département:", df[1, 2])

    )
  )

}
