#' Une fonction pour avoir un graphique du nombre des différents codes professionels
#'
#' Cette fonction génère un graphique en barres horizontales représentant
#' le nombre d'élus pour chaque code socio-professionnel.
#'
#' @param df Un data frame contenant au minimum la colonne :
#' - `Code.de.la.catégorie.socio.professionnelle` (indiquant la profession de chaque élu).
#'
#' @return Un graphique en barres montrant la répartition des élus par code socio-professionnel.
#' @export
#' @import dplyr ggplot2
#'

plot_code_professions <- function(df){

  if (length(df) != 16) {
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
      ylab = "code catégorie socio",
      main = paste("Département:", df[1, 2])

    )
  )

}
