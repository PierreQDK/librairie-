#' Une fonction pour compter le nombre d'adjoint dans une base de donnée
#' Cette fonction calcule le nombre d'élus ayant la fonction d'adjoint
#' dans un data frame contenant des informations sur les élus municipaux.
#'
#' @param df Un data frame contenant au minimum la colonne :
#' - `Libellé.de.la.fonction` (qui indique la fonction de l'élu).
#'
#' @return Un entier représentant le nombre d'élus ayant une fonction contenant le mot "adjoint".
#' @export
#' @import stringr
#' @examples
#' # Exemple avec une liste de data frames représentant plusieurs communes
#' sapply(df_list, compter_nombre_d_adjoints)
#'
compter_nombre_d_adjoints <- function(df){

  if (length(df) != 16) {
    stop("Le dataframe doit contenir 16 colonnes")
  }

  colonnes_requises <- c("Libellé.de.la.fonction")
  if (!all(colonnes_requises %in% colnames(df))) {
    stop("Le dataframe doit contenir la colonne : 'Libellé.de.la.fonction'")
  }
  # renvoie un vecteur avec TRUE ou FALSE en fonction de l'apparition ou non
  adjoint_TRUE <- grepl("adjoint", df$Libellé.de.la.fonction)

  Nbre_adjoint <- df[adjoint_TRUE, ]

  return(nrow(Nbre_adjoint))

}
