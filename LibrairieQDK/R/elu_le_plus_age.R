#' Une fonction pour trouver l'élu le plus agé dans un data frame
#' Cette fonction identifie et affiche l'élu le plus âgé dans un
#' data frame contenant des informations sur les élus.
#'
#' @param df Un data frame contenant au minimum les colonnes :
#' - `Nom.de.l.élu`
#' - `Prénom.de.l.élu`
#' - `Date.de.naissance`
#'
#' @return Une chaîne de caractères indiquant le nom, prénom et âge de l'élu le plus âgé.
#' @export
#' @import dplyr lubridate
#' @examples
#' # Exemple avec une liste de data frames
#' sapply(df_list, trouver_l_elu_le_plus_age)


trouver_l_elu_le_plus_age <- function(df){

  if (length(df) != 16) {
    stop("Le dataframe doit contenir 16 colonnes")
  }

  df$Date.de.naissance <- as.Date(df$Date.de.naissance, format = "%d/%m/%Y")

  df[which.min(df$Date.de.naissance),c("Nom.de.l.élu", "Prénom.de.l.élu", "Date.de.naissance")]

}
