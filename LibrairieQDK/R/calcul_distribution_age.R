#' Une fonction pour avoir la distribution de l'âge dans un data.frame
#'
#' Cette fonction calcule la distribution des âges des élus en renvoyant
#' les quantiles (minimum, quartiles et maximum) des âges des élus d'un data frame donné.
#'
#' @param df Un data frame contenant au minimum la colonne :
#' - `Date.de.naissance` (indiquant la date de naissance des élus).
#'
#' @return Un vecteur numérique contenant les quantiles des âges des élus :
#' - 0% (minimum)
#' - 25% (premier quartile)
#' - 50% (médiane)
#' - 75% (troisième quartile)
#' - 100% (maximum)
#' @export
#' @import lubridate
#' @examples
#' # Exemple avec une liste de data frames représentant plusieurs communes
#' sapply(df_list, calcul_distribution_age)


calcul_distribution_age <- function(df){

  if (length(df) < 16) {
    stop("Le dataframe doit contenir 16 colonnes")
  }


  df$Date.de.naissance <- as.Date(df$Date.de.naissance, format = "%d/%m/%Y")

  df$Age <- as.numeric(difftime(Sys.Date(), df$Date.de.naissance, units = "weeks")) %/% 52.25
  # Sys.date pour la date du jour, difftime() pour comparer 2 dates dans l'unité voulu,
  # division entière par le nbre moyen de semaines

  quantile(df$Age, probs = c(0, 0.25, 0.5, 0.75, 1))

}
