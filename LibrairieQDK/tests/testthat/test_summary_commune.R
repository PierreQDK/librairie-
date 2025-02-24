library(testthat)

test_that("summary_commune fonctionne avec une entrée valide", {
  df <- data.frame(
    Libellé.de.la.commune = rep("Paris", 5),
    Libellé.de.la.fonction = c("Maire", "Adjoint", "Conseiller", "Maire", "Adjoint"),
    Nom.de.l.élu = c("Dupont", "Martin", "Durand", "Lefevre", "Morel"),
    Prénom.de.l.élu = c("Jean", "Marie", "Paul", "Luc", "Pierre"),
    Date.de.naissance = as.Date(c("1950-01-01", "1960-01-01", "1970-01-01", "1980-01-01", "1990-01-01")),
    stringsAsFactors = FALSE
  )
  df <- cbind(df, matrix(ncol = 11, nrow = 5))  # Pour avoir 16 colonnes
  expect_output(summary_commune(df))
})
