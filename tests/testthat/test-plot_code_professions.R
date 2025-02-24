test_that("plot_code_professions fonctionne sans erreur", {
  df <- data.frame(
    Libellé.de.la.fonction = c("Maire", "Maire-adjoint", "Maire", "Adjoint"),
    Code.de.la.catégorie.socio.professionnelle = c("A", "B", "C", "D"),
    stringsAsFactors = FALSE
  )
  df <- cbind(df, matrix(ncol = 14, nrow = 4))  # Pour arriver à 16 colonnes
  expect_invisible(plot_code_professions(df))
})
