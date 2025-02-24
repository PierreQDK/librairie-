test_that("plot_departement fonctionne sans erreur", {
  df <- data.frame(
    Libellé.de.la.fonction = c("Maire", "Adjoint", "Conseiller"),
    Code.de.la.catégorie.socio.professionnelle = c("A", "B", "C"),
    Libellé.du.département = rep("Ile-de-France", 3),
    stringsAsFactors = FALSE
  )
  df <- cbind(df, matrix(ncol = 13, nrow = 3))
  expect_invisible(plot_departement(df))
})
