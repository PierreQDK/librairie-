test_that("plot_commune fonctionne sans erreur", {
  df <- data.frame(
    Libellé.de.la.fonction = c("Maire", "Adjoint", "Conseiller"),
    Code.de.la.catégorie.socio.professionnelle = c("A", "B", "C"),
    Libellé.de.la.commune = rep("Paris", 3),
    stringsAsFactors = FALSE
  )
  df <- cbind(df, matrix(ncol = 13, nrow = 3))
  expect_invisible(plot_commune(df))
})
