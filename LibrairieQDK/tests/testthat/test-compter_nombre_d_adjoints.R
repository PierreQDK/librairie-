test_that("compter_nombre_d_adjoints fonctionne avec une entrée valide", {
  df <- data.frame(
    Libellé.de.la.fonction = c("Maire-adjoint", "Adjoint", "Conseiller", "adjoint"),
    stringsAsFactors = FALSE
  )
  df <- cbind(df, matrix(ncol = 15, nrow = 4))  # S'assurer qu'il y a 16 colonnes
  result <- compter_nombre_d_adjoints(df)
  expect_equal(result, 2)  # Vérifier que le résultat est bien 2
})
