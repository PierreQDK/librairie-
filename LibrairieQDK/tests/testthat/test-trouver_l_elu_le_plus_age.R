library(testthat)
test_that("trouver_l_elu_le_plus_age renvoie l'élu correct", {
  df <- data.frame(Nom.de.l.élu = c("A", "B", "C"),
                   Prénom.de.l.élu = c("X", "Y", "Z"),
                   Date.de.naissance = c("01/01/1950", "01/01/1970", "01/01/1960"),
                   stringsAsFactors = FALSE)
  df <- cbind(df, matrix(ncol = 13, nrow = 3))
  result <- trouver_l_elu_le_plus_age(df)
  expect_equal(result$Nom.de.l.élu, "A")
})

test_that("trouver_l_elu_le_plus_age renvoie une erreur si la colonne de naissance est absente", {
  df <- data.frame(Nom.de.l.élu = c("A", "B", "C"))
  expect_error(trouver_l_elu_le_plus_age(df))
})
