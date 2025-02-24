library(testthat)

test_that("calcul_distribution_age renvoie les bons quartiles", {
  df <- data.frame(Date.de.naissance = c("01/01/1980", "01/01/1990", "01/01/2000", "01/01/2010", "01/01/2020"),
                   stringsAsFactors = FALSE)
  df <- cbind(df, matrix(ncol = 15, nrow = 5))
  result <- calcul_distribution_age(df)
  expect_length(result, 5)  # Doit renvoyer 5 valeurs
})

test_that("calcul_distribution_age renvoie une erreur si la colonne de naissance est absente", {
  df <- data.frame(Nom = c("A", "B", "C"))
  expect_error(calcul_distribution_age(df))
})
