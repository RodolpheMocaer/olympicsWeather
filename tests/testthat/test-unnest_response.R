#JEU DE DONNEES MINIMALE#

mock_data <- tibble::tibble(
  hourly = list(
    list("2024-02-24 12:00:00", "2024-02-24 13:00:00"),
    list(20, 22),
    list(18, 20),
    list(0.2, 0.3),
    list(0.5, 0.8)
  )
)
mock_data_df <- data.frame(do.call(cbind, mock_data$hourly))
colnames(mock_data_df) <- c("heure au tz UTC", "données de température", "données de température ressentie", "probabilité de pluie", "precipitation en mm")

#TEST UNITAIRE#
#BON NOMBRE DE LIGNE#
test_that("La fonction renvoie le bon nombre de lignes", {
  output_tibble <- unnest_response(mock_data)
  expect_equal(nrow(output_tibble), nrow(mock_data_df))
})

#BONNE TEMPERATURE#
test_that("La fonction renvoie le bon nombre de lignes", {
  output_tibble <- unnest_response(mock_data)
  expect_true(all(output_tibble[[2]] == mock_data_df[[2]]),
              info = "Les colonnes ne sont pas égales.")
})

#BON NOM DE COLONNE#
test_that("Les valeurs de la colonne temperature correspondent aux valeurs d'entrée", {
  output_tibble <- unnest_response(mock_data)
  expect_true(all.equal(names(output_tibble), names(mock_data_df)),
              info = "Les noms de colonnes ne sont pas égaux.")
})

#LE NOMBRE DE COLONNES#
test_that("Le nombre de colonnes en sortie est correct", {
  output_tibble <- unnest_response(mock_data)
  expect_equal(ncol(output_tibble), ncol(mock_data_df),
               info = "Le nombre de colonnes n'est pas égal.")
})
