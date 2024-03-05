#LIBRARY#

library(tibble)
library(lubridate)
library(tidygeocoder)
library(leaflet)
library(sf)
library(testthat)
library(devtools)
library(httr2)
library(jsonlite)

##Question 7##
#' Extraction de donnees meteorologiques d'un point donne grace à ses coordonnees.
#'
#' @param latitude coordonnees gps de la latitude
#' @param longitude coordonnees gps de la longitude
#'
#' @export
#' @import tibble
#' @import lubridate
#' @import testthat
#' @import devtools
#' @import httr2
#' @import jsonlite
#' @import tidygeocoder
#'
#' @examples
#' #Exemple d'utilisation :
#'
#' Resultat <- perform_request(37.7749, -122.4194)
#' View(Resultat)
perform_request <- function(latitude, longitude){
  url <- "https://api.open-meteo.com/v1/forecast"
  request(url) |>
    req_url_query(latitude = latitude,
                  longitude = longitude,
                  hourly= c("temperature_2m",
                            "apparent_temperature",
                            "precipitation_probability",
                            "precipitation"),
                  .multi = "comma") |>
    req_perform() |>
    resp_body_json() |>
    as_tibble()
}

##Question 8##
#' Formatage des donnees recueillis grace à la fonction perform_request
#'
#' @param Extraction  Vecteur de coordonnees de type (latitude, longitude) apres application de la fonction perform_request.
#'
#' @return Un tibble contenant les données des previsions meteo, tel que :
#'                  - L'heure au tz UTC
#'                  - Les donnees de température en Celsuis
#'                  - Les donnees de température ressentie en en Celsuis
#'                  - La probabilité de pluie en en Pourcentage
#'                  - La precipitation en mm
#' @export
#' @import tibble
#' @import lubridate
#' @import testthat
#' @import devtools
#' @import httr2
#' @import jsonlite
#' @import tidygeocoder
#'
#' @examples
#' #Exemple d'utilisation :
#'
#' Extraction <- perform_request(37.7749, -122.4194)
#' Resultat <- unnest_response(Extraction)
#' View(Resultat)
unnest_response <- function(Extraction){
  hourly_data <- Extraction$hourly
  if (length(hourly_data) == 0) {
    stop("Aucune donnée dans la colonne 'hourly'.")
  }
  output_tibble <- tibble(
    "heure au tz UTC" = ymd_hm(unlist(hourly_data[[1]])),
    "données de température" = unlist(hourly_data[[2]]),
    "données de température ressentie" = unlist(hourly_data[[3]]),
    "probabilité de pluie" = unlist(hourly_data[[4]]),
    "precipitation en mm" = unlist(hourly_data[[5]])
  )
}

##Question 10##
#' Extraction de donnees à partir d'une adresse grace aux fonctions precedentes.
#'
#' @param adresse Vecteur de type character.
#'
#' @return Un tibble contenant les données des previsions meteo, tel que :
#'                  - L'heure au tz UTC
#'                  - Les donnees de température en Celsuis
#'                  - Les donnees de température ressentie en en Celsuis
#'                  - La probabilité de pluie en en Pourcentage
#'                  - La precipitation en mm
#' @export
#' @import tibble
#' @import lubridate
#' @import testthat
#' @import devtools
#' @import httr2
#' @import jsonlite
#' @import tidygeocoder
#'
#' @examples
#' #Exemple d'utilisation :
#'
#' Resultat <- address_to_gps("Champ de Mars, 5 Av. Anatole France, 75007 Paris")
#' View(Resultat)
address_to_gps <- function(adresse) {
  df_adresse <- data.frame("nom" = character(), addr = character(), stringsAsFactors = FALSE)
  df_adresse <- rbind(df_adresse, data.frame(addr = adresse), stringsAsFactors = FALSE)
  resultat_geocodage <- df_adresse |>
    geocode(addr, method = 'arcgis')
  df_adresse <- resultat_geocodage
}

##Question 11##
#' Renvoie les coordonnees x et y associees à une adresse donnee.
#'
#' @param adresse Vecteur de type character.
#'
#' @return Les coordonnees x et y associees a une adresse donnee.
#' @export
#' @import tibble
#' @import lubridate
#' @import testthat
#' @import devtools
#' @import httr2
#' @import jsonlite
#' @import tidygeocoder
#'
#' @examples
#' #Exemple d'utilisation :
#'
#'  Resultat <- get_gps_coordinate("Champ de Mars, 5 Av. Anatole France, 75007 Paris")
#'  View(Resultat)
get_gps_coordinate <- function(adresse) {
  resultat_geocodage <- address_to_gps(adresse)

  coordinates <- c(resultat_geocodage$lat, resultat_geocodage$long)

}

##Question 12##
#' Verification du type "numeric" du parametre.
#'
#' @param coordonnée Vecteur numerique de taille 2.
#'
#' @return "L'argument coordonnee doit etre un vecteur numerique de taille 2 (latitude, longitude)." Si le vecteur ne respecte pas le bon typage.
#' @export
#'
#' @import tibble
#' @import lubridate
#' @import testthat
#' @import devtools
#' @import httr2
#' @import jsonlite
#' @import tidygeocoder
#'
forecast.numeric <- function(coordonnée) {
  if (!is.numeric(coordonnée) || length(coordonnée) != 2) {
    stop("L'argument coordonnée doit être un vecteur numérique de taille 2 (latitude, longitude).")
  }}
#' Application de la verification aux fonctions precedente.
#'
#' @param coordonnees Vecteur numerique de taille 2.
#'
#' @return
#' Un tibble contenant les données des previsions meteo, tel que :
#'                  - L'heure au tz UTC
#'                  - Les donnees de température en Celsuis
#'                  - Les donnees de température ressentie en en Celsuis
#'                  - La probabilité de pluie en en Pourcentage
#'                  - La precipitation en mm
#'Ou un message d'erreur
#' @export
#'
#' @import tibble
#' @import lubridate
#' @import testthat
#' @import devtools
#' @import httr2
#' @import jsonlite
#' @import tidygeocoder
#'
#' @examples
#'
#' #Exemple d'utilisation :
#'
#' Resultat <- get_forecast.numeric(c(37.7749, -122.4194))
#' View(Resultat)
#' #Aucun message d'erreur renvoyé.
#'
#' get_forecast.numeric("LIeu-dit le Ménec, 56340 Carnac")
#' #Attention message d'erreur renvoyé.
get_forecast.numeric <- function(coordonnees) {
  forecast.numeric(coordonnees)

  resultat_previsions <- perform_request(latitude = coordonnees[1], longitude = coordonnees[2])
  resultat_traitement <- unnest_response(resultat_previsions)

}

##Question 13##
#' Verification du type "character" du parametre.
#'
#' @param address Vecteur character de taille 1.
#'
#' @return "L'argument address doit etre de type character et de taille 1." Si le vecteur ne respecte pas le bon typage.
#' @export
#'
#' @import tibble
#' @import lubridate
#' @import testthat
#' @import devtools
#' @import httr2
#' @import jsonlite
#' @import tidygeocoder
#'
forecast.character <- function(address) {
  if (!is.character(address) || length(address) != 1) {
    stop("L'argument address doit être de type character et de taille 1.")
  }
}
#' Application de la verification aux fonctions precedente.
#'
#' @param address Vecteur character de taille 1.
#'
#' @return Un tibble contenant les données des previsions meteo, tel que :
#'                  - L'heure au tz UTC
#'                  - Les donnees de température en Celsuis
#'                  - Les donnees de température ressentie en en Celsuis
#'                  - La probabilité de pluie en en Pourcentage
#'                  - La precipitation en mm
#'Ou un message d'erreur
#' @export
#' @import tibble
#' @import lubridate
#' @import testthat
#' @import devtools
#' @import httr2
#' @import jsonlite
#' @import tidygeocoder
#'
#' @examples
#'
#' #Exemple d'utilisation :
#' Resultat <- get_forecast.character("Pointe de la Torche, 29120 Plomeur France")
#' View(Resultat)
#' #Aucun message d'erreur renvoyé.
#'
#' get_forecast.character(c(37.7749, -122.4194))
#' #Attention message d'erreur renvoyé.
get_forecast.character <- function(address) {
  forecast.character(address)

  coordinates <- get_gps_coordinate(address)

  resultat_previsions <- perform_request(latitude = coordinates[1], longitude = coordinates[2])
  resultat_traitement <- unnest_response(resultat_previsions)

}

##Question 14##

#' Obtenir les previsions meteo à partir d'une adresse.
#'
#' @param x Adresse (character) ou coordonnées (numeric).
#'
#' @return Un tibble contenant les données des previsions meteo, tel que :
#'                  - L'heure au tz UTC
#'                  - Les donnees de température en Celsuis
#'                  - Les donnees de température ressentie en en Celsuis
#'                  - La probabilité de pluie en en Pourcentage
#'                  - La precipitation en mm
#' @export
#' @import tibble
#' @import lubridate
#' @import testthat
#' @import devtools
#' @import httr2
#' @import jsonlite
#' @import tidygeocoder
#'
#' @examples
#' # Exemple d'utilisation :
#'       - resultats_finals <- get_forecast("15 rue louis Gaudin, Nantes,FRANCE")
#'         print(resultats_finals)
#'
#'       - resultats_finals2 <- get_forecast(c(37.7749, -122.4194))
#'         print(resultats_finals2)
get_forecast <- function(x) {
  if (is.numeric(x)) {
    get_forecast.numeric(x)
  } else if (is.character(x)) {
    get_forecast.character(x)
  } else {
    stop("L'argument doit être de type numeric (coordonnées) ou character (adresse).")
  }
}



