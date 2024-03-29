% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_weather_forecast.R
\name{get_forecast.numeric}
\alias{get_forecast.numeric}
\title{Application de la verification aux fonctions precedente.}
\usage{
get_forecast.numeric(coordonnees)
}
\arguments{
\item{coordonnees}{Vecteur numerique de taille 2.}
}
\value{
Un tibble contenant les données des previsions meteo, tel que :
- L'heure au tz UTC
- Les donnees de température en Celsuis
- Les donnees de température ressentie en en Celsuis
- La probabilité de pluie en en Pourcentage
- La precipitation en mm
Ou un message d'erreur
}
\description{
Application de la verification aux fonctions precedente.
}
\examples{

#Exemple d'utilisation :

Resultat <- get_forecast.numeric(c(37.7749, -122.4194))
View(Resultat)
#Aucun message d'erreur renvoyé.

get_forecast.numeric("LIeu-dit le Ménec, 56340 Carnac")
#Attention message d'erreur renvoyé.
}
