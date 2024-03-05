#LIBRARY#
library(ggplot2)

#1 FONCTION DE VISUALISATION - 1#
#' Graphique représentant la précipitation en mm et la probabilité de pluie en %
#'
#' @param resultats Un tibble retourné par la fonction get_forecast
#'
#' @return Un graphique de visualisation de la précipitation en mm et de la probabilité de pluie à un point/adresse données
#' @export
#' @import ggplot2
#'
Graphique_pluie <- function(resultats) {
  resultats$`heure au tz UTC` <- as.POSIXct(resultats$`heure au tz UTC`)
  #ECHELLES DES AXES Y#
  max_precipitations <- max(resultats$`precipitation en mm`)
  proportion_echelle <- max_precipitations / 100

  #CREATION DU GRAPHIQUE#
  graphique_combiné <- ggplot(resultats, aes(x = `heure au tz UTC`)) +
    geom_bar(aes(y = `probabilité de pluie`), stat = "identity", fill = "#003366", alpha = 0.7) +
    geom_line(aes(y = `precipitation en mm` / proportion_echelle), color = "#33CCCC", size = 1) +
    labs(title = "Probabilité de pluie et Précipitations au fil du temps", x = "Heure (UTC)", y = "Probabilité de pluie (%) / Précipitations (mm)") +
    scale_y_continuous(name = "Probabilité de pluie (%)", limits = c(0, 100)) +
    scale_y_continuous(
      name = "Précipitations (mm)",
      sec.axis = sec_axis(
        ~.*proportion_echelle,
        name = "Précipitations (mm)",
        breaks = seq(0, max_precipitations, by = 0.2),
        labels = seq(0, max_precipitations, by = 0.2)
      )
    ) +
    theme_minimal() +
    guides(
      fill = guide_legend(title = "Probabilité de pluie"),
      color = guide_legend(title = "Précipitations (mm)")
    )
  return(graphique_combiné)
}

#1 FONCTION DE VISUALISATION - 2#
#' Graphique Représentant la température et la température en degrés ccelsius
#'
#' @param resultats Un tibble retourné par la fonction get_forecast
#'
#' @return Un graphique de visualisation de la température et la température en degrés ccelsius à un point/adresse données
#' @export
#' @import ggplot2
#'
Graphique_temperature <- function(resultats) {
  resultats$`heure au tz UTC` <- as.POSIXct(resultats$`heure au tz UTC`)

  #CREATION DU GRAPHIQUE#
  graphique_temp <- ggplot(resultats, aes(x = `heure au tz UTC`)) +
    geom_line(aes(y = `données de température`, color = "Température"), size = 1) +
    geom_line(aes(y = `données de température ressentie`, color = "Température ressentie"), size = 1) +
    labs(title = "Température au fil du temps", x = "Heure (UTC)", y = "Température (°C)") +
    scale_color_manual(values = c("Température" = "#FF3333", "Température ressentie" = "#FF6600")) +
    theme_minimal()

  return(graphique_temp)
}

