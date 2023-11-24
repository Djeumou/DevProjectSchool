library(tuneR)
if (!require("tuneR")) install.packages("tuneR")
#' Analyse the spectre audio
#'
#' This package analyse the specter of a son by given path
#'
#' @param x which is the path of song to analyse
#'
#' @return the specter of song given by path
#'
#' @examples
#' SpecMad("science-and-technology-152988.wav")
#' @export
SpecMad <- function(x){
  # Charger le fichier audio
  audio <- readWave(x)

  # Extraire les données audio
  audio_data <- audio@left
  plot(audio_data, type = "l", xlab = "Temps", ylab = "Amplitude")

  # Calculer la densité spectrale de puissance
  psd <- abs(fftMad(audio_data))^2 / length(audio_data)

  # Tracer le résultat
  plot(psd, type = "l", xlab = "Fréquence", ylab = "Densité spectrale de puissance")
}
