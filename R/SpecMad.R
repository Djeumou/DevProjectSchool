SpecMad <- function(x){
  # Charger le fichier audio
  audio <- readWave(x)

  # Extraire les données audio
  audio_data <- audio@left

  # Calculer la densité spectrale de puissance
  psd <- abs(fftMad(audio_data))^2 / length(audio_data)

  # Tracer le résultat
  plot(psd, type = "l", xlab = "Fréquence", ylab = "Densité spectrale de puissance")
}
