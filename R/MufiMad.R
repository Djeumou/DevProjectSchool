# Charger le package tuneR
library(tuneR)

MufiMad <- function(x){

  # Charger un fichier audio
  audio_file <- readWave(x)

  # Extraire les données audio
  audio_data <- audio_file@left

  # Calculer la FFT
  fft_data <- ifftMad(audio_data)

  # Filtrer les fréquences indésirables
  freq <- seq(0, (length(fft_data)) - 1) * audio_file@samp.rate/length(fft_data)
  fft_data[freq > 1000 / audio_file@samp.rate * length(fft_data)] <- 0

  # Calculer l'inverse de la FFT
  filtered_data <- Re(ifftMad(fft_data))

  # Créer un nouveau fichier audio
  filtered_audio <- Wave(filtered_data, samp.rate=audio_file@samp.rate, bit=audio_file@bit, pcm=audio_file@pcm)

  #Normaliser le song
  sound_result_normalize <- normalize(filtered_audio, unit = c("1", "8", "16", "24", "32", "64", "0"),
                                      center = TRUE, level = 1, rescale = TRUE, pcm = filtered_audio@pcm)

  # Écrire le fichier audio filtré
  writeWave(sound_result_normalize, "filtered_audio.wav")
}
