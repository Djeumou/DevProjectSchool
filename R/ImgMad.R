# Charger le package fftwtools
#install.packages("fftwtools")
#library(fftwtools)

ImgMad <- function(x){

  # Charger une image
  img <- readPNG(x)

  # Convertir l'image en niveaux de gris
  img_gray <- grayscale(img)

  # Calculer la FFT
  fft_data <- fftMad(img_gray)

  # Filtrer les fréquences indésirables
  freq <- seq(0, length(fft_data) - 1) * dim(img_gray)[1] / length(fft_data)
  fft_data[freq > 100] <- 0

  # Calculer l'inverse de la FFT
  filtered_data <- Re(fftMad(fft_data, inverse=TRUE))

  # Créer une nouvelle image
  filtered_img <- array(filtered_data, dim(img_gray))
  filtered_img <- grayscale(filtered_img)

  # Écrire l'image filtrée
  writePNG(filtered_img, "filtered_image.png")
}
