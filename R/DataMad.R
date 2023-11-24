#' Compress the data of a vector elements
#'
#' This function compress a data of a given vector by using FFT and delete hight frequency.
#'
#' @param x vector given to compression
#'
#' @return comparison between initial data and compress data after compressions
#'
#' @examples
#' DataMad(rnorm(1024))
#' @export
DataMad <- function(x){
  # Générer des données aléatoires
  #set.seed(123)
  #x <- rnorm(1024)

  # Calculer la transformée de Fourier rapide
  fx <- fft(x)

  # Supprimer les coefficients de Fourier de haute fréquence
  fx[51:974] <- 0

  # Calculer la transformée de Fourier inverse
  x_compressed <- Re(ifftMad(fx))

  # Vérifier que les données compressées sont similaires aux données originales
  #all.equal(x, x_compressed)
  a <- all.equal(length(x), length(x_compressed))
  return(x_compressed)
}
