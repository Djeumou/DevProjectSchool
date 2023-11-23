#' Treat a song
#'
#' This function treat a song by using Fast Fourier Transform function of this package.
#'
#' @param X mean to the path of a song take on the wave extension.
#'
#' @return Return the song treat by a play test
#'
#' @examples
#' fft(sin(5*seq(0, 2*pi, length.out=10)))
#' @export
Mufi <- function(x){
  fichier_audio <- readWave(x)

  echantillons_audio1 <- as.numeric(fichier_audio@left)
  echantillons_audio2 <- as.numeric(fichier_audio@right)

  bruit <- rnorm(length((echantillons_audio1)), 0, 0.5)      # Bruit gaussien

  observed_signal <- echantillons_audio1 + bruit
  fft_result <- fftMad(observed_signal)

  freq <- seq(0, 1 - 1/length((echantillons_audio1)), length.out = length((echantillons_audio1)))

  df <- data.frame(freq = freq, amplitude = Mod(fft_result))
  df1 <- data.frame(freq = freq, amplitude = observed_signal)

  #p1 <- ggplot(df1, aes(x = freq, y = amplitude)) +
   # geom_line() +
   # labs(x = "Fréquence", y = "Amplitude") +
    #ggtitle("Spectre fréquentiel du signal reçu")

  #p <- ggplot(df, aes(x = freq, y = amplitude)) +
   # geom_line() +
    #labs(x = "Fréquence", y = "Amplitude") +
    #ggtitle("Spectre fréquentiel du signal observé")


   #print(p1)
   #print(p)

  result_signal <- Mod(fft_result)
  sound_result <- Wave(result_signal, samp.rate = 44100, bit = 16)

  sound_result_normalize <- normalize(sound_result, unit = c("1", "8", "16", "24", "32", "64", "0"),
                                      center = TRUE, level = 1, rescale = TRUE, pcm = sound_result@pcm)

  play(sound_result_normalize)
}
