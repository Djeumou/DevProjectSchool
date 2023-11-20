#' Calculate the fast fourier transform
#'
#' This function calculate the fast fourier transform of a given vector.
#'
#' @param v the vector given to have the fast fourier transform
#'
#' @return The fast fourier transform of vector
#'
#' @examples
#' fft(sin(5*seq(0, 2*pi, length.out=10)))
#' @export
fftMad <- function(v){
  y <- c()
  k = 1
  n <- length(v)
  m <- 0:(n-1)
  for (variable in v){
    j = 0
    y[k] <- 0
    for (j in m){
      y[k] = y[k] + v[j+1]*exp(-2*pi*(as.complex(1i))*(j-1)*(length(y)-1)/n)
      #print(length(y))
      j = j+1
    }
    k = k+1
  }
  return(y)
}
