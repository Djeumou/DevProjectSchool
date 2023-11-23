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
DftMad <- function(v){
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

#Fast Fourier Tranform
fftMad <- function(x) {
  N <- length(x)
  if (N == 1) {
    return(x)
  } else {
    even <- fft(x[seq(1, N, 2)])
    odd <- fft(x[seq(2, N, 2)])
    factor <- exp(-2i*pi*(0:(N/2-1))/N)
    return(c(even + factor * odd, even - factor * odd))
  }
}

# Inverse Cooley-Tukey FFT Algorithm
ifftMad <- function(x) {
  n <- length(x)
  if(n == 1) {
    return(x)
  } else {
    even <- ifftMad(x[seq(1, n, 2)])
    odd <- ifftMad(x[seq(2, n, 2)])
    factor <- exp(2i * pi * seq(0, n-1)/n)
    return(c(even + factor[1:n/2] * odd, even + factor[(n/2+1):n] * odd)/2)
  }
}
