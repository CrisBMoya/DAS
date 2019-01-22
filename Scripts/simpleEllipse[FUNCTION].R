#Create Simple Ellipse (Hotellings)
simpleEllipse=function (x, y, alfa = 0.95, len = 200){
  N <- length(x)
  A <- 2
  mypi <- seq(0, 2 * pi, length = len)
  r1 <- sqrt(var(x) * qf(alfa, 2, N - 2) * (2 * (N^2 - 1)/(N * 
                                                             (N - 2))))
  r2 <- sqrt(var(y) * qf(alfa, 2, N - 2) * (2 * (N^2 - 1)/(N * 
                                                             (N - 2))))
  cbind(r1 * cos(mypi) + mean(x), r2 * sin(mypi) + mean(y))
}