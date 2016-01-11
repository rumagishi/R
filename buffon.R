set.seed(3)

### n : the number of throws.
### reduction_radio : this must be less than 1.0.
### d : the interval between lines.
buffons_needle <- function(n, reduction_radio, d, axe, c) {
  # initialize
  l <- reduction_radio*d # the length of needles (l <= d).
  x <- runif(n, min = 0, max = d) # 0 <= x <= d
  theta <- runif(n, min = 0, max = pi) # 0 <= theta <= pi
  counter <- 0 # incremented by 1 when needle crosses line
  value <- c() # vector for pi
  
  # buffon throws needles.
  for (i in 1:n) {
    if (x[i]+l*sin(theta[i])>=d) {
      counter <- counter + 1
    }
    value <- append(value, (2*i*l)/(d*counter), after=length(value))
  }
  
  # plot a graph.
  Number_of_throws <- 1:n
  plot(Number_of_throws, value, main='buffon\'s needle', type='l', axes=axe, col=c, ylim=c(1.0,8.0))
  abline(h=pi, col='red')
}

buffons_needle(1000, 0.2, 4, FALSE, 'black')
par(new=T)
buffons_needle(1000, 0.4, 9, FALSE, 'cyan')
par(new=T)
buffons_needle(1000, 0.9, 4, TRUE, 'blue')

