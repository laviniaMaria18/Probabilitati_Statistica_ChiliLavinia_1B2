
#c)
N = function(u, sigma) {
  
  x = seq(-5, 5, length.out = 1000)
  densitate = dnorm(x, mean = u, sd = sigma)
  
  plot(x, densitate, type = "l", xlab = "x", ylab = "Densitate",
       main = paste0("Densitate Normala (u = ", u, ", sigma^2 = ", sigma^2, ")"))
}
N(0, 1)
