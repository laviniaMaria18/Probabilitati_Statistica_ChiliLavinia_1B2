Gamma = function(alpha, lambda, n = 50, N = c(5000, 10000, 20000), z = c(-1.5, 0, 1.5)) 
{
  for (i in N) 
  {
    for (j in z) 
    {
      samples = matrix(rgamma(i * n, shape = alpha, rate = lambda), ncol = n)
      
      sample2 = apply(samples, 1, mean)
      
      x = n * alpha / lambda
      sigma = sqrt(n * alpha) / lambda
      
      z_quantile = qnorm(pnorm(j))
      
      hist(sample2, breaks = 30, freq = FALSE, main = paste0("n = ", n, ", N = ", i, ", z = ", j))
      curve(dnorm(x, mean = x, sd = sigma), add = TRUE, col = "purple")
      abline(v = x + z_quantile * sigma, col = "darkblue")
    }
  }
}

Gamma(1, 2)
