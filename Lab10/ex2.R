Student_LNM = function(n = c(1000, 10000, 100000, 1000000), r = c(2, 3, 4, 5), n_samples, sizes) 
{
  for (i in n) 
  {
    for (j in r) 
    {
      averages = matrix(nrow = i, ncol = length(sizes))
      
      for (k in 1:length(sizes)) 
      {
        samples = matrix(rt(i * sizes[k], df = r), ncol = sizes[k])
        averages[, k] = apply(samples, 1, mean)
      }
      
      plot(sizes, colMeans(averages), type = "l", xlab = "Sample size", ylab = "Sample average")
      
      abline(h = 0, col = "pink", lty = 2)
    }
  }
}

Student_LNM(n_samples = 100, sizes = c(100, 200, 500, 1000))
