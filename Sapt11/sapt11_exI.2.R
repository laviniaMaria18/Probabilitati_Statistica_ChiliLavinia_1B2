parabola_area = function() {
  f = function(x) -2*x^2 + 5*x - 2
  
  a = 0
  b = 2
  Area = integrate(f, a, b)$value
  
  set.seed(123)
  n = 10000
  x = runif(n, a, b)
  y = runif(n, 0, max(f(x)))
  
  inside = y <= f(x)
  p_inside = mean(inside)
  
  area = (b - a) * max(f(x)) * p_inside
  e = abs(Area - area) / Area
  
  result = list(area = area, Area = Area, e = e)
  return(result)
}

result = parabola_area()

cat("Estimated area:", format(result$area, digits = 8), "\n")
cat("Exact area:", format(result$Area, digits = 8), "\n")
cat("Relative error:", format(result$e, digits = 2), "%\n")
