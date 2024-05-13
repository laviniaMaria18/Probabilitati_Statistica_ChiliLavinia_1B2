# b) 
f = function(x) exp(x)

i = 1
j = 4
exact = integrate(f, i, j)$value

#d
integrala_monte_carlo = function() {
  f = function(u) 1 / (4 * u^2 - 1)
  
  a = 0
  b = 1
  val = log(3/4)
  
  set.seed(123)
  n = 100000
  u = runif(n, a, b)
  
  integrala = (b - a) * mean(f(u))
  
  eroare_absoluta = abs(integrala - val)
  eroare_relativa = eroare_absoluta / abs(val)
  
  rezultat <- list(integrala = integrala, val = val, eroare_absoluta = eroare_absoluta, eroare_relativa = eroare_relativa)
  return(rezultat)
}

rezultat <- integrala_monte_carlo()

cat("Valoare estimata:", format(rezultat$integrala, digits = 8), "\n")
cat("Valoare exacta:", format(rezultat$valoare_exacta, digits = 8), "\n")
cat("Eroare absoluta:", format(rezultat$eroare_absoluta, digits = 8), "\n")
cat("Eroare relativa:", format(rezultat$eroare_relativa * 100, digits = 2), "%\n")
