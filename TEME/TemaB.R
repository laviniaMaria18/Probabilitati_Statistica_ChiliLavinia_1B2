#B1 #####################################
estimare_volum = function(R, r, esantion) 
{
  #nr de puncte din int 
  nr_puncte_valide = 0
  for (i in 1:esantion) 
  {
    #limitele
    x1 = runif(1, -R - r, R + r)
    x2 = runif(1, -R - r, R + r)
    x3 = runif(1, -r, r)
    #verificare daca e in interior
    if ((x1^2 + x2^2 + x3^2 <= R^2) && (sqrt(x1^2 + x2^2) >= R - r)) {
      nr_puncte_valide = nr_puncte_valide + 1
    }
  }
  procent_puncte_valide = nr_puncte_valide / esantion #desnitate de puncte valide /nr total
  volum_estimat = procent_puncte_valide * (2 * pi * R * r^2) #cu cat nr de puncte e mai mare cu atat de precisa va fi estimarea
  return(volum_estimat)
}

R = 10
r = 3
esantioane = c(10000, 20000, 50000)

for (esantion in esantioane) 
{
  volum_estimat = estimare_volum(R, r, esantion)
  volum_exact = 2 * pi * R * r^2
  eroare_relativa = abs((volum_estimat - volum_exact) / volum_exact) * 100
  cat("Pentru esantionul de dimensiune", esantion, ", eroarea relativa este:", eroare_relativa, "\n")
}
#77.06  78.22 77.748


#B2 #########################################################

a = 0  # Limita stângă pentru x
b = 2  # Limita dreaptă pentru x
c = 0  # Limita inferioară pentru y
d = 6  # Limita superioară pentru y

estimare_arie = function(esantion) 
{
  nr_puncte_in_triunghi = 0
  for (i in 1:esantion) {
    #se genereaza coord aleatorii
    x = runif(1, a, b)
    y = runif(1, c, d)
    if (y >= 0 && y <= 2 * x && y <= 6 - 3 * x) {
      nr_puncte_in_triunghi = nr_puncte_in_triunghi + 1
    }
  }
  proportie_in_triunghi = nr_puncte_in_triunghi / esantion
  #multiplicam procentul de puncte din interior cu zona rectangulara care l incadreaza
  arie_estimata = proportie_in_triunghi * (b - a) * (d - c)
  return(arie_estimata) 
}

esantion = 20000
arie_estimata = estimare_arie(esantion)
print(paste("Aria estimată a triunghiului T este:", arie_estimata))#2.3652


#B3 
  #a ##############################################################
MC_integral = function(N) {
  sum = 0
  for (i in 1:N) {
    x = runif(1, -1, 1) #intervalul
    y = runif(1, 0, 1)
    if (y <= (2*x - 1) / (x^2 - x - 6)) {
      sum = sum + 1
    }
  }
  integral_estimate = sum / N * 2  #intervalul e -1 1
  return(integral_estimate)
}

MC_integral_estimate = function(k, N) {
  #vector pt a stoca estimarile
  estimates = rep(0, k)
  for (i in 1:k) {
    estimates[i] = MC_integral(N)
  }
  mean_estimate = mean(estimates)#media
  sd_estimate = sd(estimates)#deviatia
  return(c(mean_estimate, sd_estimate))
}

num_simulations = 100# nr de simulari
num_points = 10000 #nr de puncte 

result = MC_integral_estimate(num_simulations, num_points)
print(result[1])  #0.447886
print(result[2])  #0.008113451


  #b ########################################################
MC_integral_b = function(N) {
  sum = 0
  for (i in 1:N) {
    x = runif(1, 3, 11)
    y = runif(1, 0, 5)
    if (y <= (x + 4) / sqrt(3*x - 3)) {
      sum = sum + (11 - 3) * (5 - 0) * (x + 4) / sqrt(3*x - 3)
    }
  }
  integral_estimate = sum / N
  return(integral_estimate)
}

MC_integral_estimate = function(k, N) {
  estimates = rep(0, k)
  for (i in 1:k) {
    estimates[i] = MC_integral_b(N)
  }
  mean_estimate = mean(estimates)
  sd_estimate = sd(estimates)
  return(c(mean_estimate, sd_estimate))
}
num_simulations = 100
num_points = 10000

result = MC_integral_estimate(num_simulations, num_points)
print(result[1]) # 56.1257
print(result[2])  #0.4245568


 #c ##############################################

MC_integral_c = function(N) {
  sum = 0
  max_x = 1000  
  for (i in 1:N) {
    x = runif(1, 0, max_x)
    y = runif(1, 0, 0.5)  
    if (y <= x * exp(-x^2)) {
      sum = sum + max_x * 0.5  
    }
  }
  integral_estimate = sum / N
  return(integral_estimate)
}

MC_integral_estimate_c = function(k, N) {
  estimates = rep(0, k)
  for (i in 1:k) {
    estimates[i] = MC_integral_c(N)
  }
  mean_estimate = mean(estimates)
  sd_estimate = sd(estimates)
  return(c(mean_estimate, sd_estimate))
}


num_simulations = 10000
simulation_size = 1000
result_c = MC_integral_estimate_c(num_simulations, simulation_size)

print(result[1])  #56.1257
print(result[2]) #0.4245568


#B4
 #a #######################################################
# Definirea parametrilor
n = 1000
p = 0.25
q = 0.01
target_users = 15000

MC_years_to_reach_target = function() {
  users = 10000
  years = 0
  while (users < target_users) {
    new_users = rbinom(1, n, p) #se genereaza un nr aleatroiu conform distributiei binomiale
    users = users + new_users
    users = users - rbinom(1, users, q) #se simuleaza retragerea utilizatorilor cu o prb q
    years = years + 1
  }
  return(years)
}

num_simulations = 10000
years = replicate(num_simulations, MC_years_to_reach_target())
mean_years_to_reach_target = mean(years)
cat("Numărul mediu de ani pentru a atinge 15000 de utilizatori:", mean_years_to_reach_target, "\n")
#41.6937

   # (b) ################################################## 
MC_prob_40_years = function() {
  users = 10000
  #se parcurge fiecare luna in decurs a 40 de ani
  for (i in 1:(40*12 + 10)) {
    new_users = rbinom(1, n, p) #nr de utilizatori noi
    users = users + new_users
    users = users - rbinom(1, users, q)
  }
  return(users >= target_users)
}

prob_40_years = mean(replicate(num_simulations, MC_prob_40_years()))
cat("Probabilitatea de a avea cel puțin 15000 de utilizatori după 40 de ani și 10 luni:", prob_40_years, "\n")
#1


  # (c)  #######################################################
precision = 0.01
confidence = 0.99

MC_prob_with_precision = function() {
  prob = MC_prob_40_years()
  #cat de mica posibil
  while (abs(prob - target_prob) > precision) {
    prob = MC_prob_40_years()
  }
  return(prob)
}

target_prob = prob_40_years
lower_bound = target_prob - precision
upper_bound = target_prob + precision

num_iterations = 0
confidence_interval = 0

while (confidence_interval < lower_bound || confidence_interval > upper_bound) {
  target_prob = MC_prob_with_precision()
  confidence_interval = target_prob
  num_iterations = num_iterations + 1
}
cat("Numărul de simulări pentru a obține o probabilitate cu o eroare de ±0.01:", num_iterations, "\n")#1