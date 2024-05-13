set.seed(42)  # asig reproducibilitatea

nr_simulari = 10000
probabilitate = 3/4
lambda1 = 4
lambda2 = 12

timp_servire = numeric(nr_simulari)

for (i in 1:nr_simulari) {
  if (runif(1) < probabilitate) {
    timp_servire[i] = rexp(1, rate = lambda1)
  } else {
    timp_servire[i] = rexp(1, rate = lambda2)
  }
}

media_timp_servire <- mean(timp_servire)
print(media_timp_servire)



