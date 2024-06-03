#A1
  #a ############################################
calc_probabilities = function(lambda, p, n, m, k) 
{
  
  poisson_probs = dpois(k:m, lambda)
 
  geom_probs = dgeom(k:m, p)
  
  binom_probs = dbinom(k:m, n, p)
  
  return(list(poisson = poisson_probs, geom = geom_probs, binom = binom_probs))
}
#exemplu
lambda = 3.0
p = 0.5
n = 10
m = 7
k = 2
probabilities = calc_probabilities(lambda, p, n, m, k)
print("Poisson probabilities:")
print(probabilities$poisson) # 0.22404181 0.22404181 0.16803136 0.10081881 0.05040941 0.02160403
print("Geometric probabilities:")
print(probabilities$geom)  #0.12500000 0.06250000 0.03125000 0.01562500 0.00781250 0.00390625
print("Binomial probabilities:")
print(probabilities$binom) # 0.04394531 0.11718750 0.20507812 0.24609375 0.20507812 0.11718750


 #b###########################################################
#calcularea functiilor de masa probabilitate
Geometric = function(m, p, k)
{
  x = k:m
  y = dgeom(x, p)
  
}

Poisson =function(m, lambda, k) 
{
  x = k:m
  y = dpois(x, lambda)
}

B = function(n, m, p, k)
{
  x = k:m
  y = dbinom(x, size=n, prob=p)
}

# Parametri specificati
lambda = 3
p = 0.3
n= 15
m = 7
k = 2
 
functii = cbind(Geometric(m, p, k), Poisson(m, lambda, k), B(n, m, p, k))
colnames(functii) = c("Geometric", "Poisson", "Binomial")

barplot(t(functii), beside = TRUE, col = c("red", "blue", "green"), 
        names.arg = Geometric(m, p, k), legend = TRUE)
title(main = "Probability Mass Functions", xlab = "k", ylab = "P(X=k)")



  #c ########################################################

find_k0 = function(lambda) 
{
  k0 = 0 
  prob = ppois(k0, lambda)
  while (prob <= 1 - 1e-6) {
    k0 = k0 + 1
    prob = ppois(k0, lambda)
  }
  return(k0)
}


lambda = 3.0
k0 = find_k0(lambda)
print(paste("Cea mai mică valoare a lui k0 pentru care P(Y ≤ k0) > 1 - 10^-6 este", k0))
#14



#A2
    #a################################################################

calculate_frequencies = function(file_path) {
  
  data = read.table(file_path, header=TRUE)
  
  sample_P = data$P
  sample_S = data$S
  
  freq_P = as.vector(table(sample_P))
  freq_S = as.vector(table(sample_S))
  
  mean_P = mean(sample_P)
  mean_S = mean(sample_S)
  
  return(list(freq_P = freq_P, freq_S = freq_S, mean_P = mean_P, mean_S = mean_S))
}


file_path = "Desktop/note_PS.txt"
results = calculate_frequencies(file_path)
print("Frecventele absolute pentru esantionul P:")
print(results$freq_P) #1  1  2 10 73 91 69 45 28 34
print("Frecventele absolute pentru esantionul S:")
print(results$freq_S) #8 27 36 97 73 48 28 26 11
print(paste("Media esantionului P:", results$mean_P)) #6.76553672316384
print(paste("Media esantionului S:", results$mean_S)) #5.84180790960452




   #b #########################################################

clean_outliers = function(file_path, sample_name) 
{
  
  data = read.table(file_path, header=TRUE)
  

  sample = data[[sample_name]]
  
  # Calculează media și deviația eșantionului
  m = mean(sample)
  s = sd(sample) #abaterea
  
  #maj valorilor se afla in interval
  cleaned_sample = sample[(sample >= m - 2 * s) & (sample <= m + 2 * s)]
  
 
  intervals = seq(1, 10, 1)
  hist(cleaned_sample, breaks = intervals, main = paste("Distribuția frecvențelor pentru eșantionul", sample_name), 
       xlab = "Valoare", ylab = "Frecvență", col = "skyblue", border = "black")
}

clean_outliers("Desktop/note_PS.txt", "P")
