#D1 ###############################################




probabilitati <- read.csv("Desktop/probabilitati.txt")


n <- length(probabilitati$note)

# Calculul mediei și deviației standard
mean_prob <- mean(probabilitati$note)
sd_prob <- sqrt(92.16)

#punctele critice 
t_95 <- qt(0.975, df = n - 1) #aprozimare mai precisa
t_99 <- qt(0.995, df = n - 1)

# Calculul intervalului de încredere pentru media la nivelul de semnificație 95%
interval_95 <- c(mean_prob - t_95 * (sd_prob / sqrt(n)), mean_prob + t_95 * (sd_prob / sqrt(n)))

# Calculul intervalului de încredere pentru media la nivelul de semnificație 99%
interval_99 <- c(mean_prob - t_99 * (sd_prob / sqrt(n)), mean_prob + t_99 * (sd_prob / sqrt(n)))

# Afișarea rezultatelor
print(paste("Intervalul de încredere de 95% pentru media la Probabilități:", interval_95))
print(paste("Intervalul de încredere de 99% pentru media la Probabilități:", interval_99))




#D2 ################################

# Citirea datelor din fișierul statistica.csv
probabilitati <- read.csv("statistica.csv")
# Calculul deviației standard
sd_stat <- sqrt(92.16)

# Determinarea cuantilelor pentru nivelurile de semnificație 0.05 și 0.01
Z_95 <- qnorm(0.975)
Z_99 <- qnorm(0.995)

# Calculul intervalului de încredere pentru media la nivelul de semnificație 95%
interval_95_stat <- c(mean_prob - Z_95 * (sd_stat / sqrt(n)), mean_prob + Z_95 * (sd_stat / sqrt(n)))

# Calculul intervalului de încredere pentru media la nivelul de semnificație 99%
interval_99_stat <- c(mean_prob - Z_99 * (sd_stat / sqrt(n)), mean_prob + Z_99 * (sd_stat / sqrt(n)))

# Afișarea rezultatelor
print(paste("Intervalul de încredere de 95% pentru media la Statistică:", interval_95_stat))
print(paste("Intervalul de încredere de 99% pentru media la Statistică:", interval_99_stat))



#D3 ########################################
# Numărul total de studenți
n <- 100

# Numărul de studenți care nu pot rezolva temele după schimbare
x <- 14

# Procentul de studenți care nu pot rezolva temele
p <- x / n

# Nivelurile de semnificație
alpha1 <- 0.01
alpha2 <- 0.05

# testarea ipotezei si intervalul 
z_alpha1 <- qnorm(1 - alpha1 / 2)
z_alpha2 <- qnorm(1 - alpha2 / 2)

margin_error1 <- z_alpha1 * sqrt((p * (1 - p)) / n) #marginile de eroare
margin_error2 <- z_alpha2 * sqrt((p * (1 - p)) / n)

interval1 <- c(p - margin_error1, p + margin_error1)
interval2 <- c(p - margin_error2, p + margin_error2)

cat("Intervalul de încredere de 99% pentru procentul de studenți care nu pot rezolva temele:", interval1, "\n")
cat("Intervalul de încredere de 95% pentru procentul de studenți care nu pot rezolva temele:", interval2, "\n")

# Testăm ipoteza nulă
p0 <- 0.85  # Procentul inițial de studenți care nu pot rezolva temele
z_test <- (p - p0) / sqrt((p0 * (1 - p0)) / n)


#pt a verifica daca putem respinge sau nu ipoteza nula
p_value1 <- 2 * (1 - pnorm(abs(z_test)))  # Pentru bilaterala
p_value2 <- 1 - pnorm(abs(z_test))  # Pentru unilaterală

cat("Valoarea P pentru testul bilateral:", p_value1, "\n")
cat("Valoarea P pentru testul unilateral:", p_value2, "\n")

# Interpretăm rezultatele
if (p_value1 < alpha1) {
  cat("Ipoteza nulă este respinsă la nivelul de semnificație de 1%. Concluzia este că schimbarea a fost utilă.\n")
} else {
  cat("Ipoteza nulă nu poate fi respinsă la nivelul de semnificație de 1%. Nu există suficiente dovezi pentru a susține că schimbarea a fost utilă.\n")
}

if (p_value2 < alpha2) {
  cat("Ipoteza nulă este respinsă la nivelul de semnificație de 5%. Concluzia este că schimbarea a fost utilă.\n")
} else {
  cat("Ipoteza nulă nu poate fi respinsă la nivelul de semnificație de 5%. Nu există suficiente dovezi pentru a susține că schimbarea a fost utilă.\n")
}
