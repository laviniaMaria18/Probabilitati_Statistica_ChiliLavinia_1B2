set.seed(42)  

computere = 40
infectare = 0.2
k = 4  
simulari = 10000  


simuleaza_proces <- function() {
  computere_infectate = rep(FALSE, computere)
  computere_infectate[sample(computere, 1)] = TRUE  
  
  zile = 0
  Tinfectate = FALSE
  min_15 = FALSE
  
  while(zile < 1000) { 
    zile = zile + 1
    
    infectari_potentiale = which(!computere_infectate)
    for (i in infectari_potentiale) {
      if (runif(1) < 1 - (1 - infectare)^sum(computere_infectate)) {
        computere_infectate[i] = TRUE
      }
    }
    
    
    if (all(computere_infectate)) {
      Tinfectate = TRUE
      min_15 = TRUE
      break
    }
    
    
    if (sum(computere_infectate) >= 15) {
      min_15 = TRUE
    }
    
    
    infectate_curente = which(computere_infectate)
    if (length(infectate_curente) <= k) {
      computere_infectate[infectate_curente] = FALSE
    } else {
      computere_infectate[sample(infectate_curente, k)] = FALSE
    }
  }
  
  return(c(Tinfectate, min_15))
}


rezultate = replicate(simulari, simuleaza_proces())
prob_toate_infectate = mean(rezultate[1, ])
prob_cel_putin_15 = mean(rezultate[2, ])

cat("pb ca intr o zi sa se infecteze toate calculatoarele:", prob_toate_infectate, "\n")
cat("pb ca intr o zi cel putin 15 calculatoare sa fie infectate:", prob_cel_putin_15, "\n")
