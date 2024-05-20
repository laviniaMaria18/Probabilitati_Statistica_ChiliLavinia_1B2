variabilaDiscreta = function(valori, probabilitati) 
{
     if(length(valori)!=length(probabilitati)) 
    {
       stop("Trebuie sa avem acelasi numar de valori si de probabilitati.")
    }
      
        if(any(probabilitati<0)||sum(probabilitati)!=1)
           {
               stop("Nu putem avea probabilitati negative, iar suma tuturor probabilitatilor trebuie sa fie 1")
             }
       rand = runif(1)
       probabilCumulate = cumsum(probabilitati)
       index = match(TRUE, rand <= probabilCumulate)
       return(valori[index])
 }
valori = c(5, 8, 9)
probabilitati = c(0.1, 0.1, 0.8)
valoareSimulata = variabilaDiscreta(valori, probabilitati)

print(valoareSimulata)


valori = c(4, 2, 3)
probabilitati = c(0.1, 0.4, 0.6)
valoareSimulata = variabilaDiscreta(valori, probabilitati)
print(valoareaSimulata)

valori=c('a','b','c')
probablititati=c(-0.2,0.5,0.7)
valoareSimulata=variabilaDiscreta(valori,probabilitati)
print(valoareaSimulata)
  