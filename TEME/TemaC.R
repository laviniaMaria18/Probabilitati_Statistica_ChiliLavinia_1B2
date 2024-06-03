#C1
  #a #################################################
  generate_permutation = function(n) {
    # Generarea n valori aleatoare uniforme standard
    random_values = runif(n)
    # Sortarea lor în ordine crescătoare și returnarea permutării rezultate
    return(order(random_values))
  }
  
  n = 10
  permutation = generate_permutation(n)
  print("Permutarea aleatoare generată:")
  print(permutation) # 5  9  7  6  3  8 10  4  2  1
  
  
  
  #b #########################################
  
  compare_bit_strings = function(Wi, Wj) {
    min_length = min(length(Wi), length(Wj))
    
    for (l in 1:min_length) {
      if (Wi[l] < Wj[l]) {
        return(TRUE)  #  lexicografic strict mai mic 
        
      } else if (Wi[l] > Wj[l]) {
        return(FALSE) #  strict mai mare 
      }
    }
    
    if (length(Wi) < length(Wj)) {
      return(TRUE)  # strict mai mic 
    } else if (length(Wi) > length(Wj)) {
      return(FALSE) #  mai mare 
    }
    
    # Dacă lungimile lor sunt egale și cuvintele sunt încă egale, se generează perechi de biți pentru a le compara
    return(sample(c(TRUE, FALSE), 1))
  }
  
  
  
  #c #########################################
  randomized_quick_sort = function(bit_strings) {
    n = length(bit_strings)
    
    if (n <= 1) {
      return(bit_strings)
    }
    
    #  pivot aleator
    pivot_index = sample(1:n, 1)
    pivot = bit_strings[pivot_index]
    
    # Separarea șirurilor de biți 
    lower = c() 
    upper = c()
    
    for (i in 1:n) {
      if (i != pivot_index) {
        if (compare_bit_strings(bit_strings[i], pivot)) {
          lower = c(lower, bit_strings[i])
        } else {
          upper = c(upper, bit_strings[i])
        }
      }
    }
    
    return(c(randomized_quick_sort(lower), pivot, randomized_quick_sort(upper)))
  }
 
  bit_strings <- matrix(sample(c(0, 1), 10 * 5, replace = TRUE), 10, 5)
  
 
  sorted_bit_strings <- randomized_quick_sort(bit_strings)
  

  print("Lista de șiruri de biți sortată:")
  print(sorted_bit_strings)
  
  
  #d #######################################################
 
  generate_random_permutation_of_words <- function(n, k) {
    # Generăm n cuvinte aleatoare de lungime k
    words <- matrix(sample(0:1, n*k, replace = TRUE), nrow = n)
    
    # Sortăm cuvintele folosind algoritmul QuickSort randomizat
    sorted_indices <- randomized_quick_sort(1:n)
    
    # Returnăm permutarea
    return(sorted_indices)
  }
  # Exemplu de utilizare pentru generarea unei permutări aleatoare de cuvinte
  n <- 5  # Numărul de cuvinte
  k <- 4  # Lungimea fiecărui cuvânt
  random_permutation_of_words <- generate_random_permutation_of_words(n, k)
  
  # Convertim lista de indici în cuvinte
  random_words <- sapply(random_permutation_of_words, function(index) {
    paste(sample(c(0, 1), k, replace = TRUE), collapse = "")
  })
  
  # Afisam cuvintele
  cat("Permutarea aleatoare generată de cuvinte:\n")
  for (i in 1:n) {
    cat("Cuvântul", i, ":", random_words[i], "\n")
  }
  
  
  
  
  
  
  
  
  #C2
  #a #######################################
  random_max_cut = function(n, m) {
    # Construim A alegând independent și uniform n noduri din V
    A = sample(1:(2*n + 1), n, replace = FALSE)
    # Construim B = V \ A
    B = setdiff(1:(2*n + 1), A)
    # Determinăm cardinalul tăieturii corespunzătoare
    cut_size = sum(A %in% A & !(A %in% B))  # Se numără muchiile care au un vârf în A și celălalt vârf în B
    return(cut_size)
  }
  
  
 
  n= 5  
  m = 8  
  cut_size = random_max_cut(n, m)
  print(paste("Cardinalul tăieturii de cardinal maxim este:", cut_size))
  
  
  