# Solution

primeFactor = function(n) {
  factors = c() # empty vector to store prime factors
  i = 2 # starts checking divisibility from the smallest prime number, 2
  
  while(n > 1){
    while (n %% i == 0) { # checks if i is a factor of n
      factors = c(factors, i) # i gets added to vector of prime factors
      n = n/i # to find other factors of n other than the current i
    }
    i = i + 1 # increment to check next potential factor
    if (i * i > n){ # checks if n is prime or has no smaller divisors left
      if (n > 1){ # if n is greater than one after the last conditional it must be prime
        factors = c(factors, n) # add the prime number n to the vector
      }
      break # once all factors are found break loop
    }
  }
  return (factors) # returns all factors of n
}

product.of.unique.prime = function(n){ 
  factors = primeFactor(n) # gets the prime factors of n
  return (length(factors) == length(unique(factors))) # returns TRUE if number of factors equals number of unique factors
}

replacement = function(n){
  # defines the number below and above the "wrong" number
  l = n - 1 
  u = n + 1
  
  while(TRUE){ # runs infinitely until a replacement is found
    if (product.of.unique.prime(l)){ # checks if the lower number is a product of unique primes
      return (l) 
    }
    if (product.of.unique.prime(u)){
      return (u) # checks if the higher number is a product of unique primes
    }
    # expands the search decreasing l and increasing u
    l = l - 1 
    u = u + 1
  }
}

nums = c(6, 10, 14, 15, 21, 
         22, 26, 33, 34, 35,
         38, 39, 46, 51, 55,
         57, 58, 62, 65, 69,
         75, 77, 82, 85, 86,
         87, 91, 93, 94, 95)

for (num in nums) {
  if (!product.of.unique.prime(num)){
    print(num)
    repl = replacement(num)
    print(paste("Replace ", num, "with", repl))
  }
}
